-module(lager_airbrake_notifier).

%% API
-export([notify/8]).

%% macros
-define(NOTIFIER_NAME, <<"lager_airbrake">>).
-define(NOTIFIER_VERSION, <<"0.3.0">>).
-define(NOTIFIER_REPO, <<"https://github.com/ostinelli/lager_airbrake">>).

%% records
-record(state, {
    environment = <<>> :: binary(),
    project_id = "" :: string(),
    api_key = "" :: string(),
    from_self_mp = undefined :: undefined | any(),
    extract_file_and_line_mp = undefined :: undefined | any(),
    extract_file_and_function_mp = undefined :: undefined | any(),
    ignore = [] :: list(),
    severity = undefined :: atom(),
    message = undefined :: any(),
    pid = undefined :: pid() | atom(),
    file = undefined :: binary() | list() | undefined,
    line = undefined :: non_neg_integer() | undefined,
    function = undefined :: atom(),
    node = undefined :: atom()
}).


%% ===================================================================
%% API
%% ===================================================================
-spec notify(
    Environment :: binary(),
    ProjectId :: string(),
    ApiKey :: string(),
    FromSelfMp :: any(),
    ExtractFileAndLineMp :: any(),
    ExtractFileAndFunctionMp :: any(),
    Ignore :: list(),
    LogEntry :: any()
) -> pid().
notify(Environment, ProjectId, ApiKey, FromSelfMp, ExtractFileAndLineMp, ExtractFileAndFunctionMp, Ignore, LogEntry) ->
    spawn(fun() ->
        %% get main properties
        Severity = lager_msg:severity(LogEntry),
        Message = unicode:characters_to_binary(lager_msg:message(LogEntry)),

        %% get metadata
        Metadata = lager_msg:metadata(LogEntry),
        Pid = proplists:get_value(pid, Metadata),
        File = get_file_or_module_info(Metadata),
        Line = proplists:get_value(line, Metadata),
        Function = proplists:get_value(function, Metadata),
        Node = node(),

        State = #state{
            environment = Environment,
            project_id = ProjectId,
            api_key = ApiKey,
            from_self_mp = FromSelfMp,
            extract_file_and_line_mp = ExtractFileAndLineMp,
            extract_file_and_function_mp = ExtractFileAndFunctionMp,
            ignore = Ignore,
            severity = Severity,
            message = Message,
            pid = Pid,
            file = File,
            line = Line,
            function = Function,
            node = Node
        },

        check_notify_from_emulator(State)
    end).

%% ===================================================================
%% Internal
%% ===================================================================
-spec get_file_or_module_info(Metadata :: any()) -> binary() | undefined.
get_file_or_module_info(Metadata) ->
    case proplists:get_value(file, Metadata) of
        undefined -> get_module_info(Metadata);
        Value -> to_binary(Value)
    end.

-spec get_module_info(Metadata :: any()) -> binary() | undefined.
get_module_info(Metadata) ->
    case proplists:get_value(module, Metadata) of
        undefined -> undefined;
        Value -> to_binary(Value)
    end.

-spec check_notify_from_emulator(#state{}) -> ok.
check_notify_from_emulator(#state{
    pid = emulator
}) ->
    ok; %% do not log
check_notify_from_emulator(State) ->
    check_nofify_from_self(State).

-spec check_nofify_from_self(#state{}) -> ok.
check_nofify_from_self(#state{
    from_self_mp = FromSelfMp,
    message = Message
} = State) ->
    case re:run(Message, FromSelfMp, [{capture, none}]) of
        match ->
            ok; %% do not log
        _ ->
            extract_file_and_line(State)
    end.

-spec extract_file_and_line(#state{}) -> ok.
extract_file_and_line(#state{
    extract_file_and_line_mp = ExtractFileAndLineMp,
    message = Message,
    file = undefined,
    line = undefined
} = State) ->
    case re:run(Message, ExtractFileAndLineMp, [{capture, all_but_first, binary}]) of
        nomatch ->
            extract_file_and_function(State);
        {match, [File, Line]} ->
            extract_file_and_function(State#state{file = File, line = binary_to_integer(Line)})
    end;
extract_file_and_line(State) ->
    extract_file_and_function(State).

-spec extract_file_and_function(#state{}) -> ok.
extract_file_and_function(#state{
    extract_file_and_function_mp = ExtractFileAndFunctionMp,
    message = Message,
    file = undefined,
    function = Function
} = State) ->
    case re:run(Message, ExtractFileAndFunctionMp, [{capture, all_but_first, binary}]) of
        nomatch ->
            check_ignore(State);
        {match, [File, FunctionMatch]} when Function =:= undefined ->
            check_ignore(State#state{file = File, function = FunctionMatch});
        {match, [File, _]} ->
            check_ignore(State#state{file = File})
    end;
extract_file_and_function(State) ->
    check_ignore(State).

-spec check_ignore(#state{}) -> ok.
check_ignore(#state{
    ignore = []
} = State) ->
    notify(State);
check_ignore(#state{
    ignore = [{file, _IgnoreMp} | TIgnore],
    file = undefined
} = State) ->
    check_ignore(State#state{ignore = TIgnore});
check_ignore(#state{
    ignore = [{file, IgnoreMp} | TIgnore],
    file = File
} = State) ->
    case re:run(File, IgnoreMp, [{capture, none}]) of
        nomatch ->
            check_ignore(State#state{ignore = TIgnore});
        match ->
            ok %% do not log
    end;
check_ignore(#state{
    ignore = [{message, IgnoreMp} | TIgnore],
    message = Message
} = State) ->
    case re:run(Message, IgnoreMp, [{capture, none}]) of
        nomatch ->
            check_ignore(State#state{ignore = TIgnore});
        match ->
            ok %% do not log
    end.

-spec notify(#state{}) -> ok.
notify(#state{
    project_id = ProjectId,
    api_key = ApiKey,
    severity = Severity,
    file = File,
    line = Line
} = State) ->
    %% get url
    Url = url_for(ProjectId, ApiKey),

    %% build json
    Json = json_for(State),

    %% build request
    Method = post,
    Headers = [
        {<<"Content-Type">>, <<"application/json">>}
    ],
    Options = [
        {is_ssl, true},
        {ssl_options, []}
    ],

    %% send
    case ibrowse:send_req(Url, Headers, Method, Json, Options) of
        {ok, "201", _RespHeaders, _Body} ->
            error_logger:info_msg("Sent notification to aibrak for error: ~p", [{Severity, File, Line}]);
        {ok, StatusCode, _RespHeaders, Body} ->
            %% log error
            error_logger:error_msg(
                "[LAGER_AIRBRAKE] Error sending notification: response status code: ~p, response body: ~p", [
                    StatusCode, Body
                ]);
        Other ->
            %% log error
            error_logger:error_msg("[LAGER_AIRBRAKE] Could not send notification to Airbrake: ~p", [Other])
    end.

-spec url_for(ProjectId :: string(), ApiKey :: string()) -> Url :: string().
url_for(ProjectId, ApiKey) ->
    lists:concat(["https://airbrake.io/api/v3/projects/", ProjectId, "/notices?key=", ApiKey]).

-spec json_for(#state{}) -> Json :: binary().
json_for(#state{
    environment = Environment,
    severity = Severity,
    message = Message,
    pid = Pid,
    file = File,
    line = Line,
    function = Function,
    node = Node
}) ->
    SeverityBin = to_binary(Severity),
    FileBin = to_binary(File),

    LineInt = case Line of
        undefined -> 0;
        _ -> Line
    end,
    MessageBin = iolist_to_binary([<<"[">>, SeverityBin, <<"] ">>, FileBin, <<":">>, to_binary(LineInt)]),

    JsonTerm = {[
        {notifier,
            {[
                {name, ?NOTIFIER_NAME},
                {version, ?NOTIFIER_VERSION},
                {url, ?NOTIFIER_REPO}
            ]}
        },

        {errors, [
            {[
                {type, MessageBin},
                {message, Message},
                {backtrace, [
                    {[
                        {file, FileBin},
                        {function, to_binary(Function)},
                        {line, LineInt}
                    ]}
                ]}
            ]}
        ]},

        {context,
            {[
                {environment, Environment}
            ]}
        },

        {environment,
            {[
                {node, to_binary(Node)},
                {pid, to_binary(Pid)}
            ]}
        }
    ]},
    jiffy:encode(JsonTerm).

-spec to_binary(any()) -> binary().
to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_integer(X) -> integer_to_binary(X);
to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
to_binary(X) when is_pid(X) -> list_to_binary(pid_to_list(X)).
