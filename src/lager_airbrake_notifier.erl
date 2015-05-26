-module(lager_airbrake_notifier).

%% API
-export([notify/6]).

%% macros
-define(NOTIFIER_NAME, <<"lager_airbrake">>).
-define(NOTIFIER_VERSION, <<"0.1">>).
-define(NOTIFIER_REPO, <<"https://github.com/ostinelli/lager_airbrake">>).

%% records
-record(state, {
    environment = <<>> :: binary(),
    project_id = "" :: string(),
    api_key = "" :: string(),
    extract_file_and_line_mp = undefined :: undefined | any(),
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
    ExtractFileAndLineMp :: any(),
    Ignore :: list(),
    LogEntry :: any()
) -> pid().
notify(Environment, ProjectId, ApiKey, ExtractFileAndLineMp, Ignore, LogEntry) ->
    spawn(fun() ->
        %% get main properties
        Severity = lager_msg:severity(LogEntry),
        Message = unicode:characters_to_binary(lager_msg:message(LogEntry)),

        %% get metadata
        Metadata = lager_msg:metadata(LogEntry),
        Pid = proplists:get_value(pid, Metadata),
        File = proplists:get_value(file, Metadata),
        Line = proplists:get_value(line, Metadata),
        Function = proplists:get_value(function, Metadata),
        Node = node(),

        State = #state{
            environment = Environment,
            project_id = ProjectId,
            api_key = ApiKey,
            extract_file_and_line_mp = ExtractFileAndLineMp,
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
-spec check_notify_from_emulator(#state{}) -> ok.
check_notify_from_emulator(#state{
    pid = emulator
}) ->
    ok; %% do not log
check_notify_from_emulator(State) ->
    extract_file_and_line(State).

-spec extract_file_and_line(#state{}) -> ok.
extract_file_and_line(#state{
    extract_file_and_line_mp = ExtractFileAndLineMp,
    message = Message,
    file = undefined,
    line = undefined
} = State) ->
    case re:run(Message, ExtractFileAndLineMp, [{capture, all_but_first, binary}]) of
        nomatch ->
            notify(State);
        {match, [File, Line]} ->
            check_ignore(State#state{file = File, line = binary_to_integer(Line)})
    end;
extract_file_and_line(State) ->
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
    api_key = ApiKey
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
            ok;
        {ok, StatusCode, _RespHeaders, Body} ->
            %% log error
            error("Error sending notification to Airbrake", [
                {response_status_code, StatusCode},
                {response_body, binary_to_list(Body)}
            ]);
        Other ->
            %% log error
            error("Could not send notification to Airbrake", [Other])
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
    LineBin = to_binary(Line),

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
                {type, <<"[", SeverityBin/binary, "] ", FileBin/binary, ":", LineBin/binary>>},
                {message, Message},
                {backtrace, [
                    {[
                        {file, FileBin},
                        {function, to_binary(Function)},
                        {line, Line}
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
