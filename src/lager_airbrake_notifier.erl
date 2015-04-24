-module(lager_airbrake_notifier).

%% API
-export([notify/4]).

%% macros
-define(NOTIFIER_NAME, <<"lager_airbrake">>).
-define(NOTIFIER_VERSION, <<"0.1">>).
-define(NOTIFIER_REPO, <<"https://github.com/ostinelli/lager_airbrake">>).

%% records
-record(state, {
    environment = <<>> :: binary(),
    project_id = <<>> :: binary(),
    api_key = <<>> :: binary(),
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
-spec notify(Environment :: binary(), ProjectId :: binary(), ApiKey :: binary(), LogEntry :: any()) -> pid().
notify(Environment, ProjectId, ApiKey, LogEntry) ->
    spawn(fun() ->
        %% get main properties
        Severity = lager_msg:severity(LogEntry),
        Message = lager_msg:message(LogEntry),

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
    check_notify_lager_dropped(State).

-spec check_notify_lager_dropped(#state{}) -> ok.
check_notify_lager_dropped(#state{
    message = Message
} = State) ->
    {ok, Mp} = re:compile("dropped \\d+ messages in the last second that exceeded the limit of"),
    case re:run(Message, Mp) of
        nomatch ->
            extract_file_and_line(State);
        _ ->
            ok %% do not log
    end.

-spec extract_file_and_line(#state{}) -> ok.
extract_file_and_line(#state{
    message = Message,
    file = undefined,
    line = undefined
} = State) ->
    {ok, Mp} = re:compile("\\[{file,\"([^\"]+)\"},{line,(\\d+)}\\]"),
    case re:run(Message, Mp, [{capture, all_but_first, binary}]) of
        nomatch ->
            notify(State);
        {match, [File1, Line1]} ->
            notify(State#state{file = File1, line = binary_to_integer(Line1)})
    end;
extract_file_and_line(State) ->
    notify(State).

-spec notify(#state{}) -> ok.
notify(#state{
    project_id = ProjectId,
    api_key = ApiKey
} = State) ->
    %% get url
    Url = url_for(ProjectId, ApiKey),

    %% build json
    Json = json_for(State),

    io:format("Json: ~p", [Json]),

    %% build request
    Method = post,
    Headers = [
        {<<"Content-Type">>, <<"application/json">>}
    ],
    Options = [],

    %% send
    case hackney:request(Method, Url, Headers, Json, Options) of
        {ok, 201, _RespHeaders, _ClientRef} ->
            ok;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            %% get response body
            {ok, Body} = hackney:body(ClientRef),
            %% log error
            error("Could not send notification to Airbrake", [
                {response_status_code, StatusCode},
                {response_body, binary_to_list(Body)}
            ]);
        Other ->
            %% log error
            error("Could not send notification to Airbrake", [Other])
    end.

-spec url_for(ProjectId :: binary(), ApiKey :: binary()) -> Url :: binary().
url_for(ProjectId, ApiKey) ->
    <<"https://airbrake.io/api/v3/projects/", ProjectId/binary, "/notices?key=", ApiKey/binary>>.

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
                {message, unicode:characters_to_binary(Message)},
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
