-module(lager_airbrake_notifier).

%% API
-export([notify/4]).


%% ===================================================================
%% API
%% ===================================================================
-spec notify(LogEntry :: any(), Environment :: string(), ProjectId :: binary(), ApiKey :: binary()) -> pid().
notify(LogEntry, Environment, ProjectId, ApiKey) ->
    spawn(fun() ->
        %% get main properties
        Severity = lager_msg:severity(LogEntry),
        Message = lager_msg:message(LogEntry),
        %% get metadata
        Metadata = lager_msg:metadata(LogEntry),
        Pid = proplists:get_value(pid, Metadata),
        File = proplists:get_value(file, Metadata),
        Line = proplists:get_value(line, Metadata),
        Module = proplists:get_value(module, Metadata),
        Function = proplists:get_value(function, Metadata),
        Node = node(),
        %% get url
        Url = url_for(ProjectId, ApiKey),
        %% notify
        check_notify_from_self(Url, Severity, Message, Pid, File, Line, Module, Function, Node, Environment)
    end).

%% ===================================================================
%% Internal
%% ===================================================================
-spec check_notify_from_self(
    Url :: binary(),
    Severity :: atom(),
    Message :: list(),
    Pid :: pid(),
    File :: list(),
    Line :: non_neg_integer(),
    Module :: atom(),
    Function :: atom(),
    Node :: atom(),
    Environment :: string()
) -> ok.
check_notify_from_self(_Url, _Severity, _Message, emulator, _File, _Line, _Module, _Function, _Node, _Environment) ->
    ok; %% do not log
check_notify_from_self(Url, Severity, Message, Pid, File, Line, Module, Function, Node, Environment) ->
    check_notify_lager_dropped(Url, Severity, Message, Pid, File, Line, Module, Function, Node, Environment).

-spec check_notify_lager_dropped(
    Url :: binary(),
    Severity :: atom(),
    Message :: list(),
    Pid :: pid(),
    File :: list(),
    Line :: non_neg_integer(),
    Module :: atom(),
    Function :: atom(),
    Node :: atom(),
    Environment :: string()
) -> ok.
check_notify_lager_dropped(Url, Severity, Message, Pid, File, Line, Module, Function, Node, Environment) ->
    {ok, Mp} = re:compile("dropped \\d+ messages in the last second that exceeded the limit of"),
    case re:run(Message, Mp) of
        nomatch ->
            notify(Url, Severity, Message, Pid, File, Line, Module, Function, Node, Environment);
        _ ->
            ok %% do not log
    end.

-spec notify(
    Url :: binary(),
    Severity :: atom(),
    Message :: any(),
    Pid :: pid(),
    File :: any(),
    Line :: non_neg_integer(),
    Module :: atom(),
    Function :: atom(),
    Node :: atom(),
    Environment :: string()
) -> ok.
notify(Url, Severity, Message, Pid, File, Line, Module, Function, Node, Environment) ->
    %% build json
    Json = json_for(Severity, Message, Pid, File, Line, Module, Function, Node, Environment),

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
        {ok, StatusCode, RespHeaders, ClientRef} ->
            %% get response body
            {ok, Body} = hackney:body(ClientRef),
            %% log error
            error("Could not send notification to Airbrake", [
                {request_body, Json},
                {response_status_code, StatusCode},
                {response_headers, RespHeaders},
                {response_body, Body}
            ]);
        Other ->
            %% log error
            error("Could not send notification to Airbrake", [Other])
    end.

-spec url_for(ProjectId :: binary(), ApiKey :: binary()) -> Url :: binary().
url_for(ProjectId, ApiKey) ->
    <<"https://airbrake.io/api/v3/projects/", ProjectId/binary, "/notices?key=", ApiKey/binary>>.

-spec json_for(
    Severity :: atom(),
    Message :: any(),
    Pid :: pid(),
    File :: any(),
    Line :: non_neg_integer(),
    Module :: atom(),
    Function :: atom(),
    Node :: atom(),
    Environment :: string()
) -> Json :: binary().
json_for(Severity, Message, Pid, File, Line, Module, Function, Node, Environment) ->
    JsonTerm = {[
        {notifier,
            {[
                {name, <<"lager_airbrake">>},
                {version, <<"0.1">>},
                {url, <<"https://github.com/ostinelli/lager_airbrake">>}
            ]}
        },

        {errors, [
            {[
                {type, to_binary(lists:concat(["[", Severity, "] ", Module, ":", Function]))},
                {message, to_binary(Message)},
                {backtrace, [
                    {[
                        {file, to_binary(File)},
                        {function, to_binary(Function)},
                        {line, force_integer(Line)}
                    ]}
                ]}
            ]}
        ]},

        {context,
            {[
                {environment, to_binary(Environment)}
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
to_binary(X) -> lists:flatten(io_lib:format("~p", [X])).

-spec force_integer(any()) -> integer().
force_integer(undefined) -> 0;
force_integer(X) -> X.
