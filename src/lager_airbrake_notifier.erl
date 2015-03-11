-module(lager_airbrake_notifier).

%% API
-export([notify/4]).


%% ===================================================================
%% API
%% ===================================================================
-spec notify(LogEntry :: any(), Environment :: string(), ProjectId :: string(), ApiKey :: string()) -> ok.
notify(LogEntry, Environment, ProjectId, ApiKey) ->

    spawn(fun() ->
        %% get main properties
        Severity = lager_msg:severity(LogEntry),
        Message = lists:flatten(lager_msg:message(LogEntry)),
        %% get metadata
        Metadata = lager_msg:metadata(LogEntry),

        io:format("Metadata IS: ~p", [Metadata]),


        Pid = proplists:get_value(pid, Metadata),
        File = proplists:get_value(file, Metadata),
        Line = proplists:get_value(line, Metadata),
        Module = proplists:get_value(module, Metadata),
        Function = proplists:get_value(function, Metadata),
        Node = node(),
        %% get url
        Url = url_for(ProjectId, ApiKey),
        %% notify
        check_notify(Url, Severity, Message, Pid, File, Line, Module, Function, Node, Environment)
    end).


%% ===================================================================
%% Internal
%% ===================================================================
check_notify(Url, Severity, Message, Pid, File, Line, Module, Function, Node, Environment) ->
    {ok, Mp} = re:compile("dropped \\d+ messages in the last second that exceeded the limit of"),
    case re:run(Message, Mp) of
        nomatch ->
            notify(Url, Severity, Message, Pid, File, Line, Module, Function, Node, Environment);
        _ ->
            %% do not log
            ok
    end.

notify(Url, Severity, Message, Pid, File, Line, Module, Function, Node, Environment) ->

    %% build json
    Json = json_for(Severity, Message, Pid, File, Line, Module, Function, Node, Environment),
    io:format("JSON IS: ~p", [Json]),

    %% build request
    Method = post,
    Headers = [
        {<<"Content-Type">>, <<"application/json">>}
    ],
    Options = [],
    %% send

    case hackney:request(Method, Url, Headers, Json, Options) of
        {ok, 201, _RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            io:format("BODY: ~p", [Body]),

            ok;
        _ ->
            ok
    end.

url_for(ProjectId, ApiKey) ->
    lists:concat(["https://airbrake.io/api/v3/projects/", ProjectId, "/notices?key=", ApiKey]).

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

to_binary(X) when is_pid(X) -> list_to_binary(pid_to_list(X));
to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) -> list_to_binary(X).

force_integer(undefined) -> 0;
force_integer(X) -> X.
