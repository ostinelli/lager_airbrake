-module(lager_airbrake_backend).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% records
-record(state, {
    environment = <<>> :: binary(),
    project_id = "" :: string(),
    api_key = "" :: string(),
    level = 0 :: non_neg_integer(),
    ignore = [] :: list()
}).


%% ===================================================================
%% Callbacks
%% ===================================================================

%% ----------------------------------------------------------------------------------------------------------
%% Init
%% ----------------------------------------------------------------------------------------------------------
-spec init([{atom(), any()}]) ->
    {ok, #state{}} |
    {ok, #state{}, hibernate} |
    {error, Reason :: any()}.
init(Options) ->
    %% get options
    Environment = proplists:get_value(environment, Options),
    ApiKey = proplists:get_value(api_key, Options),
    ProjectId = proplists:get_value(project_id, Options),
    Level = proplists:get_value(level, Options),
    %% get ignores & compile regexes
    Ignore = build_ignore_regexes(proplists:get_value(ignore, Options)),
    %% init
    LevelInt = lager_util:level_to_num(Level),
    %% build state
    {ok, #state{
        environment = list_to_binary(Environment),
        project_id = ProjectId,
        api_key = ApiKey,
        level = LevelInt,
        ignore = Ignore
    }}.

%% ----------------------------------------------------------------------------------------------------------
%% Event messages
%% ----------------------------------------------------------------------------------------------------------
-spec handle_event(Event :: any(), #state{}) ->
    {ok, #state{}} |
    {ok, #state{}, hibernate} |
    {swap_handler, Args1 :: any(), #state{}, Module2 :: atom() | {Module2 :: atom(), Id :: any()}, Args2 :: any()} |
    remove_handler.

handle_event({log, LogEntry}, #state{
    environment = Environment,
    project_id = ProjectId,
    api_key = ApiKey,
    level = Level,
    ignore = Ignore
} = State) ->
    case lager_util:is_loggable(LogEntry, Level, ?MODULE) of
        true ->
            lager_airbrake_notifier:notify(Environment, ProjectId, ApiKey, Ignore, LogEntry),
            {ok, State};
        false ->
            {ok, State}
    end;

handle_event(_Event, State) ->
    %% received an unknown event message
    {ok, State}.

%% ----------------------------------------------------------------------------------------------------------
%% Call messages
%% ----------------------------------------------------------------------------------------------------------
-spec handle_call(Request :: any(), #state{}) ->
    {ok, Reply :: any(), #state{}} |
    {ok, Reply :: any(), #state{}, hibernate} |
    {swap_handler, Reply :: any(), Args1 :: any(), #state{}, Module2 :: atom() | {Module2 :: atom(), Id :: any()}, Args2 :: any()} |
    {remove_handler, Reply :: any()}.

handle_call(get_loglevel, #state{level = Level} = State) ->
    {ok, Level, State};

handle_call({set_loglevel, NewLevel}, State) ->
    NewLevelInt = lager_util:level_to_num(NewLevel),
    {ok, ok, State#state{level = NewLevelInt}};

handle_call(_Request, State) ->
    %% received an unknown call message
    {ok, undefined, State}.

%% ----------------------------------------------------------------------------------------------------------
%% All non Event / Call messages
%% ----------------------------------------------------------------------------------------------------------
-spec handle_info(Info :: any(), #state{}) ->
    {ok, #state{}} |
    {ok, #state{}, hibernate} |
    {swap_handler, Args1 :: any(), #state{}, Module2 :: atom() | {Module2 :: atom(), Id :: any()}, Args2 :: any()} |
    remove_handler.

handle_info(_Info, State) ->
    %% received an unknown info message
    {ok, State}.

%% ----------------------------------------------------------------------------------------------------------
%% Terminate
%% ----------------------------------------------------------------------------------------------------------
-spec terminate(Reason :: any(), #state{}) -> terminated.
terminate(_Reason, _State) ->
    terminated.

%% ----------------------------------------------------------------------------------------------------------
%% Convert process state when code is changed.
%% ----------------------------------------------------------------------------------------------------------
-spec code_change(OldVsn :: any(), #state{}, Extra :: any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Internal
%% ===================================================================
build_ignore_regexes(IgnoreStatements) ->
    F = fun({file, IgnoreRegexp}, Acc) ->
        case catch re:compile(IgnoreRegexp) of
            {ok, Mp} ->
                [{file, Mp} | Acc];
            _ ->
                error({invalid_lager_airbrake_ignore_regex, IgnoreRegexp})
        end
    end,
    lists:foldl(F, [], IgnoreStatements).
