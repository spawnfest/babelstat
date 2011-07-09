-module(babelstat_calculation_worker).

-behaviour(gen_fsm).

-include("../include/babelstat.hrl").

%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	 terminate/3, code_change/4]).

%% gen_fsm states
-export([waiting_for_workers/2]).

-define(SERVER, ?MODULE).

-record(state, {
	  callback :: fun(),
	  search_query :: #babelstat_query{},
	  filter :: #babelstat_filter{},
	  workers :: integer(),
	  algebra = undefined :: string() | undefined,
	  result :: any()
	 }).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(Query::[{Key::atom(), Val::binary()}],
		 Filter::[{Key1::atom(), Val::binary()}] | [], ReportToPid::pid()) ->
			{ok, Pid::pid()} | ignore | {error, Error::term()}.
start_link(Query, Filter, Callback) ->
    gen_fsm:start_link(?MODULE, [Query, Filter, Callback], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([SearchQuery, Filter, Callback]) ->
    io:format("up"),
    case babelstat_database:query_database(SearchQuery) of
	{ok, [#babelstat{calculation = Calc} = Result|[]]} ->
	    % Single result
	    case {Result#babelstat.constant =:= true, Result#babelstat.calculation =:= true} of
		{true, _} ->
		    % It's a constant
		    Series = babelstat_utils:create_constants_series(SearchQuery, Filter, Result#babelstat.value,
								      Result#babelstat.scale, Result#babelstat.metric),
		    {stop, done, #state{ result = Series,
					 callback = Callback}};
		{_, true} ->
		    % It's a calculation
		    {Queries, Algebra} = babelstat_utils:parse_calculation(Calc),
		    Self = self(),
		    Workers = length(lists:map(fun(Serie) ->
						{ok, Pid} = babelstat_calculation_worker:start_link(Serie, Filter,
												    fun(Res) ->
													    gen_fsm:send_event(Self, Res)
												    end),
						Pid
					end, Queries)),
		    {next_state, waiting_for_workers, #state{workers = Workers,
							     algebra = Algebra,
							     result = [],
							     callback = Callback}};
		{_, _} ->
		    % This is a single document
		    {stop, done, #state{ result = Result,
					 callback = Callback}}
	    end;
	{ok, Results} ->
	    % done
	    Results,
	    ok;
	_ ->
	    {stop, error, #state{ result = no_document_found,
				  callback = Callback}}
    end.

-spec waiting_for_workers({done, Results::any} |
			  {error, Error::any}, #state{}) ->
				 {stop, done, #state{}} |
				 {next_state, waiting_for_workers, #state{}}.
waiting_for_workers({error, Error},  State) ->
    {stop, error, State#state{ result = Error}};
waiting_for_workers({done, NewResults}, #state{result = Results,
					       workers = 1,
					       algebra = Algebra } = State) ->
    CalculatedResults = babel_calc:calculate(babelstat_utils:replace_tokens_with_values(Results++[NewResults], Algebra)),
    {stop, done, State#state{ result = CalculatedResults,
			      workers = 0 }};
waiting_for_workers({done, NewResult}, #state{ result = Result, 
					       workers = Workers} = State) ->
    {next_state, waiting_for_workers, State#state{ result = Result ++ [NewResult],
						   workers = Workers - 1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(error, _, #state{ result = Error, callback = Callback }) ->
    Callback({error, Error});
terminate(done, _, #state{ result = Result, callback = Callback }) ->
    Callback({done, Result});

terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
