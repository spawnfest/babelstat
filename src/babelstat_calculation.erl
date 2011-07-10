-module(babelstat_calculation).

-behaviour(gen_server).

-export([start_link/3,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

-record(state, { worker :: pid(),
		 callback :: fun()
	 }).

start_link(Query, Filter, Callback) ->
    gen_server:start_link(?MODULE, [Query, Filter, Callback], []).

init([Query, Filter, Callback]) ->
    process_flag(trap_exit, true),
    Self = self(),
    Callback1 = fun(Res) ->
			gen_server:cast(Self, {workers_done, Res})
		end,
    {ok, Pid} = babelstat_calculation_worker:start_link(Query, Filter, Callback1),
    {ok, #state{ worker = Pid,
		 callback = Callback}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({workers_done, Results}, #state{ callback = Callback} = State) ->
    Callback(Results),
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
