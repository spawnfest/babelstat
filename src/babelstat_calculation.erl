-module(babelstat_calculation).

-behaviour(gen_server).

-export([start_link/3,
	 init/1,
%	 handle_call/3,
%	 handle_cast/2,
	 handle_info/2,
%	 code_change/3,
	 terminate/2]).

-record(state, {
	  worker :: pid()
	 }).

start_link(Query, Filter, Callback) ->
    gen_server:start_link(?MODULE, [Query, Filter, Callback], []).

init([Query, Filter, Callback]) ->
    Res = babelstat_calculation_worker:start_link(Query, Filter, Callback),
    io:format("Res is ~p~n", [Res]),
%    erlang:monitor(process, Pid),
    {ok, #state{}}.
%    {ok, #state{ worker = Pid }}.

handle_info({'EXIT', _Ref, process, Pid, _Reason}, #state{ worker = Pid } = State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.
