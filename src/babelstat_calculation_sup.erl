-module(babelstat_calculation_sup).

-behaviour(supervisor).

%% API
-export([start_link/2,
	 add_child/3]).

%% Supervisor callbacks
-export([init/1]).
    
start_link(Query, Filter) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Query, Filter]).

init([Query, Filter]) ->
    {ok, {{one_for_one, 0, 1}, [{babelstat_calculation_worker, {
				   {babelstat_calculation_worker, start_link, [Query, Filter]},
				   temporary, brutal_kill, worker, [babelstat_calculation_worker]}
				}]}}.

add_child(Query, Filter, SupervisorPid) ->
    supervisor:start_child(SupervisorPid, {babelstat_calculation_worker, {
					     {babelstat_calculation_worker, start_link, [Query, Filter, SupervisorPid]},
					     temporary, brutal_kill, worker, [babelstat_calculation_worker]}}).
