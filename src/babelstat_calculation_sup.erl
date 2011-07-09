-module(babelstat_calculation_sup).

-behaviour(supervisor).

%% API
-export([start_link/2,
	 add_child/3]).

%% Supervisor callbacks
-export([init/1]).
    
start_link(SeriesToCalculate, MathNotion) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [SeriesToCalculate, MathNotion]).

init([SeriesToCalculate, MathNotion]) ->
    {ok, {{one_for_one, 0, 1}, [{babelstat_calculation_worker, {
				   {babelstat_calculation_worker, start_link, [SeriesToCalculate, MathNotion, self()]},
				   temporary, brutal_kill, worker, [babelstat_calculation_worker]}
				}]}}.

add_child(SeriesToCalculate, MathNotion, SupervisorPid) ->
    supervisor:start_child(SupervisorPid, {babelstat_calculation_worker, {
					     {babelstat_calculation_worker, start_link, [SeriesToCalculate, MathNotion, SupervisorPid]},
					     temporary, brutal_kill, worker, [babelstat_calculation_worker]}}).
