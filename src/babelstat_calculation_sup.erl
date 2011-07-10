-module(babelstat_calculation_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).
    
start_link(Query, Filter, Callback) ->
    supervisor:start_link(?MODULE, [Query, Filter, Callback]).

init([Query, Filter, Callback]) ->
    WorkerSpec = {babelstat_calculation_worker,
		    {babelstat_calculation_worker, start_link, [Query, Filter, Callback]},
		    temporary, brutal_kill, worker, [babelstat_calculation_worker]},
    {ok, {{one_for_one, 0, 1}, [WorkerSpec]}}.
