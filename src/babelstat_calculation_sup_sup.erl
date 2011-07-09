-module(babelstat_calculation_sup_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([add_child/3]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    CalculationSupSpec = {babelstat_calculation_sup,
			  {babelstat_calculation_sup, start_link, []},
			  temporary,
			  brutal_kill,
			  supervisor,
			  [babelstat_calculation_sup]},
    {ok, {{simple_one_for_one, 0, 1},
	  [CalculationSupSpec]}}.

add_child(Query, Filter, Callback) ->
    supervisor:start_child(?MODULE, [Query, Filter, Callback]).
