-module(babelstat_calculation_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([add_child/3]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    CalculationSpec = {babelstat_calculation,
			  {babelstat_calculation, start_link, []},
			  temporary,
			  brutal_kill,
			  supervisor,
			  [babelstat_calculation]},
    {ok, {{simple_one_for_one, 0, 1},
	  [CalculationSpec]}}.

add_child(Query, Filter, Callback) ->
    supervisor:start_child(?MODULE, [Query, Filter, Callback]).
