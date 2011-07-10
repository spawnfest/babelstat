-module(babelstat_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    CalculationSup = {babelstat_calculation_sup,
			 {babelstat_calculation_sup, start_link, []},
			 permanent, 5000, supervisor, [babelstat_calculation_sup]},
    {ok, {{one_for_one, 5, 20}, [CalculationSup]}}.

