-module(gen_babelstat_db).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{query_database, 2},
     {save_document, 1}];
behaviour_info(_) ->
    undefined.
