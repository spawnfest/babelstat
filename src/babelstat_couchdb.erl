-module(babelstat_couchdb).
-behaviour(gen_babelstat_db).
-include("../include/babelstat.hrl").
-include_lib("couch/include/couch_db.hrl").
-export([query_database/1]).

-define(DB_NAME, "babelstat").
-define(DESIGN_NAME, "babelstat_core").
-define(VIEW_NAME, "babelstat_parameters").

-spec query_database(#babelstat_query{}) ->
			    {ok, Results::[#babelstat{}]} |
			    no_results.
query_database(#babelstat_query{ category = Category,
				 sub_category = SubCategory,
				 subject = Subject,
				 series_category = SeriesCategory,
				 title = Title }) ->

    Options = [{keys, [[Category,
		       SubCategory,
		       Subject,
		       SeriesCategory,
		       Title]]},
	       {query_args, #view_query_args{
		  include_docs = true
		 }}],
    
    {ok, Db} = couchc:open_db(?DB_NAME),

    case couchc:fold(Db, {?DESIGN_NAME, ?VIEW_NAME}, fun get_results/2, Options) of
	{error, {not_found, Reason}} ->
	    error_logger:error_msg("Error querying DB: ~p", [Reason]),
	    no_results;
	{ok, {_, _, []}} ->
	    no_results;
	{ok, {_, _, Results}} ->
	    io:format("Results are ~p~n", [Results]),
	    {ok, Results}
    end.

get_results(Row0, Acc) ->
    {Row} = Row0,
    {Doc} = proplists:get_value(doc, Row),
    Stat = #babelstat{ id = proplists:get_value(<<"_id">>, Doc),
		       rev = proplists:get_value(<<"_rev">>, Doc),
		       constant = proplists:get_value(<<"contstant">>, Doc, false),
		       date = proplists:get_value(<<"date">>, Doc),
		       value = proplists:get_value(<<"value">>, Doc),
		       metric = proplists:get_value(<<"metric">>, Doc),
		       scale = proplists:get_value(<<"scale">>, Doc),
		       frequency = to_atom(proplists:get_value(<<"frequency">>, Doc)),
		       location = to_list(proplists:get_value(<<"location">>, Doc)),
		       category = proplists:get_value(<<"category">>, Doc),
		       sub_category = proplists:get_value(<<"sub_category">>, Doc),
		       subject = proplists:get_value(<<"subject">>, Doc),
		       series_category = proplists:get_value(<<"series_category">>, Doc),
		       title = proplists:get_value(<<"title">>, Doc),
		       calculation = proplists:get_value(<<"calculation">>, Doc, false),
		       source = to_list(proplists:get_value(<<"source">>, Doc))
		     },
    {ok, Acc ++ [Stat]}.

to_atom(Value) when is_binary(Value) ->
    list_to_atom(string:to_lower(binary_to_list(Value))).

to_list([]) ->
    [];
to_list({Proplist}) ->
    Proplist;
to_list(List) ->
    List.
