%%%-------------------------------------------------------------------
%%% @author nisbus <>
%%% @copyright (C) 2011, nisbus
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2011 by nisbus <>
%%%-------------------------------------------------------------------
-module(babel_calc).
-include("../include/babelstat.hrl").
%% API
-export([calculate/1,eval/1,query_db/2]).
-export([test_dates/0,test_calculation_parser/0,test_query/0,test_query/3]).
%%%===================================================================
%%% API
%%%===================================================================
eval(Algebra) ->
    {ok,Ts,_} = calc_lexer:string(Algebra),
    calc_parser:parse(Ts).


get_documents(_Params,_Filter) ->
    %%Call db for the view    
    babelstat_utils:to_babelstat_records([]).
  
%%@doc queries the database recursively and returs a babelstat_series
-spec query_db(Params :: list(), Filter :: tuple()) -> #babelstat_series{}.		      
query_db(Params, {_,_,FilterFrequency, From, To}= Filter) ->
	View = get_documents(Params,Filter),
	case View of
	    [Doc] ->
		%%Single document returned, either it is a constant or a calculation
		case Doc#babelstat.constant of
		    true ->
			babelstat_utils:create_constants_series(Params, Filter,Doc#babelstat.value,Doc#babelstat.scale,Doc#babelstat.metric);
		    false ->
			%%it's a calculation
			Calc = Doc#babelstat.calculation,
			{Queries, Algebra} = babelstat_utils:parse_calculation(Calc),
			Series = lists:map(fun(Serie) ->
					  query_db(Serie,Filter)
				  end,Queries),
			calculate(babelstat_utils:replace_tokens_with_values(Series,Algebra))			
		end;
	    [_H|_T] = Docs ->
		{Dates, Values} = lists:foldl(fun(Doc,Acc) ->
						     {Dates, Values} = Acc,
						     {Dates++[Doc#babelstat.date],Values++[Doc#babelstat.value]}
					     end,{[],[]},Docs),
		{FilteredValues, FilteredDates,FilteredDocs} = babelstat_utils:date_adjust(Values,Dates,Docs, FilterFrequency,From, To),
		babelstat_utils:convert_docs_to_series(Params, Filter, {FilteredValues,FilteredDates}, FilteredDocs);
	    _ ->
		{error, no_documents_found}
	end.
			
    

calculate(Series)->
    lists:map(fun(X) ->
		      babel_calc:eval(X)
	      end, Series).
%%%===================================================================
%%% Test functions delete when database is up
%%%===================================================================
test_query() ->
    Params = ["Spawnfest","Teams","Jesus don't want me for a sunBEAM","code","number of lines"],
    Filter = {"unit", 1, daily, "2000-01-01", "2000-01-20"},    
    View = babelstat_utils:to_babelstat_records(document_creator:get_docs()),
    case View of
	[Doc|[]] ->
	    %%Single document returned, either it is a constant or a calculation
	    case Doc#babelstat.constant of
		true ->
		    babelstat_utils:create_constants_series(Params, Filter,Doc#babelstat.value,Doc#babelstat.scale,Doc#babelstat.metric);
		false ->
		    %%it's a calculation
		    Calc = Doc#babelstat.calculation,
		    {Queries, Algebra} = babelstat_utils:parse_calculation(Calc),
		    Series = lists:map(fun(Serie) ->
					       query_db(Serie,Filter)
				       end,Queries),
		    calculate(babelstat_utils:replace_tokens_with_values(Series,Algebra))			
	    end;
	[_H|_T] = Docs ->
	    {Dates,Values} = lists:foldl(fun(Doc, Acc) ->
						 {Dates, Values} = Acc,
						 {Dates++[Doc#babelstat.date],Values++[Doc#babelstat.value]}     
					 end,{[],[]},Docs),
	    babelstat_utils:convert_docs_to_series(Params, Filter, {Values,Dates}, Docs);
	_ ->
	    {error, no_documents_found}
    end.

test_query(Measurement,Scale, Frequency) ->
    Params = ["Spawnfest","Teams","Jesus don't want me for a sunBEAM","code","number of lines"],
    Filter = {"cm", 1, daily, "2000-01-01", "2002-01-01"},    
    View = babelstat_utils:to_babelstat_records(document_creator:get_docs(Measurement, Scale,Frequency)),
    case View of
	[Doc|[]] ->
	    %%Single document returned, either it is a constant or a calculation
	    case Doc#babelstat.constant of
		true ->
		    babelstat_utils:create_constants_series(Params, Filter,Doc#babelstat.value,Doc#babelstat.scale,Doc#babelstat.metric);
		false ->
		    %%it's a calculation
		    Calc = Doc#babelstat.calculation,
		    {Queries, Algebra} = babelstat_utils:parse_calculation(Calc),
		    Series = lists:map(fun(Serie) ->
					       query_db(Serie,Filter)
				       end,Queries),
		    calculate(babelstat_utils:replace_tokens_with_values(Series,Algebra))			
	    end;
	[_H|_T] = Docs ->
	    {Dates,Values} = lists:foldl(fun(Doc, Acc) ->
						 {Dates, Values} = Acc,
						 {Dates++[Doc#babelstat.date],Values++[Doc#babelstat.value]}     
					 end,{[],[]},Docs),
	    babelstat_utils:convert_docs_to_series(Params, Filter, {Values,Dates}, Docs);
	_ ->
	    {error, no_documents_found}
    end.

test_dates() ->
    Docs = document_creator:get_docs(<<"cm">>,1,<<"daily">>),
    Records = babelstat_utils:to_babelstat_records(Docs),
    [{Values, Dates}] = lists:map(fun(X) -> {_,D,V,_,_,_,_,_,_,_,_,_,_,_} = X, {V,D} end,[babel_calc:test_query(<<"feet">>,100,<<"daily">>)]),
    babelstat_utils:date_adjust(Values,Dates,weeks, Records,hd(Dates),lists:nth(length(Dates),Dates)).

test_calculation_parser() ->
    Test = "{Cat,SubC,Subj,Sc,T}+{Cat2,SubC2,Subj2,Sc2,T2}",
    {_,A} = babelstat_utils:parse_calculation(Test),
    babelstat_utils:replace_tokens_with_values(A,[[50.0],[50.0]]).
