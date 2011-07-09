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
-export([eval/1,query_db/2]).
-export([test_calculation_parser/0,replace_token_with_value/3]).

%%%===================================================================
%%% API
%%%===================================================================
eval(Algebra) ->
    {ok,Ts,_} = calc_lexer:string(Algebra),
    calc_parser:parse(Ts).

transpose([]) -> [];
transpose([Single,[]]) -> Single;
transpose([H|_]=L) -> [lists:map(F, L) || F <- [fun(A) -> lists:nth(N, A) end || N <- lists:seq(1, length(H))]].

get_documents(_Params,_Filter) ->
    %%Call db for the view    
    [].


%%@doc queries the database recursively and returs a babelstat_series
-spec query_db(Params :: list(), Filter :: tuple()) -> #babelstat_series{}.		      
query_db(Params, Filter) ->
	View = get_documents(Params,Filter),
	case View of
	    [Doc] ->
		%%Single document returned, either it is a constant or a calculation
		case Doc#babelstat.constant of
		    true ->
			create_constants_series(Params, Filter,Doc#babelstat.value,Doc#babelstat.scale,Doc#babelstat.metric);
		    false ->
			%%it's a calculation
			Calc = Doc#babelstat.calculation,
			{Queries, Algebra} = parse_calculation(Calc),
			Series = lists:map(fun(Serie) ->
					  query_db(Serie,Filter)
				  end,Queries),
			calculate(replace_tokens_with_values(Series,Algebra))			
		end;
	    [_H|_T] = Docs ->
		{Values,Dates} = lists:foldl(fun(Doc) ->
				  Value = proplists:get_value(<<"value">>,Doc,0.0),
				  Date = proplists:get_value(<<"date">>,Doc,0.0),
				  {Date,Value}
			  end,{[],[]},Docs),
		convert_docs_to_series(Params, Filter, {Values,Dates}, Docs);
	    _ ->
		{error, no_documents_found}
	end.
			
	


convert_docs_to_series([Category, Sub_Category, Subject, Series_Category, Title] = Params, {Metric, Scale, Frequency, _, _} = Filter, {Values, Dates}, Docs) ->
    ConvertedValues = lists:foldl(fun(Doc, Acc) ->
					  DocScale = proplists:get_value(<<"scale">>,Doc,1),
					  DocMetric = proplists:get_value(<<"metric">>,Doc,undefined),
					  Value = lists:seq(length(Acc)+1,Values),
					  NewValue = convert_scale(DocScale, Scale, Value),
					  convert_metric(DocMetric, Metric,NewValue)      
		       end,[],Docs),

    #babelstat_series{dates = Dates, values = ConvertedValues, metric = Metric, scale = Scale, 
		      frequency = Frequency, category = Category, sub_category = Sub_Category, 
		      subject = Subject, series_category = Series_Category, title = Title, 
		      legend = create_legend(Params,Filter)}.
    

create_constants_series([Category, Sub_Category, Subject, Series_Category, Title] = Params, {Metric, Scale, Frequency, From, To} = Filter, Value, DocScale,DocMetric) ->
    DateList = dates:create_range(From,To, list_to_atom(Frequency)),
    ConstantSeries = #babelstat_series{dates = DateList, metric = Metric, scale = Scale, frequency = Frequency,
				      category = Category, sub_category = Sub_Category, subject = Subject,
				      series_category = Series_Category, title = Title, 
				      legend = create_legend(Params,Filter)},
    Values = lists:map(fun(_Date) ->
			       NewValue = convert_scale(DocScale, Scale, Value),
			       convert_metric(DocMetric, Metric,NewValue)      
		       end,DateList),
    ConstantSeries#babelstat_series{values = Values}.

-spec create_legend(list(),tuple()) -> string().
create_legend([Category, Sub_Category, Subject, Series_Category, Title], {Metric, _, _, _, _}) ->
    Sep = " - ",
    lists:append([Category, Sep, Sub_Category, Sep, Subject, Sep, Series_Category, Sep, Title, " (",Metric,")"]).


-spec convert_metric(float(),float(), float()) -> float().
convert_metric(OriginalMetric,NewMetric,Value) ->
    measurements:convert(OriginalMetric, NewMetric, Value).

-spec convert_scale(float(),float(), float()) -> float().
convert_scale(OriginalScale, NewScale, Value) ->
    case OriginalScale =:= NewScale of
	true ->
	    Value;
	false ->
	    case OriginalScale < NewScale of
		true ->
		    Value*(OriginalScale/NewScale);
		false ->
		    Value*(NewScale/OriginalScale)
	    end
    end.

-spec parse_calculation(string()) -> {list(),string()}.		       
parse_calculation(Calculation) ->
    Tokens = string:tokens(Calculation,"()+-/*^"),
    PrettyAlgebra = simplify_algebra(Tokens,Calculation),
    Queries = lists:map(fun(Token) ->
				Items = string:tokens(Token,"{,}"),
				{C,SuC,Subj,Sc,T} = list_to_tuple(Items),
				[C,SuC,Subj,Sc,T]
			end,Tokens),
    {Queries, PrettyAlgebra}.

test_calculation_parser() ->
    Test = "{Cat,SubC,Subj,Sc,T}+{Cat2,SubC2,Subj2,Sc2,T2}",
    {_,A} = parse_calculation(Test),
    replace_tokens_with_values(A,[[50.0],[50.0]]).
    
-spec replace(string(),string(), string()) -> string().
replace(Original, ToReplace, ReplaceWith) ->
    Index = string:str(Original,ToReplace),
    Len = length(ToReplace),
    LeftSide = string:substr(Original,1,Index-1),
    RightSide = string:substr(Original,Index+Len),
    lists:append([LeftSide,ReplaceWith,RightSide]).

calculate(Series)->
    lists:map(fun(X) ->
		      babel_calc:eval(X)
	      end, Series).

-spec replace_tokens_with_values(string(), [list()]) -> [string()].					
replace_tokens_with_values(Algebra,List) ->
    Tokens = string:tokens(Algebra,"()+-/*^"),
    Transposed = transpose(List),    
    R = lists:map(fun(X) ->
			  lists:foldl(fun(Y,Acc) ->
					      {A,Counter} = Acc,
					      Token = lists:nth(Counter,Tokens),
					      Replaced = replace_token_with_value(A,Token,[Y]),
					      {Replaced,Counter+1}
				      end,{Algebra,1},X)			
		  end,Transposed),
    [Result || {Result,_} <- R].

-spec replace_token_with_value(string(), string(), number()) -> string().				       
replace_token_with_value(Original, ToReplace, ReplaceWith) ->
    R = case ReplaceWith of
	X when is_float(X) ->
		X;
	[Y] ->
	        Y*1.0
	end,
    Index = string:str(Original,ToReplace),
    Len = length(ToReplace),
    LeftSide = string:substr(Original,1,Index-1),
    RightSide = string:substr(Original,Index+Len),
    [Float] = io_lib:format("~.6f",[R]),
    LeftSide++Float++RightSide.

-spec simplify_algebra(string(),string()) -> string().			  
simplify_algebra(Tokens,Calculation) ->
    TokenCount = length(Tokens),    
    case TokenCount > 26 of
	true ->
	    erlang:error("this version of BabelStat only supports calculations of up to 26 variables (UPPERCASE ASCII)");
	false ->
	    lists:foldl(fun(N,Acc) ->
				Char = integer_to_list(64+N),
				Token = lists:nth(N,Tokens),
				replace(Acc,Token,Char)
			end,Calculation,lists:seq(1,TokenCount))
    end.
