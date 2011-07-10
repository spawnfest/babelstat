%%%-------------------------------------------------------------------
%%% @author nisbus <>
%%% @copyright (C) 2011, nisbus
%%% @doc
%%%
%%% @end
%%% Created :  9 Jul 2011 by nisbus <>
%%%-------------------------------------------------------------------
-module(babelstat_utils).
-include("../include/babelstat.hrl").
%% API
-export([transpose/1, date_adjust/6,convert_metric/3, convert_scale/3,convert_docs_to_series/4,create_constants_series/5]).
-export([replace_token_with_value/3,to_babelstat_records/1,parse_calculation/1,replace_tokens_with_values/2]).


%%%===================================================================
%%% API
%%%===================================================================
%%@doc Transposes a list of lists.
-spec transpose([list()]) -> [list()].
transpose([]) -> [];
transpose([Single,[]]) -> Single;
transpose([H|_]=L) -> [lists:map(F, L) || F <- [fun(A) -> lists:nth(N, A) end || N <- lists:seq(1, length(H))]].

%%@doc Aggregates documents to a common timeframe (Needs optimizing)
-spec date_adjust(list(), list(), atom(), list(), string(), string()) -> list().
date_adjust(Values, Dates, Frequency, Docs, StartDate, EndDate) ->	
    case lists:any(fun(D) -> D#babelstat.frequency =/= Frequency end,Docs) of
    	true ->	    
    	    ParsedStart = parse_date(list_to_binary(StartDate)),
    	    ParsedEnd = parse_date(list_to_binary(EndDate)),
    	    %%Dates need to be filtered (aggregated to the frequency)
    	    ValidDates = date_range:create_range(ParsedStart,ParsedEnd, Frequency),
    	    Zipped = lists:zip3(Values,Dates,Docs),
	    {_,MatchedList} = lists:foldl(fun(N,Acc) ->
					   {Counter, List} = Acc,
					   {Match,_Dont} = lists:partition(fun(P) ->
									      {_,D,_} = P,
									      Range = lists:sublist(ValidDates,N,N+1),
									      Dt = parse_date(list_to_binary(D)),
									      is_date_in_range(Range, Dt)
								      end,lists:sublist(Zipped,Counter+1,length(Zipped))),
						  case length(Match) of 
						      0-> Acc;
						      _ ->
							  {Counter + length(Match),[aggregate_docs(Match,Frequency)|List]}
						  end
				   end,{0,[]},lists:seq(1,length(ValidDates))),
	    MatchedList;
	_ ->
	    {Values,Dates,Docs}
    end.

		    
convert_docs_to_series(#babelstat_query{ category = Category,
					 sub_category = Sub_Category,
					 subject = Subject,
					 series_category = Series_Category,
					 title = Title } = Params, {Metric, Scale, Frequency, _, _} = Filter, {Values, Dates}, Docs) ->
    {ConvertedValues,_} = lists:foldl(fun(Doc, Acc) ->
					      {NewValues, Counter} = Acc,
					      DocScale = Doc#babelstat.scale,
					      DocMetric = Doc#babelstat.metric,
					      Value = lists:nth(Counter+1,Values),      
					      NewValue =  convert_scale(DocScale, Scale, Value),
					      Converted = convert_metric(DocMetric, Metric,NewValue),
					      {NewValues++[Converted],Counter+1} 
		       end,{[],0},Docs),
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

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

aggregate_docs(DocList,NewFreq) ->
    lists:foldl(fun(O,Acc) ->
			{NoV, _,_} = Acc,
			{V,D,Doc} = O,
			{V+NoV, D,Doc#babelstat{frequency = NewFreq}}
		end,{0.0,undefined,undefined},DocList).

-spec create_legend(list(),tuple()) -> string().
create_legend(#babelstat_query{ category = Category,
				sub_category = Sub_Category,
				subject = Subject,
				series_category = Series_Category,
				title = Title }, {Metric, _, _, _, _}) ->
    Sep = " - ",
    "".
    %lists:append([Category, Sep, Sub_Category, Sep, Subject, Sep, Series_Category, Sep, Title, " (",Metric,")"]).

%%%===================================================================
%%% Date helper functions
%%%===================================================================
is_date_in_range(Range, Date)->
    case length(Range) of 
	0 ->
	    false;
	1 ->
	    Date =< hd(Range);	    
	2 ->
	    From = hd(Range),
	    To = hd(lists:reverse(Range)),
	    case {{Date >= From},{Date =< To}} of
		{{true},{true}} ->
		    true;
		{_,_} ->
		    false
	    end;
	_ ->
	    false
    end.
	    
parse_date(<<Y:4/binary,"-",M:1/binary,"-",D:1/binary>>) ->
    {_,{H,Min,Sec}} = erlang:localtime(),
    {
      {list_to_integer(binary_to_list(Y)),
       list_to_integer(binary_to_list(M)),
       list_to_integer(binary_to_list(D))},
       {H,Min,Sec}
      };

parse_date(<<Y:4/binary,"-",M:1/binary,"-",D:2/binary>>) ->
    {_,{H,Min,Sec}} = erlang:localtime(),
      {{list_to_integer(binary_to_list(Y)),
      list_to_integer(binary_to_list(M)),
       list_to_integer(binary_to_list(D))},
       {H,Min,Sec}};

parse_date(<<Y:4/binary,"-",M:2/binary,"-",D:2/binary>>) ->
    {_,{H,Min,Sec}} = erlang:localtime(),
      {{list_to_integer(binary_to_list(Y)),
       list_to_integer(binary_to_list(M)),
       list_to_integer(binary_to_list(D))},
       {H,Min,Sec}};
parse_date(<<Y:4/binary,"-",M:2/binary,"-",D:2/binary," ",H:2/binary,":",Min:2/binary,":",Sec:2/binary>>) ->
      {{list_to_integer(binary_to_list(Y)),
       list_to_integer(binary_to_list(M)),
	list_to_integer(binary_to_list(D))},
       {list_to_integer(binary_to_list(H)),
	list_to_integer(binary_to_list(Min)),
	list_to_integer(binary_to_list(Sec))}}.


%%%===================================================================
%%% Metric and scale helper functions
%%%===================================================================
-spec convert_metric(float(),float(), float()) -> float().
convert_metric(OriginalMetric, NewMetric, Value) ->
    measurements:convert(binary_to_list(OriginalMetric), binary_to_list(NewMetric), Value).

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

%%%===================================================================
%%% Metric and scale helper functions
%%%===================================================================
-spec parse_calculation(string()) -> {list(),string()}.		       
parse_calculation(Calculation1) ->
    Calculation = binary_to_list(Calculation1),
    Tokens = string:tokens(Calculation,"()+-/*^"),
    PrettyAlgebra = simplify_algebra(Tokens,Calculation),
    Queries = lists:map(fun(Token) ->
				Items = string:tokens(Token,"{,}"),
				{C,SuC,Subj,Sc,T} = list_to_tuple(Items),
				#babelstat_query{ category = list_to_binary(C),
						  sub_category = list_to_binary(SuC),
						  subject = list_to_binary(Subj),
						  series_category = list_to_binary(Sc),
						  title = list_to_binary(T)
						}
			end,Tokens),
    {Queries, PrettyAlgebra}.
    
-spec replace(string(),string(), string()) -> string().
replace(Original, ToReplace, ReplaceWith) ->
    Index = string:str(Original,ToReplace),
    Len = length(ToReplace),
    LeftSide = string:substr(Original,1,Index-1),
    RightSide = string:substr(Original,Index+Len),
    lists:append([LeftSide,ReplaceWith,RightSide]).


-spec replace_tokens_with_values(string(), [list()]) -> [string()].					
replace_tokens_with_values(Algebra,List) ->
    Tokens = string:tokens(Algebra,"()+-/*^"),
    Lists1 = lists:map(fun(#babelstat_series{ values = Values}) ->
			       Values
		       end, List),
    Transposed = transpose(Lists1),    
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

%%%===================================================================
%%% Record conversion helper functions
%%%===================================================================
to_babelstat_records(Docs) ->
    lists:map(fun(X) ->
		      to_babelstat_record(X)
	      end,Docs).

to_babelstat_record({[{<<"_id">>,Id},{<<"_rev">>,Rev},{<<"date">>,Date},{<<"value">>, Value},
		     {<<"metric">>, Metric}, {<<"scale">>, Scale}, {<<"frequency">>,Frequency},
		     {<<"location">>,Location}, {<<"category">>,Cat}, {<<"sub_category">>,SubCat},
		     {<<"subject">>,Subject}, {<<"series_category">>,SerCat}, {<<"title">>,Title},
		     {<<"source">>,Source}, {<<"calculation">>,Calculation}, {<<"constant">>,Constant}]}) ->
    #babelstat{id = Id, rev = Rev, date = binary_to_list(Date), value = Value, 
	       metric = binary_to_list(Metric), scale = Scale, 
	       frequency = binary_to_atom(Frequency,latin1),
	       location = binary_to_list(Location), category = binary_to_list(Cat), 
	       sub_category = binary_to_list(SubCat), subject = binary_to_list(Subject), 
	       series_category = binary_to_list(SerCat), title = binary_to_list(Title), source = Source,
	       calculation = Calculation, constant = Constant}.
