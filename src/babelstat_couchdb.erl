-module(babelstat_couchdb).
-behaviour(gen_babelstat_db).
-include("../include/babelstat.hrl").
-include_lib("couch/include/couch_db.hrl").
-export([query_database/2,
	 save_document/1]).

-define(DB_NAME, "babelstat").
-define(DESIGN_NAME, "babelstat_core").
-define(VIEW_NAME, "babelstat_parameters").

-spec query_database(#babelstat_query{},
		     #babelstat_filter{}) ->
			    {ok, Results::[#babelstat{}]} |
			    no_results.
query_database(#babelstat_query{ category = Category,
				 sub_category = SubCategory,
				 subject = Subject,
				 series_category = SeriesCategory,
				 title = Title },
	       #babelstat_filter{} = Filter) ->
    Key = [Category,
	   SubCategory,
	   Subject,
	   SeriesCategory,
	   Title],
    query_database(Key, Filter);
query_database(Key, #babelstat_filter{ from_date = undefined,
					to_date = undefined }) ->
    StartKey = Key ++ [<<"0001-01-01 00:00:00Z">>],
    EndKey = Key ++ [<<"9999-01-01 00:00:00Z">>],
    query_database(StartKey, EndKey);
query_database(Key, #babelstat_filter{ from_date = FromDate,
				       to_date = ToDate }) when from_date =/= undefined;
								to_date =/= undefined ->
    StartKey = Key ++ [to_iso(FromDate)],
    EndKey = Key ++ [to_iso(ToDate)],
    query_database(StartKey, EndKey);

query_database(StartKey, EndKey) ->
    Options = [{query_args, #view_query_args{
		  start_key = StartKey,
		  end_key = EndKey,
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
	    {ok, Results}
    end.

-spec save_document(#babelstat{} | db_result()) ->
		      ok | error.
save_document(#babelstat {} = Babelstat) ->
    save_document(babelstat_to_document(Babelstat#babelstat{created_date = erlang:localtime_to_universaltime(erlang:localtime())}));
save_document(Doc) ->
    {ok, Db} = couchc:open_db(?DB_NAME),
    case couchc:save_doc(Db, Doc) of
	{ok, _} ->
	    ok;
	_ ->
	    error
    end.

%% Internal
get_results(Row0, Acc) ->
    {Row} = Row0,
    {Doc} = proplists:get_value(doc, Row),

    Stat = document_to_babelstat(Doc),
    {ok, Acc ++ [Stat]}.

babelstat_to_document(#babelstat { constant = Constant,
				   date = Date,
				   value = Value,
				   metric = Metric,
				   scale = Scale,
				   frequency = Frequency,
				   location = Location,
				   category = Category,
				   sub_category = SubCategory,
				   subject = Subject,
				   series_category = SeriesCategory,
				   title = Title,
				   calculation = Calculation,
				   source = Source,
				   created_date = CreateDate
				 }) ->
    {[{<<"type">>, <<"babelstat">>},
      {<<"constant">>, Constant},
      {<<"date">>, to_iso(Date)},
      {<<"value">>, Value},
      {<<"metric">>, Metric},
      {<<"scale">>, Scale},
      {<<"frequency">>, Frequency},
      {<<"location">>, Location},
      {<<"category">>, Category},
      {<<"sub_category">>, SubCategory},
      {<<"subject">>, Subject},
      {<<"series_category">>, SeriesCategory},
      {<<"title">>, Title},
      {<<"calculation">>, check_undefined(Calculation)},
      {<<"source">>, Source},
      {<<"created_date">>, to_iso(CreateDate)}]}.

document_to_babelstat(Doc) ->
    #babelstat{ id = proplists:get_value(<<"_id">>, Doc),
		rev = proplists:get_value(<<"_rev">>, Doc),
		constant = proplists:get_value(<<"contstant">>, Doc, false),
		date = to_date(proplists:get_value(<<"date">>, Doc)),
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
		source = to_list(proplists:get_value(<<"source">>, Doc)),
		created_date = to_date(proplists:get_value(<<"created_date">>, Doc))
	      }.

%2011-07-10 08:59:10Z
to_date(<<Year:4/binary,_:1/binary,Month:2/binary,_:1/binary,Day:2/binary>>) ->
    {{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}, {0,0,0}};

to_date(<<Year:4/binary,_:1/binary,Month:2/binary,_:1/binary,Day:2/binary,_:1/binary,Hour:2/binary,_:1/binary,Minute:2/binary, _:1/binary, Second:2/binary ,_/binary>>) ->
    {{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}, {binary_to_integer(Hour),binary_to_integer(Minute),binary_to_integer(Second)}}.

binary_to_integer(B) ->
    list_to_integer(binary_to_list(B)).

to_iso({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    Year0 = create_date_part(Year),
    Month0 = create_date_part(Month),
    Day0 = create_date_part(Day),
    Hour0 = create_date_part(Hour),
    Minute0 = create_date_part(Minute),
    Second0 = create_date_part(Second),
    <<Year0/binary, "-", Month0/binary, "-", Day0/binary, " ", Hour0/binary, ":", Minute0/binary, ":", Second0/binary, "Z">>.

create_date_part(Date) when is_integer(Date) ->
    create_date_part(integer_to_list(Date));
create_date_part(Date) when length(Date) =:= 4;
			    length(Date) =:= 2 ->
    list_to_binary(Date);
create_date_part(Date) when length(Date) =:= 1 ->
    list_to_binary(string:right(Date, 2, $0)).

check_undefined(undefined) ->
    null;
check_undefined(Else) ->
    Else.


to_atom(Value) when is_binary(Value) ->
    list_to_atom(string:to_lower(binary_to_list(Value))).

to_list([]) ->
    [];
to_list({Proplist}) ->
    Proplist;
to_list(List) ->
    List.
