%%%-------------------------------------------------------------------
%%% @author nisbus <>
%%% @copyright (C) 2011, nisbus
%%% @doc
%%% Used for testing purposes only
%%% @end
%%% Created :  9 Jul 2011 by nisbus <>
%%%-------------------------------------------------------------------
-module(document_creator).

%% API
-export([get_docs/0, get_docs/3,send_to_couchdb/0, send_to_couchdb/3]).

%%%===================================================================
%%% API
%%%===================================================================
get_docs() ->
    lists:map(fun(N) ->
		     create_doc(N)
	      end,lists:seq(1,20)).
		  
get_docs(Metric,Scale,Frequency) ->
    lists:map(fun(N) ->
		     create_doc(Metric, Scale, Frequency,N)
	      end,lists:seq(1,20)).

send_to_couchdb() ->    
    lists:map(fun(N) ->
		      Doc = create_doc(N),
		      babelstat_couchdb:save_document(Doc)
	      end,lists:seq(1,20)).
    
send_to_couchdb(Metric,Scale,Frequency) ->    
    lists:map(fun(N) ->
		      Doc = create_doc(Metric, Scale, Frequency,N),
		      babelstat_couchdb:save_document(Doc)
	      end,lists:seq(1,20)).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_doc(N) ->
    Date = "2011-7-"++integer_to_list(N),
    {[
      {<<"type">>,<<"babelstat">>},
      {<<"date">>,list_to_binary(Date)},
      {<<"value">>, N*1.0},
      {<<"metric">>, <<"unit">>},
      {<<"scale">>, 1},
      {<<"frequency">>,<<"daily">>},
      {<<"location">>,<<"[444,33]">>},
      {<<"category">>,<<"Spawnfest">>},
      {<<"sub_category">>,<<"Teams">>},
      {<<"subject">>,<<"Jesus don't want me for a sunBEAM">>},
      {<<"series_category">>,<<"code">>},
      {<<"title">>,<<"number of lines">>},
      {<<"source">>,<<"pure fiction">>},
      {<<"calculation">>,false},
      {<<"constant">>,false}
     ]}.

create_doc(Metric, Scale, Frequency,N) -> 
    Date = "2011-7-"++integer_to_list(N),
    {[
      {<<"type">>,<<"babelstat">>},
      {<<"date">>,list_to_binary(Date)},
      {<<"value">>, N*1.0},
      {<<"metric">>, Metric},
      {<<"scale">>, Scale},
      {<<"frequency">>,Frequency},
      {<<"location">>,<<"[444,33]">>},
      {<<"category">>,<<"Spawnfest">>},
      {<<"sub_category">>,<<"Teams">>},
      {<<"subject">>,<<"Jesus don't want me for a sunBEAM">>},
      {<<"series_category">>,<<"code">>},
      {<<"title">>,<<"number of lines">>},
      {<<"source">>,<<"pure fiction">>},
      {<<"calculation">>,false},
      {<<"constant">>,false}
     ]}.
    
