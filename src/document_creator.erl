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
-export([test_docs_to_couchdb/4, send_documents_to_couchdb/10]).
-include("../include/babelstat.hrl").
%%%===================================================================
%%% API
%%%===================================================================
test_docs_to_couchdb(Metric, Scale,Frequency,Title) ->    
    Docs = create_test_docs(Metric, Scale,Frequency,Title),
    lists:foreach(fun(Doc) ->
			  babelstat_couchdb:save_document(Doc)
		  end,Docs).
    
send_documents_to_couchdb(Category, SubCategory, Subject, SeriesCategory, Title, Source, DatesAndValues,Metric,Scale,Frequency) ->    
    Docs = create_docs(Category, SubCategory, Subject, SeriesCategory, Title, Source, DatesAndValues,Metric,Scale,Frequency),
    lists:foreach(fun(Doc) ->
			  babelstat_couchdb:save_document(Doc)
		  end,Docs).
   
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_docs(Category, SubCategory, Subject, SeriesCategory, Title, Source, DatesAndValues,Metric, Scale,Frequency) ->
    lists:map(fun({Date,Value}) ->    		
		      #babelstat{date = Date,
				 value = Value,
				 metric =  Metric,
				 scale = Scale,
				 frequency = Frequency,
				 location = [],
				 category = Category,
				 sub_category = SubCategory,
				 subject = Subject,
				 series_category = SeriesCategory,
				 title = Title,
				 source = Source,
				 calculation = undefined,
				 constant = false
				}
			  end, DatesAndValues).
    
create_test_docs(Metric,Scale,Frequency, Title) ->
    DateList = dates:get_range({{2000,1,1},{2011,7,10}}),
    
    DatesAndValues = lists:map(fun(N) ->    		      
		      Date = lists:nth(DateList,N),
		      {Date,N*1.0}
	      end,lists:seq(1,length(DateList))),
    create_docs(<<"Spawnfest">>,<<"Teams">>,<<"Jesus don't want me for a sunBEAM">>,<<"code">>,Title,<<"pure fiction">>, DatesAndValues, Metric,Scale,Frequency).

    
    
