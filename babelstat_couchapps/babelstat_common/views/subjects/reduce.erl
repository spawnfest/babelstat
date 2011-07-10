fun(Keys, Values, ReReduce) -> 
	lists:foldl(fun(_V,Acc) ->
			Acc+1 
		    end,0,Values)
end.
