fun({Doc}) ->
	case proplists:get_value(<<created_at>>,Doc,null) of
		null ->
	            undefined;
		CreatedAt ->
		    Emit(CreatedAt, {Doc})
	end
end.
