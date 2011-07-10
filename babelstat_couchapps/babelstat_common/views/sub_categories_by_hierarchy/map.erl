fun({Doc}) ->
        case proplists:get_value(<<"type">>, Doc, null) of
            null ->
                undefined;
            _ ->
                Category = proplists:get_value(<<"category">>, Doc),
		SubCategory = proplists:get_value(<<"sub_category">>, Doc),

                Emit([Category,SubCategory], 1)
        end
end.
