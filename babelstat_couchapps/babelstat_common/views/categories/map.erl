fun({Doc}) ->
        case proplists:get_value(<<"type">>, Doc, null) of
            null ->
                undefined;
            _ ->
                Category = proplists:get_value(<<"category">>, Doc),
                Emit(Category, 1)
        end
end.

