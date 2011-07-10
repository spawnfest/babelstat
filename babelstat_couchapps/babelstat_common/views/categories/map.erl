fun({Doc}) ->
        case proplists:get_value(<<"type">>, Doc, null) of
            null ->
                <<"ignore me">>;
            _ ->
                Category = proplists:get_value(<<"category">>, Doc),
                Emit(Category, 1)
        end
end.

