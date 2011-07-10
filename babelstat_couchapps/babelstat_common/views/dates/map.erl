fun({Doc}) ->
        case proplists:get_value(<<"type">>, Doc, null) of
            null ->
                undefined;
            _ ->
                Date = proplists:get_value(<<"date">>, Doc),
                Emit(Date, 1)
        end
end.

