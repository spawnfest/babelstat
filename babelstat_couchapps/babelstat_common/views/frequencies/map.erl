fun({Doc}) ->
        case proplists:get_value(<<"type">>, Doc, null) of
            null ->
                undefined;
            _ ->
                Frequency = proplists:get_value(<<"frequency">>, Doc),
                Emit(Frequency, 1)
        end
end.

