fun({Doc}) ->
        case proplists:get_value(<<"type">>, Doc, null) of
            null ->
                undefined;
            _ ->
                Metric = proplists:get_value(<<"metric">>, Doc),
                Emit(Metric, 1)
        end
end.

