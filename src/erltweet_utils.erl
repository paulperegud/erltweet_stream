-module(erltweet_utils).

-export([account_to_record/1,
         parse_opts/2,
         convert/2,
         convert_list/2]).

-include("erltweet_stream.hrl").

account_to_record(Keys) ->
    #account{consumer_key    = get_or_throw(consumer_key,     Keys),
             consumer_secret = get_or_throw(consumer_secret,  Keys),
             token           = get_or_throw(token,            Keys),
             secret          = get_or_throw(secret,           Keys)
            }.

parse_opts([], State) ->
    State;
parse_opts([{callback, Fun} | Rest], State) ->
    NewState = State#state{callback = Fun},
    parse_opts(Rest, NewState);
parse_opts([Unknown | Rest], State) ->
    ?WARNING("Unknown options: ~p~n", [Unknown]),
    parse_opts(Rest, State).


convert_list(X, To) ->
    [ convert(Y, To) || Y <- X ].
convert(X, str_list) when is_binary(X) ->
    lists:flatten(binary_to_list(X));
convert(X, str_list) when is_atom(X) ->
    atom_to_list(X);
convert(X, str_list) when is_list(X) ->
    lists:flatten(X).

get_or_throw(Key, Proplists) ->
    case proplists:get_value(Key, Proplists) of
        undefined -> throw({erltweet, Key, not_defined});
        Value     -> Value
    end.
