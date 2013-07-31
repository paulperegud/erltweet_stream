-module(erltweet_utils).

-export([account_to_record/1,
         parse_opts/2]).

-include("erltweet_stream.hrl").

account_to_record(Keys) ->
    #account{consumer_key    = get_or_throw(consumer_key,     Keys),
             consumer_secret = get_or_throw(consumer_secret,  Keys),
             token           = get_or_throw(token,            Keys),
             token_secret    = get_or_throw(token_secret,     Keys)
            }.

parse_opts([], State) ->
    State;
parse_opts([{callback, Fun} | Rest], State) ->
    NewState = State#state{callback = Fun},
    parse_opts(Rest, NewState);
parse_opts([Unknown | Rest], State) ->
    ?WARN_LOG("Unknown options: ~p~n", [Unknown]),
    parse_opts(Rest, State).



get_or_throw(Key, Proplists) ->
    case proplists:get_value(Key, Proplists) of
        undefined -> throw({erltweet, Key, not_defined});
        Value     -> Value
    end.
            
