-record(account, {consumer_key    :: binary(),
                  consumer_secret :: binary(),
                  token           :: binary(),
                  secret          :: binary()}).

-record(state, {account       :: #account{},
                callback      :: function(),
                request_id    :: term(),
                buffer = <<>> :: binary()
               }).

-define(URI_FILTER, "https://stream.twitter.com/1.1/statuses/filter.json").

-define(LOG(X), (lists:flatten(io_lib:format("=NODE: ~p | MODULE: ~p | PID: ~p | LINE: ~p ===~n~s", [node(), ?MODULE, self(), ?LINE, X]))) ).
-define(ERR_LOG(X, Y), (lager:error(?LOG(X), Y)) ).
-define(WARN_LOG(X, Y), (lager:warning(?LOG(X), Y)) ).
-define(INFO_LOG(X, Y), (lager:info(?LOG(X), Y)) ).
-define(DEBUG_LOG(X, Y), (lager:debug(?LOG(X), Y)) ).
