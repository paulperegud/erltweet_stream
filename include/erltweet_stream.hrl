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
-define(ERR_LOG(X, Y), (error_logger:error_msg(?LOG(X), Y)) ).
-define(WARN_LOG(X, Y), (error_logger:warning_msg(?LOG(X), Y)) ).
-define(INFO_LOG(X, Y), (error_logger:info_msg(?LOG(X), Y)) ).
