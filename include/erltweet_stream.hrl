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

-define(WARN_LOG(X, Y), (error_logger:warning_msg(X, Y)) ).
-define(ERR_LOG(X, Y), (error_logger:error_msg(X, Y)) ).

                

