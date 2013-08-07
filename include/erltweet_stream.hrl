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

-define(LOG(Format, Args, Level),
        lager:Level("~p ~p [~p:~p] " ++ Format, [node(), self(), ?MODULE, ?LINE] ++ Args)).
-define(DEBUG(X, Y), ?LOG(X, Y, debug)).
-define(INFO(X, Y), ?LOG(X, Y, info)).
-define(NOTICE(X, Y), ?LOG(X, Y, notice)).
-define(WARNING(X, Y), ?LOG(X, Y, warning)).
-define(ERROR(X, Y), ?LOG(X, Y, error)).
