-record(account, {consumer_key    :: binary(),
                  consumer_secret :: binary(),
                  token           :: binary(),
                  token_secret    :: binary()}).

-record(state, {account  :: #account{},
                callback :: function()
               }).


-define(WARN_LOG(X, Y), (error_logger:warning_msg(X, Y)) ).
                

