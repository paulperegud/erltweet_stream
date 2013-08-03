%%%-------------------------------------------------------------------
%%% @author Paul Peter Flis <pawel@flycode.pl>
%%% @copyright (C) 2013, Livepress
%%% @doc
%%%
%%% @end
%%% Created : 31 Jul 2013 by Paul Peter Flis <pawel@flycode.pl>
%%%-------------------------------------------------------------------
-module(erltweet_stream).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/2]).

-export([new/2,
         filter/3,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erltweet_stream.hrl").

-define(SERVER, ?MODULE).

%% -record{state [...] - state is defined in erltweet_stream.hrl

%%%===================================================================
%%% API
%%%===================================================================

new(AccountKeys, Opts) ->
    erltweet_stream:start_link(AccountKeys, Opts).

%% TO DO: locations method
filter(Pid, Method, SearchKeys) when Method =:= follow; Method =:= track ->
    gen_server:call(Pid, {filter, Method, SearchKeys}, infinity).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {error, not_set_account}.
start_link(AccountKeys, Opts) ->
    gen_server:start_link(?MODULE, [AccountKeys, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([AccountKeys, Opts]) ->
    Account = erltweet_utils:account_to_record(AccountKeys),
    State  = erltweet_utils:parse_opts(Opts, #state{}),
    {ok, State#state{account = Account}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({filter, Method, SearchKeys}, _From, #state{account = Account} = State) ->
    Keys2 = erltweet_utils:convert_list(SearchKeys, str_list),
    QS = [{atom_to_list(Method), string:join(Keys2, ",")}],
    case connect(?URI_FILTER, QS, Account) of
        {ok, ReqId} ->
            ?INFO_LOG("Start stream for QS: ~p~nReqId: ~p~n", [QS, ReqId]),
            {reply, ok, State#state{request_id = ReqId} };
        {error, Reason} = Error ->
            ?ERR_LOG("Error connection: ~p~n", [Reason]),
            {reply, Error, State}
    end;

handle_call(stop, _From, #state{request_id = ReqId} = State) ->
    ?INFO_LOG("Stop request: ~p", [ReqId]),
    case ibrowse:stream_close(ReqId) of
        ok ->
            ok;
        {error, unknown_req_id} ->
            ?WARN_LOG("Try close unknown request_id: ~p~n", [ReqId]),
            ok
    end,
    {stop, normal, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({ibrowse_async_headers, ReqId, _Code, _Headers}, State = #state{request_id = ReqId}) ->
    ibrowse:stream_next(ReqId),
    {noreply, State};

handle_info({ibrowse_async_response, ReqId, Body}, State = #state{request_id = ReqId,
                                                                  buffer     = Buffer,
                                                                  callback   = CB}) ->
    case binary:split(Body, <<$\r>>, [global, trim]) of
        [] ->
            ok = ibrowse:stream_next(ReqId),
            {noreply, State};
        [Body] -> %% No \r
            ok = ibrowse:stream_next(ReqId),
            {noreply, State#state{buffer = <<Buffer/binary, $|, Body/binary>>}};
        [Head|Tail] ->
            RealBuffer = re:replace(Buffer, <<"\\|">>, <<>>, [global, {return, binary}]),
            {Jsons, NewBuffer} =
                try extract_jsons([<<RealBuffer/binary, Head/binary>> | Tail])
                catch
                    _:Error ->
                        ?ERR_LOG("Error in parse json: ~p~n", [Error])
                end,
            case is_function(CB) of
                true  -> [CB(Tweet) || Tweet <- Jsons];
                false -> ?WARN_LOG("Received tweets, but callback is undefined...~n", [])
            end,
            ok = ibrowse:stream_next(ReqId),
            {noreply, State#state{buffer = NewBuffer}}
    end;

handle_info({ibrowse_async_response, ReqId, {error, req_timedout}}, State = #state{request_id = ReqId}) ->
    ?ERR_LOG("There're no more twitter results~n", []),
    {stop, normal, State};
handle_info({ibrowse_async_response, ReqId, {error, connection_closed}}, State = #state{request_id = ReqId}) ->
    ?ERR_LOG("Twitter hung up on us~n", []),
    {stop, normal, State};
handle_info({ibrowse_async_response, ReqId, {error, Error}}, State = #state{request_id = ReqId}) ->
    ?ERR_LOG("Error querying twitter: ~p~n", [Error]),
    {stop, {error, Error}, State};
handle_info({ibrowse_async_response_end, ReqId}, State = #state{request_id = ReqId}) ->
    ?INFO_LOG("End stream: ~p~n", [ReqId]),
    {stop, normal, State};

handle_info(_Info, State) ->
    io:fwrite("Unknown info message: ~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect(Uri, QS, Account) ->
    Consumer = {Account#account.consumer_key,
                Account#account.consumer_secret,
                hmac_sha1},
    Token  = Account#account.token,
    Secret = Account#account.secret,
    Options = [{stream_to,       {self(), once}},
               {response_format, binary},
               {connect_timeout, 25000}], %% wait for connected

    try oauth:get(Uri, QS, Consumer, Token, Secret, Options, infinity) of
        {ibrowse_req_id, ReqId}      -> {ok, ReqId};
        {ok, Status, _Headers, Body} -> {error, {Status, Body}};
        {error, Reason}              -> {error, Reason}
    catch
        _:{timeout, _} -> {error, internal_timeout};
        _:Error        -> {error, Error}
    end.

extract_jsons(Lines) ->
    extract_jsons(Lines, []).
extract_jsons([], Acc) ->
    {lists:reverse(Acc), <<>>};
extract_jsons([<<$\n>>], Acc) ->
    {lists:reverse(Acc), <<>>};
extract_jsons([NewBuffer], Acc) ->
    %%HACK: Even when Twitter Stream API docs say that...
    %%          ...every object is returned on its own line, and ends with a carriage return. Newline
    %%          characters (\n) may occur in object elements (the text element of a status object, for
    %%          example), but carriage returns (\r) should not.
    %%      ...sometimes they just don't send the \r after the last object
    try jsx:decode(NewBuffer) of
        {incomplete, _} ->
            ?WARN_LOG("Incomplete json: ~p~n", [NewBuffer]),
            {Acc, <<>>};
        Json ->
            {lists:reverse([Json|Acc]), <<>>}
    catch
        throw:{invalid_json, NewBuffer, _Err} ->
            {lists:reverse(Acc), NewBuffer}
    end;
extract_jsons([<<>> | Rest], Acc) ->
    extract_jsons(Rest, Acc);
extract_jsons([<<$\n>> | Rest], Acc) ->
    extract_jsons(Rest, Acc);
extract_jsons([Next | Rest], Acc) ->
    case jsx:decode(Next) of
        {incomplete, _} ->
            ?DEBUG_LOG("Incomplete json: ~p~n", [Next]),
            extract_jsons(Rest, Acc);
        Json ->
            extract_jsons(Rest, [Json | Acc])
    end.

