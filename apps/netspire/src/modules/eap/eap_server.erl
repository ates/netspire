-module(eap_server).

-behaviour(gen_server).

-include("eap.hrl").
-include("radius.hrl").
-include("netspire.hrl").

%% API
-export([start_link/0, verify_eap/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

verify_eap(_, Request, UserName, Password, Replies, Client) ->
    Call = {verify_eap, Request, UserName, Password, Replies, Client},
    gen_server:call(?MODULE, Call).

init([]) ->
    netspire_hooks:add(radius_auth, ?MODULE, verify_eap),
    {ok, no_state}.

% Request, UserName, Password, Replies, Client
handle_call({verify_eap, Request, UserName, Password, Replies, Client}, _From, State) ->
    case libeap:attribute_value(Request) of
        undefined -> % not found EAP-Message attribute
            {reply, undefined, State};
        Value ->
            % Found EAP-Message attribute
            <<_Code:8, Ident:8, _Len:16, Type:8, Data/binary>> = Value,
            Opts = [UserName, Password, Ident, Type, Data, Request, Client],
            Reply = case do_auth(Opts) of
                {accept, Attrs} ->
                    ?INFO_MSG("~s authentication succeeded: ~s~n",
                        [libeap:type2string(Type), UserName]),
                    {accept, ?EAP_SUCCESS(Ident) ++ Attrs ++ Replies};
                {challenge, _} = Result ->
                    Result;
                reject ->
                    ?INFO_MSG("~s authentication failed: ~s~n",
                        [libeap:type2string(Type), UserName]),
                    {reject, ?EAP_FAILURE(Ident)};
                _ ->
                    {reject, ?EAP_FAILURE(Ident)}
            end,
            {reply, {stop, Reply}, State}
    end.

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Request, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) ->
    netspire_hooks:delete_all(?MODULE).

do_auth([UserName, _Password, Ident, ?EAP_IDENTIFY, _Data, _Request, _Client]) ->
    {ok, Challenge} = eap_md5:challenge(UserName, Ident),
    ?INFO_MSG("Sending EAP-MD5 challenge for ~s~n", [UserName]),
    {challenge, Challenge};
do_auth([UserName, Password, Ident, ?EAP_MD5_CHALLENGE, Data, _Request, _Client]) ->
    case eap_md5:authenticate(UserName, Ident, Password, Data) of
        true ->
            {accept, []};
        _ -> reject
    end;
do_auth([UserName, _Password, Ident, ?EAP_NAK, <<AuthType:8>>, _Request, _Client]) ->
    case AuthType of
        ?EAP_MSCHAPV2 ->
            {ok, Challenge} = eap_mschap_v2:challenge(UserName, Ident),
            ?INFO_MSG("Sending EAP-MSCHAPv2 challenge for ~s~n", [UserName]),
            {challenge, Challenge};
        _ ->
            ?ERROR_MSG("Not supported EAP authentication type ~s~n",
                [libeap:type2string(AuthType)])
    end;
do_auth([UserName, Password, Ident, ?EAP_MSCHAPV2, Data, Request, Client]) ->
    eap_mschap_v2:authenticate(UserName, Password, Ident, Data, Request, Client).
