-module(eap_reg).

-behaviour(gen_server).

-include("netspire.hrl").

%% API
-export([start_link/0, push/2, pull/1, pull/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-define(REGISTRY, eap_register).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push(Id, Data) ->
    gen_server:call(?MODULE, {push, Id, Data}).

pull(Id) ->
    gen_server:call(?MODULE, {pull, Id, false}).

%% Set flag to true to remove entry from registry
pull(Id, Flag) ->
    gen_server:call(?MODULE, {pull, Id, Flag}).

init([]) ->
    ets:new(?REGISTRY, [named_table]),
    {ok, no_state}.

handle_call({push, Id, Data}, _From, State) ->
    ets:insert(?REGISTRY, {Id, Data}),
    {reply, ok, State};
handle_call({pull, Id, Flag}, _From, State) ->
    Reply = case ets:lookup(?REGISTRY, Id) of
        [] ->
            ?WARNING_MSG("No data for ID ~p in EAP registry~n", [Id]),
            undefined;
        [{Id, Data} = Object] ->
            case Flag of
                true ->
                    ets:delete_object(?REGISTRY, Object);
                _ -> ok
            end,
            Data
    end,
    {reply, Reply, State}.

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Request, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.
