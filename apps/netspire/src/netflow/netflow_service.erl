-module(netflow_service).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-include("netspire.hrl").
-include("netflow_v5.hrl").
-include("netflow_v9.hrl").

-record(state, {socket, handlers = []}).

start_link(Ident, [IP, Family, Port]) ->
    gen_server:start_link({local, Ident}, ?MODULE, [IP, Family, Port], []).

apply_packet_handlers(SrcIP, Pdu, Handlers) ->
    lists:foreach(fun(Mod) -> Mod:handle_packet(SrcIP, Pdu) end, Handlers).

process_packet(<<9:16, _/binary>> = Packet, IP) ->
    netflow_v9:decode(Packet, IP);
process_packet(<<5:16, _/binary>> = Packet, _) ->
    netflow_v5:decode(Packet);
process_packet(_, _) ->
    {error, unknown_packet}.

init([IP, Family, Port]) ->
    SocketOpts = [binary, Family, {ip, IP}, {reuseaddr, true}],
    case gen_udp:open(Port, SocketOpts) of
        {ok, Socket} ->
            {ok, #state{socket = Socket}};
        {error, Reason} ->
            Msg = inet:format_error(Reason),
            ?ERROR_MSG("Can not start NetFlow service: ~s~n", [Msg])
    end.

handle_info({udp, _Socket, IP, _InPortNo, Packet}, State) ->
    case process_packet(Packet, IP) of
        {ok, Pdu} ->
            apply_packet_handlers(IP, Pdu, State#state.handlers);
        {error, {badpdu, Reason}} ->
            ?INFO_MSG("Invalid packet has been discarded: ~p~n", [Reason]);
        {error, Reason} ->
            ?INFO_MSG("Unable to process packet: ~p~n", [Reason])
    end,
    {noreply, State}.

handle_call({add_handler, Module}, _From, State) ->
    ?INFO_MSG("Registered packet handler: ~p~n", [Module]),
    Handlers = [Module | State#state.handlers],
    {reply, ok, State#state{handlers = Handlers}};
handle_call({delete_handler, Module}, _From, State) ->
    ?INFO_MSG("Unregistering packet handler: ~p~n", [Module]),
    Handlers = lists:delete(Module, State#state.handlers),
    {reply, ok, State#state{handlers = Handlers}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    gen_udp:close(State#state.socket).
