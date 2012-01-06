-module(netspire_netflow).

%% API
-export([start/1]).

-include("netspire.hrl").

-spec start([netflow_opt()]) -> ok.
start(Options) ->
    SvcName = proplists:get_value(name, Options),
    {Address, Port} = proplists:get_value(listen, Options),
    {ok, {Family, IP}} = ip:address(Address),
    case netflow:start_service(SvcName, [IP, Family, Port]) of
        {ok, _} ->
            ?INFO_MSG("NetFlow service ~p started on ~s:~p~n",
                [SvcName, inet_parse:ntoa(IP), Port]);
        _ -> ok
    end.
