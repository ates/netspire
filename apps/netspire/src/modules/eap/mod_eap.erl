-module(mod_eap).

-behaviour(gen_module).

-include("netspire.hrl").

%% gen_module callbacks
-export([start/1, stop/0]).

start(_Options) ->
    ?INFO_MSG("Starting dynamic module ~p~n", [?MODULE]),
    ChildSpec = {
        eap_sup,
        {eap_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        [eap_sup]
    },
    supervisor:start_child(netspire_sup, ChildSpec).

stop() ->
    ?INFO_MSG("Stopping dynamic module ~p~n", [?MODULE]),
    supervisor:terminate_child(netspire_sup, eap_sup),
    supervisor:delete_child(netspire_sup, eap_sup).
