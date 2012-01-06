-module(netflow_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(SvcName, Opts) ->
    ChildSpec = {
        SvcName,
        {netflow_service, start_link, [SvcName, Opts]},
        permanent,
        brutal_kill,
        worker,
        [netflow_service]
    },
    supervisor:start_child(?MODULE, ChildSpec).

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
