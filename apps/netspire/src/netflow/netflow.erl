-module(netflow).

-export([start_service/2, stop_service/1, services/0]).
-export([add_handler/1, add_handler/2, delete_handler/1, delete_handler/2]).

-include("netspire.hrl").

%% @doc Start NetFlow service.
-spec start_service(atom(), [netflow_opt()]) -> ok | {error, term()}.
start_service(SvcName, Opts) ->
    netflow_sup:start_child(SvcName, Opts).

%% @doc Stop NetFlow service.
-spec stop_service(atom()) -> ok | {error, term()}.
stop_service(SvcName) ->
    gen_server:call(SvcName, stop),
    supervisor:terminate_child(netflow_sup, SvcName),
    supervisor:delete_child(netflow_sup, SvcName).

%% @doc Register packet handler for all services.
-spec add_handler(atom()) -> ok | {error, term()}.
add_handler(Module) ->
    [add_handler(SvcName, Module) || SvcName <- services()].

%% @doc Register packet handler for specific service.
-spec add_handler(atom(), atom()) -> ok | {error, term()}.
add_handler(SvcName, Module) ->
    gen_server:call(SvcName, {add_handler, Module}).

%% @doc Unregister packet handler from all services.
-spec delete_handler(atom()) -> ok | {error, term()}.
delete_handler(Module) ->
    [delete_handler(SvcName, Module) || SvcName <- services()].

%% @doc Unregister packet handler from specific service.
-spec delete_handler(atom(), atom()) -> ok | {error, term()}.
delete_handler(SvcName, Module) ->
    gen_server:call(SvcName, {delete_handler, Module}).

%% @doc Returns list of the started services.
-spec services() -> [atom()].
services() ->
    [element(1, Spec) || Spec <- supervisor:which_children(netflow_sup)].
