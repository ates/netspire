-module(netspire_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-include("netspire.hrl").

-type application_start_type() :: normal
    | {takeover, node()} | {failover, node()}.

-spec start(application_start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    consider_profiling(),
    netspire_logger:start(),
    case netspire_sup:start_link() of
        {ok, _Pid} = Sup ->
            add_code_path(),
            start_services(),
            start_modules(),
            Sup;
        Error ->
            Error
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%
%% Internal functions
%%

%% @doc Starts profiling of the application.
-spec consider_profiling() -> profiling | not_profiling.
consider_profiling() ->
    case application:get_env(profile) of
        {ok, true} ->
            eprof:start(),
            eprof:start_profiling([self()]);
        _ ->
            not_profiling
    end.

%% @doc Adds the specified directories to the end of the code path.
%% Looking for the option <em>code_path</em> with list of the directories 
%% in the configuration file and add they to the end of code path.
%% @end
-spec add_code_path() -> ok.
add_code_path() ->
    case netspire_config:get_option(code_path) of
        undefined -> ok;
        Directories ->
            code:add_pathsz(Directories)
    end.

%% @doc Starts services defined in configuration file.
-spec start_services() -> ok.
start_services() ->
    F = fun({Module, Options}) ->
            Module:start(Options)
    end,
    case netspire_config:get_option(services) of
        undefined -> ok;
        Services ->
            lists:foreach(F, Services)
    end.

%% @doc Start dynamic modules
-spec start_modules() -> ok.
start_modules() ->
    F = fun({Module, Options}) ->
            gen_module:start_module(Module, Options)
    end,
    case netspire_config:get_option(modules) of
        undefined -> ok;
        Modules ->
            lists:foreach(F, Modules)
    end.
