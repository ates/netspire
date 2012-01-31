-module(netspire).

%% API
-export([start/0, profile_output/0, uptime/0]).

-include("netspire.hrl").

%% @doc Start the netspire application. Useful when testing using the shell.
-spec start() -> ok | {error, term()}.
start() ->
    application:start(sasl),
    application:start(ibrowse),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(couchbeam),
    application:start(netspire).

%% @doc Stop profiling and display results.
-spec profile_output() -> ok.
profile_output() ->
    eprof:stop_profiling(),
    eprof:log("procs.profile"),
    eprof:analyze(procs),
    eprof:log("total.profile"),
    eprof:analyze(total).

%% @doc Tell how long the system has been running.
-spec uptime() -> uptime().
uptime() ->
    {T, _} = erlang:statistics(wall_clock),
    calendar:seconds_to_daystime(erlang:trunc(T / 1000)).
