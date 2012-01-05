-module(netspire_logger).

%% API
-export([start/0]).

%% @doc Determines the path to the log file and open it.
-spec start() -> ok | {error, term()}.
start() ->
    FileName = case os:getenv("NETSPIRE_LOGFILE") of
        false ->
            case application:get_env(logfile) of
                undefined ->
                    throw({error, no_logfile_is_defined});
                {ok, File} -> File
            end;
        File -> File
    end,
    case filelib:is_regular(FileName) of
        true ->
            Name = filename:rootname(FileName) ++ timestamp_suffix() ++ ".log",
            ok = file:rename(FileName, Name);
        false ->
            ok
    end,
    error_logger:logfile({open, FileName}).

%%
%% Internal functions
%%
-spec timestamp_suffix() -> string().
timestamp_suffix() ->
    DateTime = lists:flatten(localtime_to_string()),
    [Date, Time] = string:tokens(DateTime, " "),
    Suffix = "-" ++ Date ++ "_" ++ Time, Suffix.

-spec localtime_to_string() -> string().
localtime_to_string() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
        [Year, Month, Day, Hour, Minute, Second]).
