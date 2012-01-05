-define(INFO_MSG(Format, Args), error_logger:info_msg(Format, Args)).
-define(WARNING_MSG(Format, Args), error_logger:warning_msg(Format, Args)).
-define(ERROR_MSG(Format, Args), error_logger:error_msg(Format, Args)).

-type uptime() :: {non_neg_integer(),
    {calendar:hour(), calendar:minute(), calendar:second()}}.

-type ip_address() :: string() | inet:ip_address().

-type netflow_opt()
    :: {mode, server | relay}
     | {listen, {ip_address(), non_neg_integer()}}
     | {relay, [{ip_address(), non_neg_integer()}]}.
