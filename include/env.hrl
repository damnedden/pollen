-define(ENV_GLOBAL_CH_NAME, "(global)").
-define(ENV_CH_ID_PREFIX, "ch_").
-define(ENV_SERVER_LOGS, true).

%% Client record
-record(client, {
    name,               % user name (atom or string)
    pid,                % PID of the user
    color               % random color assigned in chat
}).

%% Channel record
-record(channel, {
    id,                 % id set by the server
    name,               % channel name (atom or string)
    owner,              % PID or user ID
    whitelist = [],     % list of allowed PIDs
    pid                 % PID of the channel
}).
