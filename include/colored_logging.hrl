%%%-------------------------------------------------------------------
%%% @doc
%%% Universal Colored Logging Header
%%% Provides consistent colored logging macros for all modules
%%% @end
%%%-------------------------------------------------------------------

%% Core colored logging macros using the colored_logger module
-define(LOG(Level, Format, Args), 
    colored_logger:log(Level, atom_to_list(?MODULE), "[~s:~p] " ++ Format, [?MODULE, ?LINE | Args])).

-define(LOG_INFO(Format, Args), ?LOG(info, Format, Args)).
-define(LOG_WARN(Format, Args), ?LOG(warning, Format, Args)).
-define(LOG_ERROR(Format, Args), ?LOG(error, Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG(debug, Format, Args)).
-define(LOG_SUCCESS(Format, Args), ?LOG(success, Format, Args)).

%% Specialized colored logging for different contexts
-define(LOG_STARTUP(Format, Args), colored_logger:startup(stable, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).
-define(LOG_NETWORK(Format, Args), colored_logger:network(connected, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).
-define(LOG_SECURITY(Format, Args), colored_logger:security(safe, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).
-define(LOG_PERFORMANCE(Format, Args), colored_logger:performance(fast, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).
-define(LOG_DATA(Format, Args), colored_logger:data(processed, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).
-define(LOG_SYSTEM(Format, Args), colored_logger:system(cpu, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).
-define(LOG_DEVELOPMENT(Format, Args), colored_logger:development(coding, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).
-define(LOG_PRODUCTION(Format, Args), colored_logger:production(stable, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).

%% Special effect logging for dramatic moments
-define(LOG_FIRE(Format, Args), colored_logger:fire(inferno, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).
-define(LOG_COSMIC(Format, Args), colored_logger:cosmic(star, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).
-define(LOG_CELEBRATION(Format, Args), colored_logger:celebration(success, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).
-define(LOG_ALARM(Format, Args), colored_logger:alarm(critical, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).

%% Simple forms without arguments
-define(LOG_INFO(Message), ?LOG_INFO(Message, [])).
-define(LOG_WARN(Message), ?LOG_WARN(Message, [])).
-define(LOG_ERROR(Message), ?LOG_ERROR(Message, [])).
-define(LOG_DEBUG(Message), ?LOG_DEBUG(Message, [])).
-define(LOG_SUCCESS(Message), ?LOG_SUCCESS(Message, [])).
-define(LOG_STARTUP(Message), ?LOG_STARTUP(Message, [])).
-define(LOG_NETWORK(Message), ?LOG_NETWORK(Message, [])).
-define(LOG_SECURITY(Message), ?LOG_SECURITY(Message, [])).
-define(LOG_PERFORMANCE(Message), ?LOG_PERFORMANCE(Message, [])).
-define(LOG_DATA(Message), ?LOG_DATA(Message, [])).
-define(LOG_SYSTEM(Message), ?LOG_SYSTEM(Message, [])).
-define(LOG_DEVELOPMENT(Message), ?LOG_DEVELOPMENT(Message, [])).
-define(LOG_PRODUCTION(Message), ?LOG_PRODUCTION(Message, [])).
-define(LOG_FIRE(Message), ?LOG_FIRE(Message, [])).
-define(LOG_COSMIC(Message), ?LOG_COSMIC(Message, [])).
-define(LOG_CELEBRATION(Message), ?LOG_CELEBRATION(Message, [])).
-define(LOG_ALARM(Message), ?LOG_ALARM(Message, [])).