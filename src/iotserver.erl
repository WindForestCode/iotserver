
-module(iotserver).
-author("MoonNew").

-export([start_link/0, start_link/1, stop/0]).
-export([add_device/5, delete_device_by_id/1, find_device_by_id/1, change_device_by_id/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

-behavior(gen_server).

-include("iot_device.hrl").

%% Exported client functions
-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, FileName} = application:get_env(dets_name),
    start_link(FileName).

-spec start_link(string()) -> {ok, pid()}.
start_link(FileName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Customer Services API
-spec add_device(id(), string(), string(), float(), metrics()) -> term().
add_device(Id, Name, Address, Temperature, Metrics) ->
    gen_server:call(?MODULE, {add_device, Id, Name, Address, Temperature, Metrics}).

-spec delete_device_by_id(id()) -> term().
delete_device_by_id(Id) ->
    gen_server:call(?MODULE, {delete_device, Id}).

-spec find_device_by_id(id()) -> term().
find_device_by_id(Id) ->
    gen_server:call(?MODULE, {find_device_by_id, Id}).

-spec change_device_by_id(id(), atom(), term()) -> term().
change_device_by_id(Id, FieldToUpdate, NewValue) ->
    gen_server:call(?MODULE, {change_device, Id, FieldToUpdate, NewValue}).

%% Callback functions
-spec init(string()) -> {ok, _LoopData} | {ok, null}.
init(FileName) ->
    iotserver_db:create_tables(FileName),
    iotserver_db:restore_database(),
    {ok, null}.

terminate(_Reason, _LoopData) ->
    iotserver_db:close_tables().

handle_call({add_device, Id, Name, Address, Temperature, Metrics}, _From, LoopData) ->
    Reply = iotserver_db:add_device(#iot_device{
        id = Id,
        name = Name,
        address = Address,
        temperature = Temperature,
        metrics = Metrics}),
    {reply, Reply, LoopData};

handle_call({delete_device, Id}, _From, LoopData) ->
    Reply = iotserver_db:delete_device_by_id(Id),
    {reply, Reply, LoopData};

handle_call({find_device, Id}, _From, LoopData) ->
    Reply = iotserver_db:find_device_by_id(Id),
    {reply, Reply, LoopData};

%% Изменение только одного параметра.
handle_call({change_device, Id, FieldToUpdate, NewValue}, _From, LoopData) ->
    Reply = iotserver_db:update_device_by_id(Id, FieldToUpdate, NewValue),
    {reply, Reply, LoopData}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.