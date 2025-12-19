
-module(iotserv).
-author("MoonNew").

-export([start_link/0, start_link/1, stop/0]).
-export([add_device/5, delete_device_by_id/1, find_device_by_id/1, change_device_by_id/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

-behavior(gen_server).

-include("iot_device.hrl").

%% Exported client functions

start_link() ->
    {ok, FileName} = application:get_env(dets_name),
    start_link(FileName).

start_link(FileName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Customer Services API
add_device(Id, Name, Address, Temperature, Metrics) ->
    gen_server:call(?MODULE, {add_device, Id, Name, Address, Temperature, Metrics}).

delete_device_by_id(Id) ->
    gen_server:call(?MODULE, {delete_device, Id}).

find_device_by_id(Id) ->
    gen_server:call(?MODULE, {find_device_by_id(Id)}).

change_device_by_id(Id, FieldToUpdate, NewValue) ->
    gen_server:call(?MODULE, {change_device, Id, FieldToUpdate, NewValue}).

%% Callback functions
init(FileName) ->
    iotserv_db:create_tables(FileName),
    iotserv_db:restore_database(),
    {ok, null}.

terminate(_Reason, _LoopData) ->
    iotserv_db:close_tables().

handle_call({add_device, Id, Name, Address, Temperature, Metrics}, _From, LoopData) ->
    Reply = iotserv_db:add_device(#iot_device{
        id = Id,
        name = Name,
        address = Address,
        temperature = Temperature,
        metrics = Metrics}),
    {reply, Reply, LoopData};

handle_call({delete_device, Id}, _From, LoopData) ->
    Reply = iotserv_db:delete_device_by_id(Id),
    {reply, Reply, LoopData};

handle_call({find_device, Id}, _From, LoopData) ->
    Reply = iotserv_db:find_device_by_id(Id),
    {reply, Reply, LoopData};

%% Изменение только одного параметра.
handle_call({change_device, Id, FieldToUpdate, NewValue}, _From, LoopData) ->
    Reply = iotserv_db:update_device_by_id(Id, FieldToUpdate, NewValue),
    {reply, Reply, LoopData}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.