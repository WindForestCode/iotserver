
-module(iotserv).
-author("MoonNew").

-export([]).
-export([init/1, handle_call/3, handle_cast/2]).

-behavior(gen_server).

-include("iot_device.hrl").

%% Callback fun
init(FileName) ->
    iotserv_db:create_tables(FileName),
    iotserv_db:restore_database(),
    {ok, null}.

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
handle_call({change_device, Id, Field, NewValue}, _From, LoopData) ->
    Reply = iotserv_db:update_device_by_id(Id, Field, NewValue),
    {reply, Reply, LoopData}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

