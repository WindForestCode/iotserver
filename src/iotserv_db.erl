-module(iotserv_db).
-include("iot_device.hrl").

-export([create_tables/1, close_tables/0, add_device/1, delete_device_by_id/1,
    update_device_by_id/3, find_device_by_id/1, restore_database/0]).

create_tables(DataBaseName) ->
    ets:new(iotDevicesRam, [named_table,
        set,
        {keypos, #iot_device.id}]),

    dets:open_file(iotDevicesDisk, [{file, DataBaseName},
        {type, set},
        {keypos, #iot_device.id}]).

close_tables() ->
    ets:delete(iotDevicesRam),
    dets:close(iotDevicesDisk).

add_device(Device) ->
    ets:insert(iotDevicesRam, Device),
    dets:insert(iotDevicesDisk, Device),
    ok.

delete_device_by_id(Id) ->
    case dets:member(iotDevicesDisk, Id) of
        true ->
            dets:delete(iotDevicesRam, Id),
            ets:delete(iotDevicesDisk, Id),
            ok;
        false ->
            {error, not_found}
    end.

find_device_by_id(Id) ->
    case ets:lookup(iotDevicesRam, Id) of
        [Device] -> {ok, Device};
        [] -> {error, instance}
    end.

update_device_by_id(Id, Field, NewValue) ->
    case ets:lookup(iotDevicesRam, Id) of
        [Device] ->
            UpdatedDevice = update_field(Device, Field, NewValue),
            ets:insert(iotDevicesRam, UpdatedDevice),
            dets:insert(iotDevicesDisk, UpdatedDevice),
            {ok, UpdatedDevice};
        [] ->
            {error, not_found}
    end.


update_field(Device, name, NewValue) when is_list(NewValue) ->
    Device#iot_device{name = NewValue};

update_field(Device, address, NewValue) when is_list(NewValue) ->
    Device#iot_device{address = NewValue};

update_field(Device, temperature, NewValue) when is_float(NewValue) ->
    Device#iot_device{temperature = NewValue};

update_field(Device, metrics, NewValue) when is_list(NewValue) ->
    Device#iot_device{metrics = NewValue};

update_field(_, Field, _) ->
    {invalid_field, Field}.

restore_database() ->
    ets:from_dets(iotDevicesRam, iotDevicesDisk).