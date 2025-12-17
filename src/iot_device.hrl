
-type id() :: 1..9_999_999_9999.
-type metrics() :: { atom(), integer()}.

-record(iot_device, {
    id              :: id(),
    name            :: string(),
    address         :: string(),
    temperature     :: float(),
    metrics = []    :: [metrics()]
}).