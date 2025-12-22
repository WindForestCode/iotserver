
-module(conf_reader).
-author("MoonNew").

-export([read_config/0]).

read_config() ->
    FileName = "priv/config.json",
    case file:read_file(FileName) of
        {ok, Binary} ->
            try jsx:decode(Binary, [return_maps]) of
                ConfigJson ->
                    #{<<"database">> := #{<<"path">> := Path}} = ConfigJson,
                    binary_to_list(Path)
            catch
                _ : Error ->
                    io:format("Ошибка при парсинге JSON config: ~p~n", [Error]),
                    {error, invalid_json}
            end;
        {error, Reason} ->
            io:format("Ошибка при чтении файла ~p~n", [Reason]),
            {error, file_read_error}
    end.