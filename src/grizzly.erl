-module(grizzly).

-compile(export_all).

get_modules_info(Modules) ->
    lists:sort([{Module, (catch Module:module_info(compile))} || Module <- Modules]).

sync_application_modules(AppName, Modules) ->
    DeployPath = ebin_path(AppName),

    ok = copy_modules(DeployPath, Modules),
    ok = reload_modules([element(1, ModuleEntry) || ModuleEntry <- Modules]).

reload_modules(Modules) ->
    lists:foreach(fun reload_module/1, Modules).

reload_module(Module) ->
    code:purge(Module),
    {module, Module} = code:load_file(Module).

copy_modules(BasePath, Modules) ->
    lists:foreach(
      fun({Module, Binary, _Filename}) ->
              ok = file:write_file(
                     filename:join(BasePath, atom_to_list(Module) ++ ".beam"),
                     Binary)
      end,
      Modules).

ebin_path(AppName) ->
    case code:lib_dir(AppName, ebin) of
        {error, Reason} ->
            erlang:error(Reason);
        Path ->
            Path
    end.

