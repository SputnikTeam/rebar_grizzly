-module('45de73ed-8476-4947-be19-7200697325b2').

-export([
         get_modules_info/1,
         get_beams_list/1,
         sync_application_modules/3
        ]).

get_modules_info(Modules) ->
    lists:sort([{Module, safe_module_info(Module)} || Module <- Modules]).

safe_module_info(Module) ->
    try
        Module:module_info()
    catch _ : _ ->
            []
    end.

sync_application_modules(AppName, ForUpdate, ForDelete) ->
    DeployPath = ebin_path(AppName),

    ok = update_modules(DeployPath, ForUpdate),
    ok = delete_unused_modules(DeployPath, ForDelete).

delete_unused_modules(DeployPath, ForDelete) ->
    lists:foreach(
      fun(Module) ->
              DeletePath = beam_path(DeployPath, Module),
              ok = file:delete(DeletePath),
              code:purge(Module),
              true = code:delete(Module)
      end,
      ForDelete).

update_modules(DeployPath, ForUpdate) ->
    ok = copy_modules(DeployPath, ForUpdate),
    ok = reload_modules([element(1, ModuleEntry) || ModuleEntry <- ForUpdate]).

reload_modules(Modules) ->
    lists:foreach(fun reload_module/1, Modules).

reload_module(Module) ->
    code:purge(Module),
    {module, Module} = code:load_file(Module).

copy_modules(BasePath, Modules) ->
    lists:foreach(
      fun({Module, Binary, _Filename}) ->
              ok = file:write_file(
                     beam_path(BasePath, Module),
                     Binary)
      end,
      Modules).

get_beams_list(AppName) ->
    filelib:wildcard(
        ebin_path(AppName, [$* | code:objfile_extension()])
    ).

ebin_path(App, Path) ->
    filename:join(ebin_path(App), Path).
    
ebin_path(App) ->
    case code:lib_dir(App, ebin) of
        {error, bad_name} ->
            AppName = atom_to_binary(App, utf8),
            erlang:error({bad_name, <<"Can't resolve the ebin of application ", AppName/binary>>});
        Path ->
            Path
    end.

beam_path(DeployPath, Module) ->
    filename:join(DeployPath, atom_to_list(Module) ++ code:objfile_extension()).

