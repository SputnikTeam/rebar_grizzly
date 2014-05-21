-module('45de73ed-8476-4947-be19-7200697325b2').

-export([
         get_modules_info/1,
         get_beams_list/1,
         sync_application_modules/3
        ]).

get_modules_info(Modules) ->
    lists:sort([{Module, (catch Module:module_info(compile))} || Module <- Modules]).

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
    filelib:wildcard(filename:join(ebin_path(AppName), "*.beam")).

ebin_path(AppName) ->
    case code:lib_dir(AppName, ebin) of
        {error, Reason} ->
            erlang:error(Reason);
        Path ->
            Path
    end.

beam_path(DeployPath, Module) ->
    filename:join(DeployPath, atom_to_list(Module) ++ ".beam").

