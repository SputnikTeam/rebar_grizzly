-module(grizzly).

-export(
   [
    grizzly/2
   ]).

grizzly(Config, AppFile) ->
    case rebar_app_utils:is_app_dir() of
        {true, _AppSrcFile} ->
            case grizzly_utils:read_config(Config, AppFile) of
                no_grizzly ->
                    rebar_log:log(info, "No work for grizzly bear~n", []);
                {ok, GrizzlyConfig} ->
                    AppName = proplists:get_value(app_name, GrizzlyConfig),
                    Nodes   = proplists:get_value(nodes, GrizzlyConfig),
                    Modules = proplists:get_value(modules, GrizzlyConfig),
                    ExcludeModules = proplists:get_value(exclude_modules, GrizzlyConfig),

                    rebar_log:log(info, "grizzly config available~n\tapplication: ~p~n\tnodes: ~p~n\tmodules: ~p~n",
                                  [AppName, Nodes, Modules]),
                    grizzly_utils:start_net_kernel('grizzly-node'),
                    lists:foreach(fun grizzly_utils:deploy/1, Nodes),

                    sync_modules(AppName, Nodes, Modules, ExcludeModules)
            end;
        false ->
            io:format("not app - ~p~n", [AppFile]),
            ok
    end.

sync_modules(_AppName, [], _Modules, _ExcludeModules) ->
    ok;
sync_modules(AppName, [Node | NodesTail], Modules, ExcludeModules) ->
    rebar_log:log(info, "start modules sync on '~s'~n", [Node]),

    RemoteModulesInfo = grizzly_utils:get_remote_modules_ct(Node, Modules),
    true = code:add_path(filename:absname("ebin")), %% add default rebar output path
    LocalModulesInfo = grizzly_utils:get_local_modules_ct(Modules),
    ModulesForUpdate = get_modules_for_update(LocalModulesInfo, RemoteModulesInfo),
    ModulesForDelete = get_modules_for_delete(
                         Modules,
                         Node,
                         AppName,
                         ExcludeModules
                        ),

    rebar_log:log(info, "upload task on '~s':~n\t~p ... ~n",
                  [
                   Node,
                   ModulesForUpdate
                  ]),

    io:format("sync modules on '~s'~n\tupdate modules: ~p~n\tdelete modules: ~p~n",
              [Node, ModulesForUpdate, ModulesForDelete]),
    
    ok = grizzly_utils:sync_application_modules(
           AppName,
           Node,
           ModulesForUpdate,
           ModulesForDelete
          ),

    io:format("sync successfull~n"),

    rebar_log:log(info, "sync application modules finished~n", []),

    sync_modules(AppName, NodesTail, Modules, ExcludeModules).

get_modules_for_update(LocalModulesInfo, RemoteModulesInfo) ->
    lists:foldl(
      fun({{M, CT1}, {M, CT2}}, AccIn) ->
              if
                  CT1 > CT2 ->
                      [M | AccIn];
                  true ->
                      AccIn
              end
      end,
      [],
      lists:zip(lists:usort(LocalModulesInfo), lists:usort(RemoteModulesInfo))).

get_modules_for_delete(LocalModules, Node, AppName, ExcludeModules) ->
    RemoteModules = grizzly_utils:get_beams_list(Node, AppName, ExcludeModules),
    RemoteModules -- LocalModules.
