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
                    rebar_log:log(debug, "grizzly config: ~p~n", [GrizzlyConfig]),
                    AppName = proplists:get_value(app_name, GrizzlyConfig),
                    Nodes   = proplists:get_value(nodes, GrizzlyConfig),
                    Modules = proplists:get_value(modules, GrizzlyConfig),
                    ExcludeModules = proplists:get_value(exclude_modules, GrizzlyConfig),
                    CompareBy = proplists:get_value(compare_by, GrizzlyConfig),

                    rebar_log:log(info, "grizzly config available~n\t\
application: ~p~n\tnodes: ~p~n\tmodules: ~p~n\tcompare_by: ~p~n",
                                  [AppName, Nodes, Modules, CompareBy]),
                    grizzly_utils:start_net_kernel('grizzly-node'),
                    lists:foreach(fun grizzly_utils:deploy/1, Nodes),

                    sync_modules(AppName, Nodes, Modules, ExcludeModules, CompareBy)
            end;
        false ->
            io:format("not app - ~p~n", [AppFile]),
            ok
    end.

sync_modules(_AppName, [], _Modules, _ExcludeModules, _CompareBy) ->
    ok;
sync_modules(AppName, [Node | NodesTail], Modules, ExcludeModules, CompareBy) ->
    rebar_log:log(info, "start modules sync on '~s'~n", [Node]),

    RemoteModulesInfo = grizzly_utils:get_remote_modules_compare_info(Node, Modules, CompareBy),
    true = code:add_path(filename:absname("ebin")), %% add default rebar output path
    LocalModulesInfo = grizzly_utils:get_local_modules_compare_info(Modules, CompareBy),
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

    sync_modules(AppName, NodesTail, Modules, ExcludeModules, CompareBy).

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
    RemoteAppModules = grizzly_utils:get_application_modules(Node, AppName),
    DeletableModules = RemoteModules -- RemoteAppModules,
    DeletableModules -- LocalModules.
