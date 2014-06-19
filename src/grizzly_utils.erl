-module(grizzly_utils).

-export([
         read_config/2,
         start_net_kernel/1,
         deploy/1,
         get_remote_modules_ct/2,
         get_local_modules_ct/1,
         sync_application_modules/4,
         get_beams_list/3,
         get_application_modules/2
        ]).

-define(DEFAULT_TIMEOUT, 1000).
-define(GRIZZLY_MODULE, '45de73ed-8476-4947-be19-7200697325b2').

read_config(ConfigOriginal, AppSrcFile) ->
    rebar_log:log(debug, "AppSrcFile: ~p~n", [AppSrcFile]),
    case rebar_config:get(ConfigOriginal, grizzly, no_grizzly) of
        no_grizzly ->
            no_grizzly;
        GrizzlyOptions ->
            rebar_log:log(debug, "GrizzlyOptions: ~p~n", [GrizzlyOptions]),
            AppFile = rebar_app_utils:app_src_to_app(AppSrcFile),
            {Config, AppName} = rebar_app_utils:app_name(ConfigOriginal, AppFile),
            XconfKey = {appfile, {app_file, AppFile}},
            {AppName, AppData} = rebar_config:get_xconf(Config, XconfKey),
            rebar_log:log(debug, "AppData: ~p~n", [AppData]),

            case lists:member(AppName, proplists:get_value(apply_for_apps, GrizzlyOptions)) of
                true ->
                    rebar_log:log(info, "grizzly starts working with ~s application~n", [AppName]),
                    ExcludeModules = proplists:get_value(exclude_modules, GrizzlyOptions, []),
                    Modules = proplists:get_value(modules, AppData, []) -- ExcludeModules,
                    Nodes = proplists:get_value(nodes, GrizzlyOptions, []),

                    {ok, [
                          {app_name, AppName},
                          {nodes, Nodes},
                          {modules, Modules},
                          {exclude_modules, ExcludeModules}
                         ]};
                false ->
                    rebar_log:log(info, "grizzly doesn't eat ~s application~n", [AppName]),
                    no_grizzly
            end
    end.

deploy(NodeName) ->
    rebar_log:log(info, "deploy grizzly to ~s~n", [NodeName]),
    case check_grizzly_available(NodeName) of
        ok ->
            rebar_log:log(info, "~s - already loaded~n", [NodeName]);
        not_available ->
            rebar_log:log(info, "~s - need deploy~n", [NodeName]),
            deploy_module(NodeName, ?GRIZZLY_MODULE)
    end.

deploy_module(NodeName, Module) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    rpc_call(NodeName, code, purge, [Module]),
    {module, Module} = rpc_call(NodeName, code, load_binary, [Module, Filename, Binary]),
    rebar_log:log(info, "~s - loaded~n", [NodeName]).

get_remote_modules_ct(Node, Modules) ->
    extract_time(rpc_call(Node, ?GRIZZLY_MODULE, get_modules_info, [Modules])).

get_beams_list(Node, AppName, ExcludeModules) ->
    BeamFiles = rpc_call(Node, ?GRIZZLY_MODULE, get_beams_list, [AppName]),
    [list_to_atom(filename:basename(File, code:objfile_extension())) || File <- BeamFiles] -- ExcludeModules.

get_application_modules(Node, AppName) ->
    try
        rpc_call(Node, application, get_key, [AppName, modules])
    of
        {ok, AppFileModules} ->
            AppFileModules
    catch _ : _ ->
            [] %% if app not started delete any modules
    end.

get_local_modules_ct(Modules) ->
    extract_time(?GRIZZLY_MODULE:get_modules_info(Modules)).

extract_time(Info) ->
    lists:map(
      fun({M, CompileInfo}) ->
              {M, proplists:get_value(time, CompileInfo)}
      end,
      Info).

sync_application_modules(AppName, Node, ForUpdate, ForDelete) ->
    ModulesCode = [code:get_object_code(Module) || Module <- ForUpdate],
    ok = rpc_call(Node, ?GRIZZLY_MODULE, sync_application_modules,
                  [AppName, ModulesCode, ForDelete]).

check_grizzly_available(Node) ->
    rebar_log:log(debug, "grizzly deploy name: ~s~n", [?GRIZZLY_MODULE]),

    case rpc:call(Node, ?GRIZZLY_MODULE, module_info, [compile], ?DEFAULT_TIMEOUT) of
        {badrpc, timeout} ->
            rebar_log:log(error, "grizzly rpc timeout~n"),
            erlang:error(timeout);
        {badrpc, _} ->
            not_available;
        RemoteModuleInfo ->
            case ?GRIZZLY_MODULE:module_info(compile) of
                RemoteModuleInfo ->
                    ok;
                _ ->
                    not_available
            end
    end.

start_net_kernel(NodeName) ->
    case net_kernel:start([NodeName, longnames]) of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok;
        {error, Reason} -> 
            rebar_log:log(error, "net_kernel start error: ~p~n", [Reason]),
            erlang:error({start_net_kernel, Reason})
    end.

rpc_call(Node, M, F, A) ->
    rpc_call(Node, M, F, A, ?DEFAULT_TIMEOUT).

rpc_call(Node, M, F, A, Timeout) ->
    case rpc:call(Node, M, F, A, Timeout) of
        {badrpc, Reason} ->
            rebar_log:log(error, "grizzly badrpc: ~p~n", [Reason]),
            erlang:error(Reason);
        Result ->
            Result
    end.

