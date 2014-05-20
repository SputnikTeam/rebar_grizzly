-module(grizzly_utils).

-compile(export_all).

-record(cfg, {
    app_name,
    nodes,
    modules
}).

-define(GRIZZLY_MODULE, grizzly).

read_config(ConfigOriginal, AppSrcFile) ->
    rebar_log:log(debug, "AppSrcFile: ~p~n", [AppSrcFile]),
    case rebar_config:get(ConfigOriginal, grizzly, no_grizzly) of
        no_grizzly ->
            no_grizzly;
        GrizzlyOptions ->
            rebar_log:log(debug, "GrizzlyOptions: ~p~n", [GrizzlyOptions]),
            AppFile = rebar_app_utils:app_src_to_app(AppSrcFile),
            {Config, AppName}  = rebar_app_utils:app_name(ConfigOriginal, AppFile),
            XconfKey = {appfile, {app_file, AppFile}},
            {AppName, AppData} = rebar_config:get_xconf(Config, XconfKey),
            rebar_log:log(debug, "AppData: ~p~n", [AppData]),

            Modules = proplists:get_value(modules, AppData, []),
            Nodes = proplists:get_value(nodes, GrizzlyOptions, []),

            {ok, #cfg{
                app_name = AppName,
                nodes = Nodes,
                modules = Modules
            }}
    end.

deploy(NodeName) ->
    rebar_log:log(info, "deploy: ~s~n", [NodeName]),
    case check_grizzly_available(NodeName) of
        ok ->
            rebar_log:log(info, "~s - already loaded~n", [NodeName]);
        not_available ->
            rebar_log:log(info, "~s - need deploy~n", [NodeName]),
            deploy_module(NodeName, ?GRIZZLY_MODULE)
    end.

deploy_module(NodeName, Module) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    {module, Module} = rpc_call(NodeName, code, load_binary, [Module, Filename, Binary]),
    rebar_log:log(info, "~s - loaded~n", [NodeName]).

grizzly_app(#cfg{app_name = AppName}) ->
    AppName.

grizzly_nodes(#cfg{nodes = Nodes}) ->
    Nodes.

grizzly_modules(#cfg{modules = Modules}) ->
    Modules.

check_grizzly_available(Node) ->
    rebar_log:log(info, "grizzly deploy name: ~s~n", [?GRIZZLY_MODULE]),

    case rpc_call(Node, code, ensure_loaded, [?GRIZZLY_MODULE]) of
        {module, _} ->
            ok;
        {error, _} ->
            not_available
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
    rpc_call(Node, M, F, A, 1000).

rpc_call(Node, M, F, A, Timeout) ->
    case rpc:call(Node, M, F, A, Timeout) of
        {badrpc, Reason} ->
            rebar_log:error("grizzly badrpc: ~p~n", [Reason]),
            erlang:error(Reason);
        Result ->
            Result
    end.

