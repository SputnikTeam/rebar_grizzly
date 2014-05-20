# Rebar hotsync plugin

## Installation

To install this plugin, you need to add it as a dependency to your rebar application:

```erlang
{deps, [
    {rebar_hotsync_plugin, ".*", {git, "https://github.com/tank-bohr/rebar_hotsync_plugin.git"}}
]}.
```

To let rebar know about the new plugin add the folowing line to your `rebar.config`

```erlang
{plugins, [rebar_hotsync_plugin] }.
```