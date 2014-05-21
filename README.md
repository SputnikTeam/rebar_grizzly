# Rebar grizzly plugin

Rebar plugin for erlang hot code upgrade. Synchronizes (reload and _save_) beam files on remote nodes with your application's `ebin` dir.

Grizzly uses only long names for rpc.

## Installation

To install grizzly, you need to add it as a dependency to your rebar config:

```erlang
{deps, [
    {rebar_grizzly, ".*", {git, "https://github.com/SputnikTeam/rebar_grizzly.git"}}
]}.
```

To let rebar know about the new plugin add the folowing line to your `rebar.config`

```erlang
{plugins, [grizzly] }.
```

## Configuration

```erlang
{grizzly, [
    {apply_for_apps, [your_app, another_app]},
    {nodes, [
        bear@some-host-1.blah.local,
        bear@some-host-2.blah.local,
        bear@some-host-3.blah.local,
        bear@some-host-4.blah.local
    ]}
]}.
```

## Usage

`rebar grizzly`


## License

a kind of MIT
