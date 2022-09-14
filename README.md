Taskwarrior-Habitica Bridge
==========

Sync Taskwarrior tasks with Habitica (and _vice versa_).

## What It Does

- Add tasks added through Taskwarrior (e.g. `task add`) to Habitica
- Modify tasks modified through Taskwarrior (e.g. `task modify`) on Habitica
- Bidirectionally synchronize tasks between Taskwarrior and Habitica

## Installation and Setup

### Dependencies

- [Nix](https://nixos.org/nix/)

### Installing the program

1. If you don't have the Nix package manager installed, install it now: `curl https://nixos.org/nix/install | sh`
2. Install via the setup script: `curl https://raw.githubusercontent.com/robwhitaker/taskwarrior-habitica-bridge/master/setup.sh | sh`

### Installing the hooks

1. Copy all the files in the `hooks/` directory into your Taskwarrior hooks folder (`.task/hooks/` by default).

### Adding Habitica UDAs to Taskwarrior

Some UDAs are needed to represent Habitica types. Add the following UDAs to your `.taskrc` file:

```
uda.habitica_uuid.label=Habitica UUID
uda.habitica_uuid.type=string

uda.habitica_difficulty.label=Habitica Difficulty
uda.habitica_difficulty.type=string
uda.habitica_difficulty.values=trivial,easy,medium,hard

uda.habitica_task_type.label=Habitica Task Type
uda.habitica_task_type.type=string
uda.habitica_task_type.values=daily,todo
```

### Providing Habitica credentials

In order to interact with your tasks, Taskwarrior-Habitica Bridge needs your Habitica user ID and API key. You must provide them in your `.taskrc` file (or some included rc file) as follows:

```
habitica.user_id=YOUR_USER_ID
habitica.api_key=YOUR_API_KEY
```

## Usage

When editing tasks in Taskwarrior, the hook scripts will automatically update them on Habitica so you can use Taskwarrior as per usual.

`task2habitica sync` will synchronize tasks between Taskwarrior and Habitica in the case things don't sync automatically (e.g. you've added a task on Habitica you want to pull into Taskwarrior).

### A note on recurring tasks in Taskwarrior

Taskwarrior-Habitica Bridge supports syncing instances of recurring Taskwarrior tasks. However, the first time you create a recurring task in Taskwarrior, the `on-add` hook doesn't trigger. This means you will need to run `task2habitica sync` to get that first instance onto Habitica. After that, things should work as expected.

### Other hook scripts

While `task2habitica sync` runs, it runs various task commands which will trigger hooks. An environment variable, `TASK2HABITICA_RUNNING`, is set (to `1`) while the `sync` command runs so that you may check if a sync is occurring and possibly skip the processing of certain hook scripts if needed.

## Not Supported
- Syncing tags
- Syncing description (Taskwarrior has no notion of a description in the Habitica sense)
