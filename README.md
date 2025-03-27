jobctrl
=====

Simple job control app

Build
-----

    $ rebar3 compile

Concept
-----

`jobctrl` is built as erlang application with `rebar3`. It consists of following major components:

```
web-server (cowboy)
top-level supervisor   -> (jobctrl_sup.erl)
└── job supervisor     -> (job_queue.erl)
    ├── job1 ([tasks]) -> (job_stub.erl)
    ├── job2 ([tasks])
    ├── job3 ([tasks])
    └── ....
```

- web server provides REST API
- top-level supervisor spawns job supervisor
- job supervisor spawns new job every time we call `POST /jobs` REST API to create job with tasks
- job process holds tasks and performs topological sorting or task resolution to streamline task execution with dependencies

Main app module (`jobctrl_app.erl`) provides high-level api which interacts with job supervisor (`job_queue.erl`) which in controls its children (`job_stub.erl`) if requested by caller (termination, enumeration, etc.). Top-level supervisor is found inside `job_ctrl_sup.erl` module.
REST API handlers are defined iniside `job_api.erl` module. Also, there are two utility modules `graph.erl` (for sorting/graph related stuff) and `utils.erl` for misc code.

Note that, there are no unit tests unfortunately. 
Also, didn't figure out on how to debug entire thing with vscode, so there is no documentation about that.


Testing
-----

Please refer to `testing.md` inside the `./docs` directory
