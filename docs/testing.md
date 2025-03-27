# Running & testing app

## General flow

To make sense of all API calls, first we need to upload our job (in form of json):

1. Create/submit job -> `POST /jobs`

    We are going to put definition inside body, so:

    ```
    {
      "tasks": [
          {
              "name": "task-1",
              "command": "touch /tmp/file1"
          },
          {
              "name": "task-2",
              "command": "cat /tmp/file1",
              "requires": [
                  "task-3"
              ]
          },
          {
              "name": "task-3",
              "command": "echo 'Hello World!' > /tmp/file1",
              "requires": [
                  "task-1"
              ]
          },
          {
              "name": "task-4",
              "command": "rm /tmp/file1",
              "requires": [
                  "task-2",
                  "task-3"
              ]
          }
      ]
    }
    ```

    If created, it will return `job_id`, like this:
    ```
    {
      "job_id": "3ab0bad9-b174-49e1-b009-a2540875cdc7"
    }
    ```
2. We can get check our job to see all the tasks -> `jobs/:job_id`

    We should see similar json as in previous step:
    ```
    {
      "id": "3ab0bad9-b174-49e1-b009-a2540875cdc7",
      "tasks": [
        {
          "command": "touch /tmp/file1",
          "name": "task-1"
        },
        {
          "command": "echo 'Hello World!' > /tmp/file1",
          "name": "task-3"
        },
        {
          "command": "cat /tmp/file1",
          "name": "task-2"
        },
        {
          "command": "rm /tmp/file1",
          "name": "task-4"
        }
      ]
    }
    ```
    > Notice that all the tasks are resolved and put into correct execution order - job process was responsible for this and it topo0-sorts them as soon as they are uploaded. (you can find more about that inside root `README.md`).

3. We can now generate shell script based on this definition - `GET /jobs/:job_id/dump`

    ```
    #!/usr/bin/env bash
    rm /tmp/file1
    cat /tmp/file1
    echo 'Hello World!' > /tmp/file1
    touch /tmp/file1
    ```
4. If we want to resolve tasks again (although they are automatically resolved/sorted upon job creation), we can use -> `POST /jobs/:job_id/schedule`
5. To see all job ids currently in the system -> `GET /jobs`

    ```
    [
      "3ab0bad9-b174-49e1-b009-a2540875cdc7",
      ...
    ]
    ```

## Running

Simplest way to run application is to boot it within `erl` shell:

- `$ rebar3 update`
- `$ rebar3 get-deps`
- `$ rebar3 compile`
- `$ rebar3 shell`

Configuration can be found inside `{root}/config/{dev,test}` per `dev` and `test` profiles respectively.

Although I've included releas creation using `relx`, I didn't quite test it, but anyway, one can create `dev` release for instance like this:

1. assemble release `$ rebar3 as dev release`
2. boot app  `/_build/dev/rel/jobctrl/bin/jobctrl [console|foreground|...]`

## REST API

In the `api` dir, you can find REST api collection (I used bruno - https://www.usebruno.com/) for testing entire application.

Here is a quick summary of modeled API calls (please refer to collection for specific inputs, requests etc.):

- `GET /jobs` - returns all registered jobs
- `POST /jobs` - creates/submits new job (`json` definition)
- `GET /jobs/:job_id/dump` - returns shell script representation of a job
- `GET /jobs/:job_id/query` - returns tasks inside the given job
- `POST /jobs/:job_id/schedule` - returns resolved/topologically sorted tasks inside the job (given that dependencies can be resolved)

For more detailed semantics/logic breakdown, please refer to README.md in the root of this project.