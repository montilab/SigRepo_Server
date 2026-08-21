# Frequently asked questions

## How do I get help?

**SigRepo_Server** is under active development. If you need help, would
like to contribute, or simply want to talk about the project with
like-minded individuals, we have a number of open channels for
communication.

- Send us an email at <sigrepo@bu.edu>

- To report bugs or file feature requests, use the [issue tracker on
  Github](https://github.com/montilab/SigRepo_Server/issues).

- To contribute code, submit a [pull request on
  Github](https://github.com/montilab/SigRepo_Server/pulls).

## How to verify all containers are built and run successfully

``` bash
docker compose ps


CONTAINER ID   IMAGE                           COMMAND                  CREATED          STATUS          PORTS                               NAMES
9210e8d30506   montilab/sigrepo:latest         "/bin/bash -c /SigRe…"   2 minutes ago    Up 2 minutes    0.0.0.0:8050->3838/tcp              sigrepo-shiny
57076db95606   montilab/sigrepo:latest         "/bin/bash -c /SigRe…"   2 minutes ago    Up 2 minutes    0.0.0.0:8020->3838/tcp              sigrepo-api
77ca9528cb3b   montilab/sigrepo-mysql:latest   "docker-entrypoint.s…"   26 minutes ago   Up 14 minutes   0.0.0.0:3306->3306/tcp, 33060/tcp   sigrepo-mysql
```

## Docker Compose operations

| Command | Description |
|----|----|
| [docker compose build](https://docs.docker.com/engine/reference/commandline/compose_build/) | Build or rebuild services |
| [docker compose config](https://docs.docker.com/engine/reference/commandline/compose_config/) | Parse, resolve and render compose file in canonical format |
| [docker compose cp](https://docs.docker.com/engine/reference/commandline/compose_cp/) | Copy files/folders between a service container and the local filesystem |
| [docker compose create](https://docs.docker.com/engine/reference/commandline/compose_create/) | Creates containers for a service. |
| [docker compose down](https://docs.docker.com/engine/reference/commandline/compose_down/) | Stop and remove containers, networks |
| [docker compose events](https://docs.docker.com/engine/reference/commandline/compose_events/) | Receive real time events from containers. |
| [docker compose exec](https://docs.docker.com/engine/reference/commandline/compose_exec/) | Execute a command in a running container. |
| [docker compose images](https://docs.docker.com/engine/reference/commandline/compose_images/) | List images used by the created containers |
| [docker compose kill](https://docs.docker.com/engine/reference/commandline/compose_kill/) | Force stop service containers. |
| [docker compose logs](https://docs.docker.com/engine/reference/commandline/compose_logs/) | View output from containers |
| [docker compose ls](https://docs.docker.com/engine/reference/commandline/compose_ls/) | List running compose projects |
| [docker compose pause](https://docs.docker.com/engine/reference/commandline/compose_pause/) | Pause services |
| [docker compose port](https://docs.docker.com/engine/reference/commandline/compose_port/) | Print the public port for a port binding. |
| [docker compose ps](https://docs.docker.com/engine/reference/commandline/compose_ps/) | List containers |
| [docker compose pull](https://docs.docker.com/engine/reference/commandline/compose_pull/) | Pull service images |
| [docker compose push](https://docs.docker.com/engine/reference/commandline/compose_push/) | Push service images |
| [docker compose restart](https://docs.docker.com/engine/reference/commandline/compose_restart/) | Restart service containers |
| [docker compose rm](https://docs.docker.com/engine/reference/commandline/compose_rm/) | Removes stopped service containers |
| [docker compose run](https://docs.docker.com/engine/reference/commandline/compose_run/) | Run a one-off command on a service. |
| [docker compose start](https://docs.docker.com/engine/reference/commandline/compose_start/) | Start services |
| [docker compose stop](https://docs.docker.com/engine/reference/commandline/compose_stop/) | Stop services |
| [docker compose top](https://docs.docker.com/engine/reference/commandline/compose_top/) | Display the running processes |
| [docker compose unpause](https://docs.docker.com/engine/reference/commandline/compose_unpause/) | Unpause services |
| [docker compose up](https://docs.docker.com/engine/reference/commandline/compose_up/) | Create and start containers |
| [docker compose version](https://docs.docker.com/engine/reference/commandline/compose_version/) | Show the Docker Compose version information |
