#!/usr/bin/env sh
export DOCKER_BUILDKIT=1
# export BUILDKIT_PROGRESS=plain  # -- useful for debugging
export COMPOSE_DOCKER_CLI_BUILD=1

docker_folder="$(cd "$(dirname "$(realpath $0)")" && pwd)"
compose="$docker_folder/compose.yml"
{
  cd "$(dirname "$docker_folder")";
  docker-compose --log-level DEBUG -f "$compose" up -d;
  docker-compose -f "$compose" logs -f -t
}
