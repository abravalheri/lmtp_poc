#!/usr/bin/env bash
project_folder="$(cd "$(cd "$(dirname "$(realpath $0)")")" && pwd)"
compose="$project_folder/docker/compose.yml"

existing_email="postmaster@localhost"
unknown_email="unknown@localhost"

start_containers() {
  docker-compose -f "$compose" build;
  docker-compose -f "$compose" up -d
}

stop_containers() {
  docker-compose -f "$compose" down
}

count_entries() {
  docker-compose -f "$compose" exec -T lmtp ls -l "$1" | wc -l
}

send_mail() {
  docker-compose -f "$compose" exec -T smtp sendmail -f sender@mail "$1"
}

test_send_success() {
  before="$(count_entries "$existing_email")"
  echo "Subject: hello" | send_mail "$existing_email"
  after="$(count_entries "$existing_email")"
  echo -ne "   --- ${FUNCNAME[0]}  \t"
  if ((after = before + 1)); then
    echo "ok"
  else
    echo "fail - no email was received"
  fi
}

test_send_error() {
  before="$(count_entries .)"
  error="$(echo "Subject: hello" | send_mail "$unknown_email" 2>&1)"
  after="$(count_entries .)"
  echo -ne "   --- ${FUNCNAME[0]}  \t"
  if ((after > before)); then
    echo "fail - more entries than expected after sending to an unknown recipient"
  elif [[ "$error" == *"550 Invalid recipient"* ]]; then
    echo "ok"
  else
    echo "fail - expected to see 'Invalid recipient' as error message, but not received"
  fi
}

echo "Running system tests with the help of docker"
{
  cd "$project_folder";
  start_containers;
  test_send_success;
  test_send_error;
  stop_containers
}
