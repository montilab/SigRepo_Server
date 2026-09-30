#!/usr/bin/env bash
# How migrate.sh and schema_fingerprint.sh reach MySQL. Sourced, not run.
#
# Two ways in, because the two callers live in different places:
#
#   --container NAME   run the mysql client that is already inside the MySQL
#                      container. This is what an installed instance, the local
#                      stack, montilab and sigrepo.org use: the host needs no
#                      MySQL client, and the root password never leaves the
#                      container, which already holds it as MYSQL_ROOT_PASSWORD.
#   --host HOST        run a mysql client on this machine. This is what CI uses,
#                      where MySQL is a service container and the password comes
#                      from MYSQL_PWD in the environment.
#
# The password is never put on a command line in either mode.

CONN_CONTAINER=""
CONN_HOST=""
CONN_PORT=3306
CONN_USER=root
CONN_DB=""
CONN_SHIFT=0
# "docker" or "sudo docker"; deliberately unquoted where it is used.
DOCKER=${DOCKER:-docker}

# conn_parse_arg "$@": consume one connection option if $1 is one. Returns 0
# and sets CONN_SHIFT to the number of words used, or returns 1.
conn_parse_arg() {
  case "${1:-}" in
    --container) CONN_CONTAINER=$2; CONN_SHIFT=2 ;;
    --host)      CONN_HOST=$2;      CONN_SHIFT=2 ;;
    --port)      CONN_PORT=$2;      CONN_SHIFT=2 ;;
    --user)      CONN_USER=$2;      CONN_SHIFT=2 ;;
    --database)  CONN_DB=$2;        CONN_SHIFT=2 ;;
    *) return 1 ;;
  esac
  return 0
}

conn_require() {
  if [ -n "$CONN_CONTAINER" ] && [ -n "$CONN_HOST" ]; then
    echo "give --container or --host, not both" >&2; return 2
  fi
  if [ -z "$CONN_CONTAINER" ] && [ -z "$CONN_HOST" ]; then
    echo "give --container NAME or --host HOST" >&2; return 2
  fi
  if [ -n "$CONN_HOST" ] && [ -z "$CONN_DB" ]; then
    echo "--host needs --database" >&2; return 2
  fi
}

# conn_mysql [mysql options]: run the client against the chosen database, SQL
# on stdin. In container mode the database defaults to the container's own
# MYSQL_DATABASE.
conn_mysql() {
  if [ -n "$CONN_CONTAINER" ]; then
    $DOCKER exec -i -e SIGREPO_DB="$CONN_DB" "$CONN_CONTAINER" sh -c \
      'MYSQL_PWD="$MYSQL_ROOT_PASSWORD" exec mysql -uroot "$@" "${SIGREPO_DB:-$MYSQL_DATABASE}"' sh "$@"
  else
    mysql -h"$CONN_HOST" -P"$CONN_PORT" -u"$CONN_USER" "$@" "$CONN_DB"
  fi
}

# conn_query "SQL": one statement, tab separated rows, no header.
conn_query() {
  conn_mysql -N -B -e "$1" </dev/null
}
