#!/usr/bin/env bash
# Dump a known-good SigRepo database and restore it over a staging one.
#
#   scripts/reseed_staging.sh dump    /path/out.sql.gz
#   sudo bash scripts/reseed_staging.sh restore /path/in.sql.gz
#
# Staging is writable on purpose: that is most of its value, because production
# forbids adding, deleting or updating signatures, so those paths can only
# really be exercised there. The cost is that test data accumulates. This makes
# a reset cheap enough that nobody avoids testing to keep the database tidy.
#
# THE GUARD MATTERS MORE THAN THE FEATURE. This script drops a database.
# Production MySQL runs as "sigrepo-mysql" from docker-compose-vm.yml, one
# character away from the "sigrepo-local-mysql" this is for, and sigrepo.org is
# read-only for signatures by policy. So the container name is checked against
# an allow-list before anything else happens, including before the file is read.

set -uo pipefail

MODE=${1:-}
FILE=${2:-}
CONTAINER=${CONTAINER:-sigrepo-local-mysql}

die() { echo "ABORT: $*" >&2; exit 1; }

if [ -z "$MODE" ] || [ -z "$FILE" ]; then
  die "usage: reseed_staging.sh {dump|restore} <file.sql.gz>"
fi

# First, before reading the file, before touching docker: is this a target we
# are allowed to drop a database on?
case "$CONTAINER" in
  sigrepo-local-mysql|sigrepo-staging-mysql) : ;;
  *) die "refusing to operate on container '$CONTAINER'; only the local or staging MySQL may be reseeded" ;;
esac

docker inspect "$CONTAINER" > /dev/null 2>&1 || die "container $CONTAINER is not running here"

case "$MODE" in
  dump)
    docker exec "$CONTAINER" sh -c \
      'mysqldump -uroot -p"$MYSQL_ROOT_PASSWORD" --single-transaction --routines --triggers --databases sigrepo' \
      2>/dev/null | gzip -6 > "$FILE"
    [ "${PIPESTATUS[0]}" -eq 0 ] || die "mysqldump failed"
    gunzip -t "$FILE" || die "the dump that was just written is corrupt"
    echo "wrote $FILE ($(du -h "$FILE" | cut -f1))"
    ;;

  restore)
    [ -f "$FILE" ] || die "no such file: $FILE"
    # Check the dump is readable BEFORE dropping anything. Discovering a
    # corrupt archive after the drop means staging has no database at all.
    gunzip -t "$FILE" || die "$FILE is corrupt; refusing to drop a database for it"

    backup="${FILE%.sql.gz}_pre_restore_$(date +%Y%m%d_%H%M%S).sql.gz"
    echo "backing up the current database to $backup"
    docker exec "$CONTAINER" sh -c \
      'mysqldump -uroot -p"$MYSQL_ROOT_PASSWORD" --single-transaction --databases sigrepo' \
      2>/dev/null | gzip > "$backup"
    # gzip writes a perfectly valid archive when its input produced nothing, so
    # an unchecked backup can be two bytes of emptiness. This is the only copy
    # of whatever staging held; verify it before dropping anything for it.
    [ "${PIPESTATUS[0]}" -eq 0 ] || die "the pre-restore backup failed; refusing to drop anything"
    gunzip -t "$backup" || die "the pre-restore backup is corrupt; refusing to drop anything"
    [ -s "$backup" ] || die "the pre-restore backup is empty; refusing to drop anything"

    echo "dropping and restoring; this takes a couple of minutes"
    docker exec "$CONTAINER" sh -c \
      'mysql -uroot -p"$MYSQL_ROOT_PASSWORD" -e "DROP DATABASE IF EXISTS sigrepo;"' 2>/dev/null \
      || die "could not drop the database; the pre-restore backup is at $backup"

    # Capture mysql's own status, not the filter's. A grep that prints nothing
    # exits 1, which reads as a failed restore when the restore was fine: that
    # exact mistake made a successful run report "restore exit: 1" on 2026-09-24.
    #
    # The `|| true` MUST stay inside the brace group. As a trailing operator it
    # is a separate command in the AND/OR list, so `true` runs, PIPESTATUS
    # collapses to its own single element, and `set -u` then kills the script on
    # the next line -- on a restore that had just succeeded.
    gunzip -c "$FILE" | docker exec -i "$CONTAINER" sh -c 'mysql -uroot -p"$MYSQL_ROOT_PASSWORD"' 2>&1 \
      | { grep -v "Using a password" || true; }
    status=${PIPESTATUS[1]}
    [ "$status" -eq 0 ] || die "restore failed with status $status; the pre-restore backup is at $backup"

    echo "verifying"
    docker exec "$CONTAINER" sh -c 'mysql -uroot -p"$MYSQL_ROOT_PASSWORD" -t -e "
      select (select count(*) from sigrepo.signatures) signatures,
             (select count(*) from sigrepo.users) users,
             (select count(*) from sigrepo.signature_feature_set) feature_set;"' 2>/dev/null \
      || die "restore completed but the database does not answer; backup at $backup"
    echo "pre-restore backup kept at $backup"
    ;;

  *) die "unknown mode: $MODE (expected dump or restore)" ;;
esac
