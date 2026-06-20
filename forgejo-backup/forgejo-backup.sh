#!/bin/sh
# Forgejo nightly backup: dump (self-contained zip) -> R3S local copy.
# The dump already includes app.ini + forgejo-db.sql + all repos + data dir,
# so no separate bundling is needed (verified on Forgejo v15.0.3).
# Design:  plans/forgejo_backup_r3s.md
# Runbook: plans/forgejo_backup_r3s_runbook.md
set -eu

CONF=/etc/forgejo/app.ini
WORKDIR=/var/lib/forgejo
STAGING=/var/lib/forgejo/backups
R3S=root@192.168.1.1
R3S_KEY=/root/.ssh/id_forgejo_backup
R3S_DIR=/backup/forgejo
RETAIN_DAYS=30
RUNUSER=/usr/sbin/runuser
SSH="ssh -i ${R3S_KEY} -o BatchMode=yes -o ConnectTimeout=10"

TS=$(date +%Y%m%d-%H%M)
BUNDLE="forgejo-${TS}.zip"

# 1) dump as the git user (self-contained: app.ini + forgejo-db.sql + repos + data)
install -d -o git -g git -m 700 "$STAGING"
rm -f "$STAGING"/forgejo-*.zip
"$RUNUSER" -u git -- env GITEA_WORK_DIR="$WORKDIR" \
  /usr/local/bin/forgejo dump --config "$CONF" --file "$STAGING/$BUNDLE" --type zip --tempdir "$STAGING"

# 2) push to R3S (OpenWrt has no rsync/sftp -> ssh|cat; .partial+mv = atomic write)
$SSH "$R3S" "mkdir -p '$R3S_DIR'"
$SSH "$R3S" "cat > '$R3S_DIR/$BUNDLE.partial' && mv '$R3S_DIR/$BUNDLE.partial' '$R3S_DIR/$BUNDLE'" < "$STAGING/$BUNDLE"

# 3) verify remote size == local size (catch a truncated transfer)
LOCAL_SIZE=$(stat -c %s "$STAGING/$BUNDLE")
REMOTE_SIZE=$($SSH "$R3S" "wc -c < '$R3S_DIR/$BUNDLE'" | tr -d ' ')
if [ "$LOCAL_SIZE" != "$REMOTE_SIZE" ]; then
  echo "ERROR: size mismatch local=$LOCAL_SIZE remote=$REMOTE_SIZE" >&2
  exit 1
fi
echo "pushed $BUNDLE ($LOCAL_SIZE bytes) to $R3S:$R3S_DIR"

# 4) prune on R3S (busybox find has no -delete)
$SSH "$R3S" "find '$R3S_DIR' -name 'forgejo-*.zip' -mtime +$RETAIN_DAYS -exec rm -f {} +"

# 5) cleanup local staging (M6 is copy #1; keep nothing here)
rm -f "$STAGING/$BUNDLE"
echo "backup OK: $BUNDLE"
