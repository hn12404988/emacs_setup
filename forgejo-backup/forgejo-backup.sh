#!/bin/sh
# Forgejo nightly backup: dump (self-contained zip) -> R3S local copy + AWS S3.
# The dump already includes app.ini + forgejo-db.sql + all repos + data dir,
# so no separate bundling is needed (verified on Forgejo v15.0.3).
# R3S and S3 are independent: one destination failing does not block the other.
# S3 upload uses a dedicated IAM key that can only PutObject (no read/delete/list).
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
S3_BUCKET=forgejo-backup-020195185189-ap-east-2-an
S3_PROFILE=forgejo-backup
S3_REGION=ap-east-2

TS=$(date +%Y%m%d-%H%M)
BUNDLE="forgejo-${TS}.zip"

# 1) dump as the git user (self-contained: app.ini + forgejo-db.sql + repos + data)
install -d -o git -g git -m 700 "$STAGING"
rm -f "$STAGING"/forgejo-*.zip
"$RUNUSER" -u git -- env GITEA_WORK_DIR="$WORKDIR" \
  /usr/local/bin/forgejo dump --config "$CONF" --file "$STAGING/$BUNDLE" --type zip --tempdir "$STAGING"

LOCAL_SIZE=$(stat -c %s "$STAGING/$BUNDLE")
R3S_OK=0
S3_OK=0

# 2) push to R3S (OpenWrt has no rsync/sftp -> ssh|cat; .partial+mv = atomic write)
if $SSH "$R3S" "mkdir -p '$R3S_DIR'" \
   && $SSH "$R3S" "cat > '$R3S_DIR/$BUNDLE.partial' && mv '$R3S_DIR/$BUNDLE.partial' '$R3S_DIR/$BUNDLE'" < "$STAGING/$BUNDLE"; then
  REMOTE_SIZE=$($SSH "$R3S" "wc -c < '$R3S_DIR/$BUNDLE'" | tr -d ' ')
  if [ "$LOCAL_SIZE" = "$REMOTE_SIZE" ]; then
    R3S_OK=1
    echo "r3s=ok pushed $BUNDLE ($LOCAL_SIZE bytes) to $R3S:$R3S_DIR"
    # 3) prune on R3S (busybox find has no -delete)
    $SSH "$R3S" "find '$R3S_DIR' -name 'forgejo-*.zip' -mtime +$RETAIN_DAYS -exec rm -f {} +" \
      || echo "WARN: R3S prune failed" >&2
  else
    echo "ERROR: R3S size mismatch local=$LOCAL_SIZE remote=$REMOTE_SIZE" >&2
  fi
else
  echo "ERROR: R3S push failed" >&2
fi

# 4) upload to S3 with the PutObject-only key; verify ETag == local MD5 (single PUT, <5GB)
LOCAL_MD5=$(md5sum "$STAGING/$BUNDLE" | cut -d' ' -f1)
ETAG=$(aws s3api put-object \
  --bucket "$S3_BUCKET" \
  --key "$BUNDLE" \
  --body "$STAGING/$BUNDLE" \
  --profile "$S3_PROFILE" \
  --region "$S3_REGION" \
  --query 'ETag' \
  --output text 2>/dev/null | tr -d '"')
if [ -n "$ETAG" ] && [ "$LOCAL_MD5" = "$ETAG" ]; then
  S3_OK=1
  echo "s3=ok uploaded $BUNDLE (md5 $LOCAL_MD5) to s3://$S3_BUCKET/$BUNDLE"
else
  echo "ERROR: S3 upload failed or ETag mismatch local_md5=$LOCAL_MD5 etag=$ETAG" >&2
fi

# 5) cleanup local staging (M6 is copy #1; keep nothing here)
rm -f "$STAGING/$BUNDLE"

if [ "$R3S_OK" = 1 ] && [ "$S3_OK" = 1 ]; then
  echo "backup OK: $BUNDLE r3s=ok s3=ok"
  exit 0
fi
echo "backup FAILED: $BUNDLE r3s=$([ "$R3S_OK" = 1 ] && echo ok || echo fail) s3=$([ "$S3_OK" = 1 ] && echo ok || echo fail)" >&2
exit 1
