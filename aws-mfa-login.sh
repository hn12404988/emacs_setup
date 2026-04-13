#!/bin/bash
#
# aws-mfa-login.sh — Automates AWS MFA session credential refresh
#
# Usage:  source ./aws-mfa-login.sh [profile]
#
# This script:
#   1. Temporarily unsets session credentials so base credentials are used
#   2. Auto-detects your MFA device ARN (uses [profile-base] if available)
#   3. Prompts for MFA token code
#   4. Calls aws sts get-session-token
#   5. Backs up base credentials to [profile-base] in ~/.aws/credentials
#   6. Updates ~/.aws/credentials profile with session credentials
#   7. Updates shell rc file (~/.zshrc on macOS, ~/.bashrc on Linux)
#   8. Exports them into the current shell
#
# Source the script (don't just execute it) so the exports take effect
# in your current shell session.

PROFILE="${1:-default}"

# --- Auto-detect shell rc file based on OS ---
if [[ "$(uname)" == "Darwin" ]]; then
    RC_FILE="$HOME/.zshrc"
else
    RC_FILE="$HOME/.bashrc"
fi

AWS_CREDS_FILE="$HOME/.aws/credentials"

echo "==> Using AWS profile: $PROFILE"
echo "    Shell rc file: $RC_FILE"

# --- Step 1: Unset existing session credentials so base creds are used ---
unset AWS_ACCESS_KEY_ID 2>/dev/null || true
unset AWS_SECRET_ACCESS_KEY 2>/dev/null || true
unset AWS_SESSION_TOKEN 2>/dev/null || true

# --- Step 2: Determine auth profile (use base backup if available) ---
BASE_PROFILE="${PROFILE}-base"
BASE_KEY=$(aws configure get aws_access_key_id --profile "$BASE_PROFILE" 2>/dev/null)
if [ -n "$BASE_KEY" ]; then
    echo "==> Using saved base credentials from [$BASE_PROFILE]"
    AUTH_PROFILE="$BASE_PROFILE"
else
    AUTH_PROFILE="$PROFILE"
fi

# --- Step 3: Auto-detect MFA device ARN ---
echo "==> Detecting MFA device..."
MFA_OUTPUT=$(aws iam list-mfa-devices --profile "$AUTH_PROFILE" --output json 2>&1)
if [ $? -ne 0 ]; then
    echo "ERROR: Failed to list MFA devices. Check your base credentials in ~/.aws/credentials"
    echo "$MFA_OUTPUT"
    return 1
fi

MFA_SERIAL=$(python3 -c "
import json, sys
data = json.loads(sys.stdin.read())
devices = data.get('MFADevices', [])
if not devices:
    print('ERROR: No MFA devices found', file=sys.stderr)
    sys.exit(1)
print(devices[0]['SerialNumber'])
" <<< "$MFA_OUTPUT")

if [ $? -ne 0 ]; then
    echo "ERROR: Could not detect MFA device."
    return 1
fi

echo "    MFA device: $MFA_SERIAL"

# --- Step 4: Prompt for MFA token ---
echo -n "==> Enter MFA token code: "
read TOKEN_CODE

if [ -z "$TOKEN_CODE" ]; then
    echo "ERROR: Token code cannot be empty."
    return 1
fi

# --- Step 5: Get session token ---
echo "==> Requesting session token..."
STS_OUTPUT=$(aws sts get-session-token \
    --profile "$AUTH_PROFILE" \
    --serial-number "$MFA_SERIAL" \
    --token-code "$TOKEN_CODE" \
    --duration-seconds 43200 \
    --output json 2>&1)

if [ $? -ne 0 ]; then
    echo "ERROR: STS get-session-token failed."
    echo "$STS_OUTPUT"
    return 1
fi

# --- Step 6: Extract credentials ---
read -r NEW_ACCESS_KEY NEW_SECRET_KEY NEW_SESSION_TOKEN <<< $(python3 -c "
import json, sys
data = json.loads(sys.stdin.read())
creds = data['Credentials']
print(creds['AccessKeyId'], creds['SecretAccessKey'], creds['SessionToken'])
" <<< "$STS_OUTPUT")

if [ -z "$NEW_ACCESS_KEY" ]; then
    echo "ERROR: Failed to parse credentials from STS response."
    return 1
fi

# --- Step 7: Back up base credentials (only once, skip if already backed up) ---
if [ -z "$BASE_KEY" ]; then
    echo "==> Backing up base credentials to [$BASE_PROFILE]..."
    ORIG_KEY=$(aws configure get aws_access_key_id --profile "$PROFILE" 2>/dev/null)
    ORIG_SECRET=$(aws configure get aws_secret_access_key --profile "$PROFILE" 2>/dev/null)
    if [ -n "$ORIG_KEY" ]; then
        aws configure set aws_access_key_id "$ORIG_KEY" --profile "$BASE_PROFILE"
        aws configure set aws_secret_access_key "$ORIG_SECRET" --profile "$BASE_PROFILE"
    fi
fi

# --- Step 8: Update ~/.aws/credentials ---
echo "==> Updating $AWS_CREDS_FILE [$PROFILE]..."
aws configure set aws_access_key_id "$NEW_ACCESS_KEY" --profile "$PROFILE"
aws configure set aws_secret_access_key "$NEW_SECRET_KEY" --profile "$PROFILE"
aws configure set aws_session_token "$NEW_SESSION_TOKEN" --profile "$PROFILE"

# --- Step 9: Update shell rc file ---
echo "==> Updating $RC_FILE..."

# Back up first
cp "$RC_FILE" "$RC_FILE.bak"

# Detect sed in-place syntax (macOS vs Linux)
if [[ "$(uname)" == "Darwin" ]]; then
    sed -i '' "s|^export AWS_ACCESS_KEY_ID=.*|export AWS_ACCESS_KEY_ID=\"$NEW_ACCESS_KEY\"|" "$RC_FILE"
    sed -i '' "s|^export AWS_SECRET_ACCESS_KEY=.*|export AWS_SECRET_ACCESS_KEY=\"$NEW_SECRET_KEY\"|" "$RC_FILE"
    sed -i '' "s|^export AWS_SESSION_TOKEN=.*|export AWS_SESSION_TOKEN=\"$NEW_SESSION_TOKEN\"|" "$RC_FILE"
else
    sed -i "s|^export AWS_ACCESS_KEY_ID=.*|export AWS_ACCESS_KEY_ID=\"$NEW_ACCESS_KEY\"|" "$RC_FILE"
    sed -i "s|^export AWS_SECRET_ACCESS_KEY=.*|export AWS_SECRET_ACCESS_KEY=\"$NEW_SECRET_KEY\"|" "$RC_FILE"
    sed -i "s|^export AWS_SESSION_TOKEN=.*|export AWS_SESSION_TOKEN=\"$NEW_SESSION_TOKEN\"|" "$RC_FILE"
fi

# --- Step 10: Export to current shell ---
export AWS_ACCESS_KEY_ID="$NEW_ACCESS_KEY"
export AWS_SECRET_ACCESS_KEY="$NEW_SECRET_KEY"
export AWS_SESSION_TOKEN="$NEW_SESSION_TOKEN"

echo ""
echo "==> Done! AWS MFA credentials updated."
echo "    Access Key: ${NEW_ACCESS_KEY:0:8}..."
echo "    Session valid for 12 hours."
echo "    $RC_FILE updated (backup at $RC_FILE.bak)"
echo "    $AWS_CREDS_FILE [$PROFILE] updated (base creds saved in [$BASE_PROFILE])"
echo "    Current shell environment updated."
