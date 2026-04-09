#!/bin/bash
#
# aws-mfa-login.sh — Automates AWS MFA session credential refresh
#
# Usage:  source ./aws-mfa-login.sh [profile]
#
# This script:
#   1. Temporarily unsets session credentials so base credentials from
#      ~/.aws/credentials are used
#   2. Auto-detects your MFA device ARN
#   3. Prompts for MFA token code
#   4. Calls aws sts get-session-token
#   5. Updates ~/.zshrc with the new session credentials
#   6. Exports them into the current shell
#
# Source the script (don't just execute it) so the exports take effect
# in your current shell session.

PROFILE="${1:-default}"
ZSHRC="$HOME/.zshrc"

echo "==> Using AWS profile: $PROFILE"

# --- Step 1: Unset existing session credentials so base creds are used ---
unset AWS_ACCESS_KEY_ID 2>/dev/null || true
unset AWS_SECRET_ACCESS_KEY 2>/dev/null || true
unset AWS_SESSION_TOKEN 2>/dev/null || true

# --- Step 2: Auto-detect MFA device ARN ---
echo "==> Detecting MFA device..."
MFA_OUTPUT=$(aws iam list-mfa-devices --profile "$PROFILE" --output json 2>&1)
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

# --- Step 3: Prompt for MFA token ---
echo -n "==> Enter MFA token code: "
read TOKEN_CODE

if [ -z "$TOKEN_CODE" ]; then
    echo "ERROR: Token code cannot be empty."
    return 1
fi

# --- Step 4: Get session token ---
echo "==> Requesting session token..."
STS_OUTPUT=$(aws sts get-session-token \
    --profile "$PROFILE" \
    --serial-number "$MFA_SERIAL" \
    --token-code "$TOKEN_CODE" \
    --duration-seconds 43200 \
    --output json 2>&1)

if [ $? -ne 0 ]; then
    echo "ERROR: STS get-session-token failed."
    echo "$STS_OUTPUT"
    return 1
fi

# --- Step 5: Extract credentials ---
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

# --- Step 6: Update ~/.zshrc ---
echo "==> Updating $ZSHRC..."

# Back up first
cp "$ZSHRC" "$ZSHRC.bak"

# Use sed to replace the credential lines in-place
sed -i '' "s|^export AWS_ACCESS_KEY_ID=.*|export AWS_ACCESS_KEY_ID=\"$NEW_ACCESS_KEY\"|" "$ZSHRC"
sed -i '' "s|^export AWS_SECRET_ACCESS_KEY=.*|export AWS_SECRET_ACCESS_KEY=\"$NEW_SECRET_KEY\"|" "$ZSHRC"
sed -i '' "s|^export AWS_SESSION_TOKEN=.*|export AWS_SESSION_TOKEN=\"$NEW_SESSION_TOKEN\"|" "$ZSHRC"

# --- Step 7: Export to current shell ---
export AWS_ACCESS_KEY_ID="$NEW_ACCESS_KEY"
export AWS_SECRET_ACCESS_KEY="$NEW_SECRET_KEY"
export AWS_SESSION_TOKEN="$NEW_SESSION_TOKEN"

echo ""
echo "==> Done! AWS MFA credentials updated."
echo "    Access Key: ${NEW_ACCESS_KEY:0:8}..."
echo "    Session valid for 12 hours."
echo "    ~/.zshrc updated (backup at ~/.zshrc.bak)"
echo "    Current shell environment updated."
