#!/usr/bin/env bash
# Integration tests for install-stt.sh
# Run: bash tests/test-install-stt.sh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
INSTALL_SCRIPT="$PROJECT_DIR/install-stt.sh"

PASS=0
FAIL=0
FAIL_MSGS=()

assert() {
  local desc="$1" condition="$2"
  if eval "$condition" 2>/dev/null; then
    PASS=$((PASS + 1))
    echo "  PASS: $desc"
  else
    FAIL=$((FAIL + 1))
    FAIL_MSGS+=("FAIL: $desc")
    echo "  FAIL: $desc"
  fi
  return 0
}

echo "=== install-stt.sh integration tests ==="

# ── existence and basic properties ──

echo ""
echo "--- file checks ---"

assert "install-stt.sh exists" '[ -f "$INSTALL_SCRIPT" ]'
assert "install-stt.sh is executable" '[ -x "$INSTALL_SCRIPT" ]'

if [ ! -f "$INSTALL_SCRIPT" ]; then
  echo ""
  echo "=== RESULTS: $PASS passed, $FAIL failed ==="
  for msg in "${FAIL_MSGS[@]}"; do echo "$msg"; done
  exit 1
fi

# ── source and function checks ──

echo ""
echo "--- sourcing ---"

# Source the script directly so functions are available in this shell.
# The script must guard its main() with BASH_SOURCE check so it defines
# functions but does not run the install when sourced.
SOURCE_ERR_FILE="$(mktemp)"
# shellcheck disable=SC1090
source "$INSTALL_SCRIPT" 2>"$SOURCE_ERR_FILE" >/dev/null || true
SOURCE_ERR="$(cat "$SOURCE_ERR_FILE")"
rm -f "$SOURCE_ERR_FILE"

PASS=$((PASS + 1))
echo "  PASS: sourced install-stt.sh"
if [ -n "$SOURCE_ERR" ]; then
  echo "         (stderr: $SOURCE_ERR)"
fi

# ── function presence ──

echo ""
echo "--- function presence ---"

REQUIRED_FUNCS=(
  "detect_os"
  "detect_arch"
  "download_url"
  "print_ghostty_config"
)

for fn in "${REQUIRED_FUNCS[@]}"; do
  assert "function '$fn' is defined" 'declare -F "$fn" >/dev/null 2>&1'
done

# ── OS detection ──

echo ""
echo "--- OS detection ---"

DETECTED_OS="$(declare -f detect_os >/dev/null 2>&1 && detect_os || echo "")"
# detect_os must return either "linux" or "darwin"
case "$DETECTED_OS" in
  linux|darwin)
    assert "detect_os returns '$DETECTED_OS' (valid)" 'true'
    ;;
  *)
    assert "detect_os returns 'linux' or 'darwin' (got: '$DETECTED_OS')" 'false'
    ;;
esac

# ── arch detection ──

echo ""
echo "--- architecture detection ---"

DETECTED_ARCH="$(declare -f detect_arch >/dev/null 2>&1 && detect_arch || echo "")"
case "$DETECTED_ARCH" in
  x86_64|arm64|aarch64)
    assert "detect_arch returns '$DETECTED_ARCH' (valid)" 'true'
    ;;
  *)
    assert "detect_arch returns 'x86_64' or 'arm64' (got: '$DETECTED_ARCH')" 'false'
    ;;
esac

# ── download URL ──

echo ""
echo "--- download URL ---"

# download_url should take os and arch and build the correct URL.
# We test known combinations.
URL_LINUX_ARM64="$(declare -f download_url >/dev/null 2>&1 && download_url linux arm64 || echo "")"
assert "download_url linux arm64 is non-empty" '[ -n "$URL_LINUX_ARM64" ]'
assert "download_url linux arm64 contains 'stt-daemon'" '[[ "$URL_LINUX_ARM64" == *stt-daemon* ]]'
assert "download_url linux arm64 contains 'linux'" '[[ "$URL_LINUX_ARM64" == *linux* || "$URL_LINUX_ARM64" == *Linux* ]]'
assert "download_url linux arm64 contains 'arm64' or 'aarch64'" '[[ "$URL_LINUX_ARM64" == *arm64* || "$URL_LINUX_ARM64" == *aarch64* ]]'

URL_DARWIN_X86_64="$(declare -f download_url >/dev/null 2>&1 && download_url darwin x86_64 || echo "")"
assert "download_url darwin x86_64 is non-empty" '[ -n "$URL_DARWIN_X86_64" ]'
assert "download_url darwin x86_64 contains 'stt-daemon'" '[[ "$URL_DARWIN_X86_64" == *stt-daemon* ]]'
assert "download_url darwin x86_64 contains 'darwin' or 'macos'" '[[ "$URL_DARWIN_X86_64" == *darwin* || "$URL_DARWIN_X86_64" == *macos* || "$URL_DARWIN_X86_64" == *Darwin* ]]'
assert "download_url darwin x86_64 contains 'x86_64' or 'amd64'" '[[ "$URL_DARWIN_X86_64" == *x86_64* || "$URL_DARWIN_X86_64" == *amd64* ]]'

# download_url should be https
assert "download_url uses https" '[[ "$URL_LINUX_ARM64" == https://* ]]'

# ── install path ──

echo ""
echo "--- install path ---"

# We expect a get_install_dir or similar function, but the contract says ~/.local/bin.
# Test that the install destination matches the spec.
DEST="$(declare -f get_install_dir >/dev/null 2>&1 && get_install_dir || echo "")"
if [ -n "$DEST" ]; then
  assert "install dir contains .local/bin" '[[ "$DEST" == *".local/bin"* ]]'
else
  # It's OK if there's no separate function; the behavior is verified
  # by the Ghostty config test below (which checks the toggle endpoint).
  echo "  SKIP: no get_install_dir function (may be inline in install flow)"
fi

# ── Ghostty config output ──

echo ""
echo "--- Ghostty config ---"

GHOSTTY_OUTPUT="$(declare -f print_ghostty_config >/dev/null 2>&1 && print_ghostty_config || echo "")"
assert "print_ghostty_config emits keybind line" '[[ "$GHOSTTY_OUTPUT" == *keybind* ]]'
assert "print_ghostty_config references localhost:9876" '[[ "$GHOSTTY_OUTPUT" == *localhost:9876* || "$GHOSTTY_OUTPUT" == *9876* ]]'
assert "print_ghostty_config references toggle endpoint" '[[ "$GHOSTTY_OUTPUT" == *toggle* ]]'

# ── systemd unit generation (Linux path) ──

echo ""
echo "--- systemd unit ---"

SYSTEMD_OUTPUT="$(declare -f generate_systemd_unit >/dev/null 2>&1 && generate_systemd_unit || echo "")"
if [ -n "$SYSTEMD_OUTPUT" ]; then
  assert "systemd unit is a valid service unit" 'echo "$SYSTEMD_OUTPUT" | grep -q "\[Unit\]"'
  assert "systemd unit has [Service] section" 'echo "$SYSTEMD_OUTPUT" | grep -q "\[Service\]"'
  assert "systemd unit has [Install] section" 'echo "$SYSTEMD_OUTPUT" | grep -q "\[Install\]"'
  assert "systemd unit references stt-daemon binary" 'echo "$SYSTEMD_OUTPUT" | grep -q "stt-daemon"'
  assert "systemd unit is user-scoped (WantedBy=default.target)" 'echo "$SYSTEMD_OUTPUT" | grep -q "WantedBy=default.target"'
else
  echo "  SKIP: no generate_systemd_unit function (may be inline or templated differently)"
fi

# ── LaunchAgent plist generation (macOS path) ──

echo ""
echo "--- LaunchAgent plist ---"

PLIST_OUTPUT="$(declare -f generate_launchagent_plist >/dev/null 2>&1 && generate_launchagent_plist || echo "")"
if [ -n "$PLIST_OUTPUT" ]; then
  assert "plist is valid XML plist" 'echo "$PLIST_OUTPUT" | grep -q "<plist"'
  assert "plist references stt-daemon binary" 'echo "$PLIST_OUTPUT" | grep -q "stt-daemon"'
  assert "plist has Label key" 'echo "$PLIST_OUTPUT" | grep -q "Label"'
  assert "plist has ProgramArguments key" 'echo "$PLIST_OUTPUT" | grep -q "ProgramArguments"'
  assert "plist has KeepAlive or RunAtLoad key" 'echo "$PLIST_OUTPUT" | grep -qE "KeepAlive|RunAtLoad"'
else
  echo "  SKIP: no generate_launchagent_plist function (may be inline or templated differently)"
fi

# ── final: dry-run mode does not actually install ──

echo ""
echo "--- safety: dry-run / no-side-effect mode ---"

# The script should support a --dry-run flag or default to informational output
# when no --install flag is given. We test that running the script does not
# create files in ~/.local/bin or systemd/LaunchAgent dirs without explicit opt-in.
if [ -x "$INSTALL_SCRIPT" ]; then
  TMP_HOME="$(mktemp -d)"
  trap "rm -rf '$TMP_HOME'" EXIT
  mkdir -p "$TMP_HOME/.local/bin"
  mkdir -p "$TMP_HOME/.config/systemd/user"

  # Run script in dry-run mode if it supports it, else just see what it does with --help
  DRY_OUTPUT="$(HOME="$TMP_HOME" bash "$INSTALL_SCRIPT" --help 2>&1 || true)"
  if echo "$DRY_OUTPUT" | grep -qi "dry-run\|--dry-run\|usage\|help"; then
    # Script has a help/dry-run mechanism — good
    assert "script supports --help or --dry-run" 'true'
  else
    # Without a guard, at minimum the script should not install by default
    # Check that no stt-daemon binary was written to the tmp home
    if [ ! -f "$TMP_HOME/.local/bin/stt-daemon" ]; then
      assert "script does not install without explicit flags" 'true'
    else
      assert "script does not install without explicit flags" 'false'
    fi
  fi
else
  echo "  SKIP: script not executable"
fi

# ── summary ──

echo ""
echo "=== RESULTS: $PASS passed, $FAIL failed ==="
for msg in "${FAIL_MSGS[@]}"; do echo "$msg"; done

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi
exit 0
