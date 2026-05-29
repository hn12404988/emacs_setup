#!/bin/sh
# Foreground run for ad-hoc debugging.
# For the supervised run use: systemctl --user start local-llm.service
set -eu
cd "$(dirname "$0")"
exec python3 server.py
