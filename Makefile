# ── pieces-daemon dev workflow ────────────────────────────────────────────
# `make pieces-dev` builds the daemon, installs it over the version-pinned
# binary that `skills/pieces/pieces.sh up` launches, and restarts it.
#
# BIN matches that script's layout exactly ($VERSION/bin/pieces-$os-$arch) so a
# local dev build lands where the script looks — otherwise `up` would re-download
# the release binary over it when the daemon is not already running.
#
# The running daemon is a detached process (parent = init) bound to PORT, so we
# stop it by PORT with `fuser` — this catches the one the script started (which
# has no pidfile) and avoids `pkill -f` matching this recipe's own shell.

VERSION    := 0.2.1
PORT       := 8723
PIECES_DIR := $(HOME)/.local/share/pieces
OS         := $(shell uname -s | sed 's/Linux/linux/;s/Darwin/darwin/')
ARCH       := $(shell uname -m | sed 's/amd64/x86_64/;s/aarch64/arm64/')
BIN        := $(PIECES_DIR)/$(VERSION)/bin/pieces-$(OS)-$(ARCH)
LOG        := $(PIECES_DIR)/daemon.log
PIDFILE    := $(PIECES_DIR)/daemon.pid
BUILD      := pieces-daemon/target/debug/pieces

.PHONY: pieces-dev pieces-stop pieces-status

## pieces-dev: build (debug), overwrite the installed binary, restart the daemon
pieces-dev:
	# 1. build — aborts here (no install) if the build fails
	cd pieces-daemon && cargo build
	# 2. overwrite the installed binary with the fresh build (creates bin/ if needed)
	install -D -m 0755 $(BUILD) $(BIN)
	# 3a. stop whatever currently holds the port (SIGKILL — reliable port release)
	-fuser -k $(PORT)/tcp 2>/dev/null
	@for i in $$(seq 1 20); do fuser -s $(PORT)/tcp 2>/dev/null || break; sleep 0.1; done
	# 3b. start detached so it survives this make process (parent becomes init)
	@mkdir -p $(PIECES_DIR)
	nohup $(BIN) --port $(PORT) >> $(LOG) 2>&1 & echo $$! > $(PIDFILE)
	# 3c. wait for /health, then report
	@for i in $$(seq 1 50); do \
		curl -fsS "http://127.0.0.1:$(PORT)/health" >/dev/null 2>&1 && { \
			echo "✓ pieces daemon up: http://127.0.0.1:$(PORT)/ (pid $$(cat $(PIDFILE)))"; \
			exit 0; }; \
		sleep 0.1; \
	done; \
	echo "✗ daemon did not become healthy — see $(LOG)"; exit 1

## pieces-stop: stop the running daemon
pieces-stop:
	-fuser -k $(PORT)/tcp 2>/dev/null
	@echo "stopped any daemon on port $(PORT)"

## pieces-status: show whether the daemon is healthy
pieces-status:
	@curl -fsS "http://127.0.0.1:$(PORT)/health" >/dev/null 2>&1 \
		&& echo "✓ up on http://127.0.0.1:$(PORT)/" \
		|| echo "✗ not responding on port $(PORT)"
