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

.PHONY: pieces-dev pieces-stop pieces-status version-bump

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

## version-bump: set the pieces version in every file that pins it.
## Usage: make version-bump 0.2.2
version-bump:
	@v="$(filter-out $@,$(MAKECMDGOALS))"; \
	if [ -z "$$v" ]; then echo "usage: make version-bump <X.Y.Z>   e.g. make version-bump 0.2.2"; exit 1; fi; \
	echo "$$v" | grep -qE '^[0-9]+\.[0-9]+\.[0-9]+$$' || { echo "version must be X.Y.Z (got: $$v)"; exit 1; }; \
	echo "bumping pieces version $(VERSION) -> $$v"; \
	sed "s/^version = \"[^\"]*\"/version = \"$$v\"/" pieces-daemon/Cargo.toml > pieces-daemon/Cargo.toml.tmp && mv pieces-daemon/Cargo.toml.tmp pieces-daemon/Cargo.toml; \
	awk -v v="$$v" 'BEGIN{q="\""} /^name = "pieces"$$/{f=1} f&&/^version = /{sub(/"[^"]*"/,q v q);f=0} {print}' pieces-daemon/Cargo.lock > pieces-daemon/Cargo.lock.tmp && mv pieces-daemon/Cargo.lock.tmp pieces-daemon/Cargo.lock; \
	sed "s/^PIECES_VERSION=\"[^\"]*\"/PIECES_VERSION=\"$$v\"/" skills/pieces/pieces.sh > skills/pieces/pieces.sh.tmp && mv skills/pieces/pieces.sh.tmp skills/pieces/pieces.sh && chmod +x skills/pieces/pieces.sh; \
	sed "s/\"version\": \"[^\"]*\"/\"version\": \"$$v\"/" .claude-plugin/plugin.json > .claude-plugin/plugin.json.tmp && mv .claude-plugin/plugin.json.tmp .claude-plugin/plugin.json; \
	sed "s/^VERSION[[:space:]]*:=.*/VERSION    := $$v/" Makefile > Makefile.tmp && mv Makefile.tmp Makefile; \
	echo "updated: Cargo.toml, Cargo.lock, pieces.sh, plugin.json, Makefile -> $$v"; \
	echo "next: review 'git diff', commit, then  git tag -f v$$v && git push origin v$$v --force"

# Let a bare version arg (the 0.2.2 in `make version-bump 0.2.2`) be a no-op goal
# instead of a "No rule to make target" error. Match-anything rules are excluded
# from makefile remaking, so this does not interfere with the real targets above.
%:
	@:
