# inline-suggestion llama.cpp server management
# Usage:
#   make status          - check if the server is running
#   make up              - start the server (idempotent)
#   make down            - stop the server (idempotent)
#
# Requires env var INLINE_SUGGEST_MODEL (e.g. "qwen-coder-7b.gguf")
# Override defaults:
#   make up PORT=8081

MODEL   := ~/models/$(INLINE_SUGGEST_MODEL)
PORT    ?= 8080
GPU     ?= 99
PIDFILE := /tmp/inline-suggestion-llama.pid

.PHONY: status up down

status:
	@if [ -f "$(PIDFILE)" ] && kill -0 $$(cat "$(PIDFILE)") 2>/dev/null; then \
		echo "llama-server is running (pid $$(cat $(PIDFILE)), port $(PORT))"; \
	else \
		rm -f "$(PIDFILE)"; \
		echo "llama-server is not running"; \
	fi

up:
	@if [ -z "$(INLINE_SUGGEST_MODEL)" ]; then \
		echo "Error: INLINE_SUGGEST_MODEL env var is not set"; exit 1; \
	fi
	@if [ -f "$(PIDFILE)" ] && kill -0 $$(cat "$(PIDFILE)") 2>/dev/null; then \
		echo "llama-server already running (pid $$(cat $(PIDFILE)))"; \
	else \
		rm -f "$(PIDFILE)"; \
		echo "Starting llama-server on port $(PORT) ..."; \
		nohup llama-server -m $(MODEL) -ngl $(GPU) --port $(PORT) \
			> /tmp/inline-suggestion-llama.log 2>&1 & \
		echo $$! > "$(PIDFILE)"; \
		echo "llama-server started (pid $$(cat $(PIDFILE)))"; \
	fi

down:
	@if [ -f "$(PIDFILE)" ] && kill -0 $$(cat "$(PIDFILE)") 2>/dev/null; then \
		echo "Stopping llama-server (pid $$(cat $(PIDFILE))) ..."; \
		kill $$(cat "$(PIDFILE)"); \
		rm -f "$(PIDFILE)"; \
		echo "llama-server stopped"; \
	else \
		rm -f "$(PIDFILE)"; \
		echo "llama-server is not running"; \
	fi
