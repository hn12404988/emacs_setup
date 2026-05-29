# Thin wrapper around the local-llm systemd user service.
# The unit file is version-controlled here (./local-llm.service) and
# `make install` copies it into ~/.config/systemd/user/.

SERVICE := local-llm.service
UNIT_DIR := $(HOME)/.config/systemd/user
HERE := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

.PHONY: up down status logs enable disable install

up:
	systemctl --user start $(SERVICE)

down:
	systemctl --user stop $(SERVICE)

status:
	systemctl --user status $(SERVICE) --no-pager

logs:
	journalctl --user-unit $(SERVICE) -f

enable:
	systemctl --user enable $(SERVICE)

disable:
	systemctl --user disable $(SERVICE)

install:
	mkdir -p $(UNIT_DIR)
	cp $(HERE)$(SERVICE) $(UNIT_DIR)/$(SERVICE)
	systemctl --user daemon-reload
	@echo "Installed $(SERVICE) -> $(UNIT_DIR)"
	@echo "One-time only (run once if not already done):"
	@echo "  sudo loginctl enable-linger $$USER"
	@echo "  systemctl --user enable --now $(SERVICE)"
