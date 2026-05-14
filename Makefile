.PHONY: all lint deploy help link

all: lint

help:
	@echo "Targets:"
	@echo "* lint   - Run lints on the repo"
	@echo "* deploy - Apply chezmoi configuration"
	@echo "* link   - Symlink repo to ~/.local/share/chezmoi"
	@echo "* help   - Show this help"

lint:
	@bash utils/lint.sh

deploy:
	chezmoi apply -v

link:
	@ln -snf "$(CURDIR)" "$$HOME/.local/share/chezmoi"
	@echo "Linked $(CURDIR) -> $$HOME/.local/share/chezmoi"
