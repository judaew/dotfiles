.PHONY: all sync

all: sync

install:
	./bootstrap.sh

sync:
	./utils/sync.sh


