.PHONY: all install sync clean

all: clean sync

install:
	./bootstrap.sh

sync:
	./utils/sync.sh

clean:
	./utils/clean.sh
