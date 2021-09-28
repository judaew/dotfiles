.PHONY: all install sync clean

all: clean sync

install:
	./utils/bootstrap.sh

sync:
	./utils/sync.sh

clean:
	./utils/clean.sh
