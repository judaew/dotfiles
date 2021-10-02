.PHONY: all install sync lint clean

all: clean sync lint

install:
	./utils/bootstrap.sh

sync:
	./utils/sync.sh

lint:
	./utils/lint.sh

clean:
	./utils/clean.sh

