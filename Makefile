.PHONY: build

all: build

build: 
	stack build --fast

watch:
	stack build --fast --file-watch
