.PHONY: all build run

# default build target
all::

all:: build

APP:=$(shell basename $(PWD))

build:

	docker build --tag heroku-docker-r-example:$(APP) .

run:

	docker run -it --rm heroku-docker-r-example:$(APP)
