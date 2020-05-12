.PHONY: prepare restore build test docs docs_ci

CONFIGURATION ?= Release
TAG_VERSION_SUFFIX := $(shell tools/version.sh)

all: restore prepare build

prepare:
	./fake.sh build --single-target --target AssemblyInfo
	./fake.sh build --single-target --target PaketFiles

restore:
	dotnet tool restore

build: prepare restore
	dotnet build src/Logary.sln -c Release

test: build
	./fake.sh build --single-target --target Tests

docs:
	(cd ./docs && yarn && yarn dev)

docs_ci:
	npm install -g now-pipeline@1.8.0
	(cd ./docs && yarn && yarn cypress:run)

pack_library:
	./fake.sh build --single-target --target Pack

push_library: pack_library
	./fake.sh build --single-target --target CheckEnv
	./fake.sh build --single-target --target Push

release_library:
	./fake.sh build --single-target --target CheckEnv
	./fake.sh build --single-target --target Release

release: restore build test pack_library release_library push_library

clean:
	git clean -fxd
