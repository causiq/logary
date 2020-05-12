.PHONY: restore version_files build test docs docs_ci rutta

CONFIGURATION ?= release
TAG_VERSION_SUFFIX := $(shell tools/version.sh)

all: restore version_files build

restore:
	dotnet tool restore

version_files:
	dotnet fake build --single-target --target AssemblyInfo
	dotnet fake build --single-target --target PaketFiles

build: version_files restore
	dotnet build src/Logary.sln -c release

test: build
	dotnet fake build --single-target --target Tests

docs:
	(cd ./docs && yarn && yarn dev)

docs_ci:
	npm install -g now-pipeline@1.8.0
	(cd ./docs && yarn && yarn cypress:run)

pack_library:
	dotnet fake build --single-target --target Pack

push_library: pack_library
	dotnet fake build --single-target --target CheckEnv
	dotnet fake build --single-target --target Push

release_library:
	dotnet fake build --single-target --target CheckEnv
	dotnet fake build --single-target --target Release

rutta:
	(cd src/services/Logary.Services.Rutta && make release)

release: restore build test pack_library release_library push_library rutta

clean:
	git clean -fxd
