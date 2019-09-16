.PHONY: prepare restore build test docs
export CONFIGURATION=${CONFIGURATION:-'Release'}
TAG_VERSION_SUFFIX := $(shell tools/version.sh)

all: restore build

prepare:
	./fake.sh build --single-target --target AssemblyInfo
	./fake.sh build --single-target --target PaketFiles

restore:
	tools/install-tools
	dotnet restore src/Logary.sln

build: prepare restore
	dotnet build src/Logary.sln -c Release

test: build
	./fake.sh build --single-target --target Tests

docs:
	(cd ./docs && yarn && yarn dev)

image:
	echo "Building image v$(TAG_VERSION_SUFFIX)"
ifneq ($(TRAVIS_TAG),)
	docker build -t haaf/rutta:latest -t haaf/rutta:$(TAG_VERSION_SUFFIX) -t haaf/rutta:$(TRAVIS_TAG) .
	docker build -t haaf/rutta-curl:latest -t haaf/rutta-curl:$(TAG_VERSION_SUFFIX) -t haaf/rutta:$(TRAVIS_TAG) src/services/rutta-helm-chart/rutta-curl
else
	docker build -t haaf/rutta:latest -t haaf/rutta:$(TAG_VERSION_SUFFIX) .
	docker build -t haaf/rutta-curl:latest -t haaf/rutta-curl:$(TAG_VERSION_SUFFIX) src/services/rutta-helm-chart/rutta-curl
endif

push:
ifneq ($(TRAVIS_TAG),)
	docker push haaf/rutta:latest
	docker push haaf/rutta:$(TRAVIS_TAG)
	docker push haaf/rutta:$(TAG_VERSION_SUFFIX)
	docker push haaf/rutta-curl:latest
	docker push haaf/rutta-curl:$(TRAVIS_TAG)
	docker push haaf/rutta-curl:$(TAG_VERSION_SUFFIX)
else
	docker push haaf/rutta:latest
	docker push haaf/rutta:$(TAG_VERSION_SUFFIX)
	docker push haaf/rutta-curl:latest
	docker push haaf/rutta-curl:$(TAG_VERSION_SUFFIX)
endif

pack_library:
	./fake.sh build --single-target --target Pack

push_library: pack_library
	./fake.sh build --single-target --target CheckEnv
	./fake.sh build --single-target --target Push

release_library:
	./fake.sh build --single-target --target CheckEnv
	./fake.sh build --single-target --target Release

release: restore build test pack_library release_library image push push_library

clean:
	git clean -fxd
