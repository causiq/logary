.PHONY: restore build test
export CONFIGURATION=${CONFIGURATION:-'Release'}
TAG_VERSION_SUFFIX=$(shell tools/get_version.sh)

all: restore build

prepare:
	./fake.sh build --single-target --target AssemblyInfo
	./fake.sh build --single-target --target PaketFiles

restore:
	dotnet restore src/Logary.sln

build: prepare restore
	dotnet build src/Logary.sln -c Release

test: build
	./fake.sh build --single-target --target Tests

image:
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

release: restore build test release_library image push

clean:
	git clean -fxd
