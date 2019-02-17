.PHONY: restore build test
TAG_VERSION_SUFFIX=$(shell tools/get_version.sh)

all: restore build

prepare:
	./fake.sh build --target AssemblyInfo
	./fake.sh build --target PaketFiles

restore:
	dotnet restore src/Logary.sln

build: prepare
	dotnet build src/Logary.sln -c Release

test:
	./fake.sh build --target Tests

image:
ifneq ($(TRAVIS_TAG),)
	docker build -t haaf/rutta:latest -t haaf/rutta:$(TAG_VERSION_SUFFIX) -t haaf/rutta:$(TRAVIS_TAG) .
	docker build -t haaf/rutta-curl:latest -t haaf/rutta-curl:$(TAG_VERSION_SUFFIX) -t haaf/rutta:$(TRAVIS_TAG) src/services/rutta-curl
else
	docker build -t haaf/rutta:latest -t haaf/rutta:$(TAG_VERSION_SUFFIX) .
	docker build -t haaf/rutta-curl:latest -t haaf/rutta-curl:$(TAG_VERSION_SUFFIX) src/services/rutta-curl
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

release: restore build tests image push
	dotnet publish

clean:
	git clean -fxd
