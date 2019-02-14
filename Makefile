.PHONY: restore build test
VERSION_SUFFIX=$(shell tools/get_version.sh)

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
	docker build -t haaf/rutta:latest -t haaf/rutta:$(VERSION_SUFFIX) .
	docker build -t haaf/rutta-curl:latest -t haaf/rutta-curl:$(VERSION_SUFFIX) src/services/rutta-curl

push:
	docker push haaf/rutta:latest
	docker push haaf/rutta:$(VERSION_SUFFIX)
	docker push haaf/rutta-curl:latest
	docker push haaf/rutta-curl:$(VERSION_SUFFIX)

release: restore build tests image push
	dotnet publish

clean:
	git clean -fxd
