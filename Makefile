.PHONY: restore build test
VERSION_SUFFIX=$(shell ./tools/get_version.sh)

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
	docker build . -t haaf/rutta:latest -t haaf/rutta:$(VERSION_SUFFIX)

push:
	docker push haaf/rutta

release: restore build tests image push
	dotnet publish

clean:
	git clean -fxd
