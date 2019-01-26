VERSION_SUFFIX=$(./tools/get_version.sh)

all: restore build

prepare:
	./fake.sh build --target AssemblyInfo
	./fake.sh build --target PaketFiles

restore:
	dotnet restore

build: prepare
	dotnet build src/Logary.sln -c Release

test:
	./fake.sh build --target Tests

release: restore build tests
	dotnet publish

clean:
	git clean -fxd
