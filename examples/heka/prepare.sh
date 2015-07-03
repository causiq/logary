#!/usr/bin/env bash
set -e
(cd ../.. && mono tools/paket.bootstrapper.exe && \
             mono tools/paket.exe install && \
             xbuild /p:Configuration=Debug src/Heka.sln
)
mkdir -p ./bin && cp ../../src/targets/Logary.Targets.Heka/bin/Debug/* ./bin
fsharpc --lib:../../src/targets/Logary.Targets.Heka/bin/Debug -r:Logary.dll -r:Logary.Targets.Heka.dll heka.fs && mv heka.exe bin/
docker-compose build
