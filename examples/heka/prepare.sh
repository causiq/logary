#!/usr/bin/env bash
set -e
#(cd ../.. && bundle && CONFIGURATION=Debug bundle exec rake build)
mkdir -p ./bin && cp ../../src/targets/Logary.Targets.Heka/bin/Debug/* ./bin
fsharpc --lib:../../src/targets/Logary.Targets.Heka/bin/Debug -r:Logary.dll -r:Logary.Targets.Heka.dll heka.fs && mv heka.exe bin/
docker build -t logary/heka-sample .

