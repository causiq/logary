#!/usr/bin/env bash
(cd ../.. && bundle && bundle exec rake build || true)
mkdir -p ./bin && cp ../../src/targets/Logary.Targets.Heka/bin/Release/* ./bin
fsharpc --lib:bin -r:bin/Logary.dll -r:bin/Logary.Targets.Heka.dll heka.fs && mv heka.exe bin/
docker build -t logary/heka-sample .
