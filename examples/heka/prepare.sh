#!/usr/bin/env bash
(cd ../.. && bundle && bundle exec rake || true)
mkdir -p ./bin && cp ../../src/targets/Logary.Targets.Heka/bin/Release/* ./bin
fsharpc --lib:bin -r:bin/Logary.dll -r:bin/Logary.Targets.Heka.dll heka.fs && mv heka.exe bin/