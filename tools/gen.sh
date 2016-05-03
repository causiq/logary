#!/usr/bin/env bash
openssl req -x509 -sha256 -nodes -days 1825 -newkey rsa:2048 -keyout logary.key -out logary.crt
openssl rsa -in logary.key -outform PVK -pvk-strong -out logary.pvk
openssl pkcs12 -export -out logary.pfx -inkey logary.key -in logary.crt
# [type password]
openssl crl2pkcs7 -nocrl -certfile logary.crt -outform DER -out logary.spc
# [type password]
