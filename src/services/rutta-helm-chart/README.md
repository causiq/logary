# Logary Rutta Helm chart

Rutta is a high-performance log router/shipper written in F# but configured from the command line. It's "cloud native" in that it runs as a docker container on top of .Net Core.

Rutta is GPLv3 licensed (or alternatively commercially licensed). It's a packaging of [Logary](https://github.com/logary/logary) as a service, that you can run, either as a sidecar container or as a log router in the middle of things.

Rutta is completely stateless, so you can run any number of replicas. It runs as a Kubernetes deployment with three replicas by default.

A common configuration for Rutta is to configure a Stackdriver target, a HTTP ingestion listener as well as a UDP ingestion listener. Your apps send UDP log messages to the UDP endpoint and your frontends (native apps and web sites) send HTTP messages to the HTTP endpoint. Rutta when batch-ships these log messages into Stackdriver.

By default this chart exposes a HTTP listener/endpoint and prints to console; in order for it to log to Stackdriver, AliYun or AppInsights, you have to configure those explicitly in the values file.

## Configuration

TODO.
