# Rutta ᜑ high performance log router 🦋

The recommended approach to using Rutta is to run it as a docker container;

    docker run --rm -it haaf/rutta
    
or, if you're a Kubernetes user;

    helm install incubator/rutta

See the [Rutta Helm chart for details][helm-chart] on installation.

See the [Stackdriver docs][stackdriver-docs] for configuring the Stackdriver target.


## Testing Rutta


Terminal 1

    nc -u -p 54321 127.0.0.1 9090
    hello world

Terminal 2

    dotnet run -- router --listener udp 0.0.0.0:9090 plain --target console://. --verbose

 [helm-chart]: https://github.com/logary/logary/tree/master/src/services/rutta-helm-chart/README.md
 [stackdriver-docs]: https://github.com/logary/logary/blob/master/src/targets/Logary.Targets.Stackdriver/README.md
