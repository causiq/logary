# Rutta ᜑ high performance log router 🦋

Logary Rutta works great for shipping logs from nodes into a central location.

You can either deploy Logary Rutta as a DaemonSet, which will cause it to appear on every node in your
cluster:

    kustomize build k8s/as-daemonset | kubectl apply -f -

or you can deploy it as a load-balanced Deployment that is used by multiple nodes;

    # useful when you only have one node and you're testing:
    kustomize build k8s/as-daemonset | kubectl apply -f -

    # alternative, for production:
    kustomize build k8s/as-daemonset-with-scaling | kubectl apply -f -

## Testing Rutta

Terminal 1

    nc -u -p 54321 127.0.0.1 9090
    hello world

Terminal 2

    dotnet run -- router --listener udp 0.0.0.0:9090 plain --target console://. --verbose

 [helm-chart]: https://github.com/logary/logary/tree/master/src/services/rutta-helm-chart/README.md
 [stackdriver-docs]: https://github.com/logary/logary/blob/master/src/targets/Logary.Targets.Stackdriver/README.md
