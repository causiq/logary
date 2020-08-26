# Ingress

If you want to append the route in your own ingress, you have to create an `ExternalName` service in your own namespace:

```
kind: Service
apiVersion: v1
metadata:
  name: rutta
  namespace: my-awesome-namespace
spec:
  type: ExternalName
  externalName: rutta.logary.svc.cluster.local
```