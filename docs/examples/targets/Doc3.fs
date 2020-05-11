open Logary.Targets.Stackdriver

let projectId = "your gcloud project id"
// either a custom name, or you can use one of the well-known stream names that you can retrieve from [the lists](https://cloud.google.com/logging/docs/view/logs_index)
// this name doesn't have to be url-encoded as per the spec, the target will do that for you
// the specified log should exist before use
let logname = "the stream you want to log to"
// create your monitored resource:
let resource = ComputeInstance("my zone", "my instanceId")
// or container:
// let resource = Container("my cluster", "my namespace", "my instanceID", "my pod", "my name", "my zone")
// or appengine:
// let resource = AppEngine("my moduleId", "my version")

let conf = StackdriverConf.create(projectId, logname, resource)