namespace Logary.Model

[<RequireQualifiedAccess>]
type K8s =
  | Cluster of string
  | DaemonSet of string
  | StatefulSet of string
  | Deployment of string
  | ReplicaSet of string
  | Namespace of string
  | Pod of string
  | Container of string

type ResourceLocation =
  | Named of name: string * value: string
  | BuildVersion of version: string
  | BuildId of buildId: string
  | Kubernetes of K8s list
  | Hostname of string
  | Hypervisor of string
  | Rack of string
  | DataCenter of string
  | Region of string
  | Country of string

/// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/overview.md#resources
/// Resource captures information about the entity for which telemetry is recorded. For example, metrics exposed by a Kubernetes container can be linked to a resource that specifies the cluster, namespace, pod, and container name.
type Resource =
  { service: string
    detail: ResourceLocation list
    /// Formats the resource as a set of key-value pairs describing the location that this software runs.
  }

  static member create(service, detail) =
    { service=service; detail=detail }
  static member create(service, ?host) =
    if host.IsSome then
      Resource.create(service, Hostname host.Value :: [])
    else
      Resource.create(service, [])

module Resource =
  let ofLabels (xs: #seq<string * string>) =
    let state = { service = ""; detail = [] }

    let addK8s s v =
      match s.detail |> List.tryPick (function Kubernetes k -> Some k | _ -> None) with
      | Some ks ->
        { s with detail = s.detail |> List.map (function Kubernetes _ -> Kubernetes (v :: ks) | o -> o) }
      | None ->
        { s with detail = Kubernetes [ v ] :: s.detail }

    let fold s (k, v) =
      match k with
      | "service" -> { s with service = v }
      | "build_version" -> { s with detail = BuildVersion v :: s.detail }
      | "build_id" -> { s with detail = BuildId v :: s.detail }
      | "rack" -> { s with detail = Rack v :: s.detail }
      | "hypervisor" -> { s with detail = Hypervisor v :: s.detail }
      | "dataCenter" -> { s with detail = DataCenter v :: s.detail }
      | "region" -> { s with detail = Region v :: s.detail }
      | "country" -> { s with detail = Country v :: s.detail }
      | "k8s_cluster" -> addK8s s (K8s.Cluster v)
      | "k8s_daemonset" -> addK8s s (K8s.DaemonSet v)
      | "k8s_statefulset" -> addK8s s (K8s.StatefulSet v)
      | "k8s_deployment" -> addK8s s (K8s.Deployment v)
      | "k8s_replicaset" -> addK8s s (K8s.ReplicaSet v)
      | "k8s_namespace" -> addK8s s (K8s.Namespace v)
      | "k8s_pod" -> addK8s s (K8s.Pod v)
      | "k8s_container" -> addK8s s (K8s.Container v)
      | _ -> { s with detail = Named (k, v) :: s.detail }

    xs |> Seq.fold fold state

  let ofMap m =
    m |> Seq.map (fun (KeyValue (k, v)) -> k, v) |> ofLabels

  let asLabels (x: Resource) =
    seq {
      yield "service", x.service
      for detail in x.detail do
        match detail with
        | Named (k, v) -> yield k, v
        | BuildVersion v -> yield "build_version", v
        | BuildId bid -> yield "build_id", bid
        | Kubernetes ks ->
          for k in ks do
            match k with
            | K8s.Cluster c -> yield "k8s_cluster", c
            | K8s.DaemonSet d -> yield "k8s_daemonset", d
            | K8s.StatefulSet s -> yield "k8s_statefulset", s
            | K8s.Deployment d -> yield "k8s_deployment", d
            | K8s.ReplicaSet r -> yield "k8s_replicaset", r
            | K8s.Namespace n -> yield "k8s_namespace", n
            | K8s.Pod p -> yield "k8s_pod", p
            | K8s.Container c -> yield "k8s_container", c
        | Hypervisor h -> yield "hypervisor", h
        | Hostname h -> yield "hostname", h
        | Rack r -> yield "rack", r
        | DataCenter d -> yield "datacenter", d
        | Region r -> yield "region", r
        | Country c -> yield "country", c
    }

  let asMap r =
    asLabels r |> Map.ofSeq

  let setDetail matcher value (resource: Resource) =
    let replace detail replacement = detail |> List.map (function Hostname _ -> replacement | x -> x)

    let existing = resource.detail |> List.exists matcher
    if existing then
      { resource with detail = value |> replace resource.detail }
    else
      { resource with detail = value :: resource.detail }
