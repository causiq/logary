namespace Logary.Services.Rutta

open Argu

type PrefixedEnvVarReader(prefix: string) =
  let inner = EnvironmentVariableConfigurationReader() :> IConfigurationReader
  interface IConfigurationReader with
    member __.Name = "Logary Rutta Environment Variable reader"
    member __.GetValue key =
      // default implementation will give key:
      // 'LOGARY_RUTTA_analytics id'
      let munged = key.ToUpperInvariant().Replace(" ", "_")
      let key = sprintf "%s%s" prefix munged
      inner.GetValue(key)