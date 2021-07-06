namespace Logary.Services.Rutta

open Argu

type PrefixedEnvVarReader(prefix: string) =
  let inner = EnvironmentVariableConfigurationReader() :> IConfigurationReader
  interface IConfigurationReader with
    member _.Name = "Logary Rutta Environment Variable reader"
    member _.GetValue key =
      // default implementation will give key:
      // 'LOGARY_RUTTA_analytics id'
      let munged = key.ToUpperInvariant().Replace(" ", "_")
      inner.GetValue($"%s{prefix}%s{munged}")