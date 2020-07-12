namespace Logary.Configuration.Target

type TargetConfWriter<'targetConf> =
  abstract write: key: string * value: string * hasOwnField: bool -> 'targetConf
