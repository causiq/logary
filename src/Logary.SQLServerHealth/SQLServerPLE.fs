module Logary.Metrics.SQLServerPLE

module Database =
  open FsSql

  type PLE =
    { serverName         : string
      objectName         : string
      instanceName       : string
      pageLifeExpectancy : int64 }

  /// PLE is a good measurement of memory pressure.
  /// Higher PLE is better. Watch the trend over time, not the absolute value.
  /// This will only return one row for non-NUMA systems.
  /// 
  /// Page Life Expectancy (PLE) value for each NUMA node in current instance
  /// (Query 35) (PLE by NUMA Node)
  let ple connMgr =
    let sql = "
SELECT
  @@SERVERNAME  AS serverName,
  [object_name] AS objectName,
  instance_name AS instanceName,
  cntr_value    AS pageLifeExpectancy
FROM sys.dm_os_performance_counters WITH (NOLOCK)
WHERE [object_name] LIKE N'%Buffer Node%' -- Handles named instances
  AND counter_name = N'Page life expectancy' OPTION (RECOMPILE)"
    Sql.execReader connMgr sql []
    |> Sql.mapOne (Sql.asRecord<PLE> "")
    |> fun ple ->
      { ple with objectName   = ple.objectName.TrimEnd()
                 instanceName = ple.instanceName.TrimEnd() }
