-- PLE is a good measurement of memory pressure.
-- Higher PLE is better. Watch the trend over time, not the absolute value.
-- This will only return one row for non-NUMA systems.

-- Page Life Expectancy (PLE) value for each NUMA node in current instance  (Query 35) (PLE by NUMA Node)
SELECT
	@@SERVERNAME AS[Server Name],
	[object_name],
	instance_name,
	cntr_value AS [Page Life Expectancy]
FROM sys.dm_os_performance_counters WITH (NOLOCK)
WHERE [object_name] LIKE N'%Buffer Node%' -- Handles named instances
	AND counter_name = N'Page life expectancy' OPTION (RECOMPILE);