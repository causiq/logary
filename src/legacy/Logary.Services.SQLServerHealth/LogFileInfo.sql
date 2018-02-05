-- Things to look at:
-- How full are the transaction logs ?

-- Log file size, log usage size  (Query 21) (Database Properties)
-- for all databases on instance
SELECT
	db.[name] AS [Database Name],
	ls.cntr_value AS [Log Size (KB)],
	lu.cntr_value AS [Log Used (KB)],
	CAST(CAST(lu.cntr_value AS FLOAT) / CAST(ls.cntr_value AS FLOAT)AS DECIMAL(18,2)) * 100 AS [Log Used %]
FROM sys.databases AS db WITH (NOLOCK)
	INNER JOIN sys.dm_os_performance_counters AS lu WITH (NOLOCK)
		ON db.name = lu.instance_name
	INNER JOIN sys.dm_os_performance_counters AS ls WITH (NOLOCK)
		ON db.name = ls.instance_name
WHERE lu.counter_name LIKE N'Log File(s) Used Size (KB)%'
	AND ls.counter_name LIKE N'Log File(s) Size (KB)%'
	AND ls.cntr_value > 0 OPTION (RECOMPILE);