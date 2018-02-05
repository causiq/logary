-- Shows you the drive-level latency for reads and writes, in milliseconds
-- Latency above 20-25ms is usually a problem
-- http://www.sqlskills.com/blogs/paul/how-to-examine-io-subsystem-latencies-from-within-sql-server/

-- Drive level latency information (Query 19) (Drive Level Latency)
-- Based on code from Jimmy May
SELECT [Drive],
	CASE
		WHEN num_of_reads = 0 THEN 0
		ELSE (io_stall_read_ms/num_of_reads)
	END AS [Read Latency],
	CASE
		WHEN io_stall_write_ms = 0 THEN 0
		ELSE (io_stall_write_ms/num_of_writes)
	END AS [Write Latency],
	CASE
		WHEN (num_of_reads = 0 AND num_of_writes = 0) THEN 0
		ELSE (io_stall/(num_of_reads + num_of_writes))
	END AS [Overall Latency],
	CASE
		WHEN num_of_reads = 0 THEN 0
		ELSE (num_of_bytes_read/num_of_reads)
	END AS [Avg Bytes/Read],
	CASE
		WHEN io_stall_write_ms = 0 THEN 0
		ELSE (num_of_bytes_written/num_of_writes)
	END AS [Avg Bytes/Write],
	CASE
		WHEN (num_of_reads = 0 AND num_of_writes = 0) THEN 0
		ELSE ((num_of_bytes_read + num_of_bytes_written)/(num_of_reads + num_of_writes))
	END AS [Avg Bytes/Transfer]
FROM (
	SELECT
		LEFT(UPPER(mf.physical_name), 2) AS Drive,
		SUM(num_of_reads) AS num_of_reads,
		SUM(io_stall_read_ms) AS io_stall_read_ms,
		SUM(num_of_writes) AS num_of_writes,
		SUM(io_stall_write_ms) AS io_stall_write_ms,
		SUM(num_of_bytes_read) AS num_of_bytes_read,
		SUM(num_of_bytes_written) AS num_of_bytes_written,
		SUM(io_stall) AS io_stall
	FROM sys.dm_io_virtual_file_stats(NULL, NULL) AS vfs
		INNER JOIN sys.master_files AS mf WITH (NOLOCK)
			ON vfs.database_id = mf.database_id
			AND vfs.file_id = mf.file_id
	GROUP BY LEFT(UPPER(mf.physical_name), 2)
	) AS tab
ORDER BY [Overall Latency] OPTION (RECOMPILE);
