SET STATISTICS IO ON
GO
SELECT
  [io_stall_read_ms] AS ioStallReadMs,
  [io_stall_write_ms] AS ioStallWriteMs,
  [io_stall] AS ioStall,
  [num_of_reads] AS numOfReads,
  [num_of_writes] AS numOfWrites,
  [num_of_bytes_read] AS numOfBytesRead,
  [num_of_bytes_written] AS numOfBytesWritten,
  LEFT ([mf].[physical_name], 2) AS drive,
  DB_NAME ([vfs].[database_id]) AS dbName,
  [mf].[physical_name] AS filePath,
  [vfs].[file_id] AS fileType
FROM
  sys.dm_io_virtual_file_stats (NULL,NULL) AS [vfs]
JOIN sys.master_files AS [mf]
  ON [vfs].[database_id] = [mf].[database_id]
  AND [vfs].[file_id] = [mf].[file_id]