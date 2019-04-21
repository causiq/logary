// set 'logDir' to specific path like Environment.CurrentDirectory if you are on windows
.Target<File.Builder>(
    "file",
    file => file.Target.FileSystem(new FileSystem.DotNetFileSystem(logDir))
                       .Naming("{service}-{host}-{datetime}", "log").Done())