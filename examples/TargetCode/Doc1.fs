let fileConf =
  { File.FileConf.create logDir (Naming ("{service}-{host}-{datetime}", "log")) }

// ... withTargets [
  File.create fileConf "file"
// ] ...