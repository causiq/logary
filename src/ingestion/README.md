# Logary Ingestion

All packages in this folder should fulfil the following module interface:

 - `create: XXXConfig -> Ingest -> Job<unit>`
 
`XXXConfig` must have a field `cancelled: Hopac.Promise<unit>`, which can be used to shut down the server.
 
The returned `Hopac.Job<Proc>` must throw immediately if the server fails to start, otherwise,
it must return a Proc that can be joined at shutdown.
