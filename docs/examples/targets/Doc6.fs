LogaryFactory.New("demoService", conf =>
    conf.InternalLoggingLevel(LogLevel.Verbose)
        .Target<Debugger.Builder>("internal.debugger", tb => tb.UseForInternalLog())
        .Target<Logary.Targets.Console.Builder>("internal.console", tb => tb.UseForInternalLog())
        .Target<LiterateConsole.Builder>("console1")
        .Target<AliYun.Builder>("AliYunLog", tb => {
             tb.MinLevel(LogLevel.Verbose)
             .Target
             .ConfClient("key",
                         "keyid",
                         "endpoint")
             .ConfLogLocation("project", "logstore")
             .SetConnectTimeOut(1000)
             .SetReadWriteTimeOut(5000)
             .Done();
        })
);