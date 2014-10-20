using System;

using Logary;
using Logary.Configuration;
using Logary.Target;
using Logary.AspNet;
using Debugger = Logary.Target.Debugger;
using Logger = Logary.Logger;
using LogManager = Logary.LogManager;

[assembly: WebActivatorEx.PreApplicationStartMethod(
    typeof($rootnamespace$.App_Start.LogaryLifecycle), "PreStart")]
[assembly: WebActivatorEx.PreApplicationStartMethod(
    typeof($rootnamespace$.App_Start.LogaryLifecycle), "Shutdown")]

namespace $rootnamespace$.App_Start {
    static readonly Logger logger = Logary.Logging.GetCurrentLogger();
    static LogManager _logManager;

    public static class LogaryLifecycle {
        public static void PreStart() {
            _logManager = LogglyFactory.New($AssemblyName,
                with => with
                            .Target<Debugger.Builder>(
                                "debugger", conf => conf.AcceptIf(NotReader)
                            )
                            .Target<Logstash.Builder>(
                                "logstash-app",
                                conf => conf
                                            .MinLevel(LogLevel.Info)
                                            .Target
                                            .Hostname(SettingFor("logstash.host"))
                                            .Port(Convert.ToUInt16(SettingFor("logstash.port")))
                                            .EventVersion(Logstash.EventVersion.One)
                                            .Done()));
        }

        static string SettingFor(string key)
        {
            var val = ConfigurationManager.AppSettings[key];
            if (val == null) throw new Exception(string.Format("no AppSetting[{0}] configured", key));
            return val;
        }
        public static void Shutdown() {
            _logManager.Dispose();
        }
    }
}