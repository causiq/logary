// copyright Henrik Feldt 2018

namespace Topshelf

[<AutoOpen>]
module FSharpApi =

  open System
  open Topshelf
  open Topshelf.HostConfigurators
  open Topshelf.Runtime

  type Service =
    { start: HostControl -> bool
      stop: HostControl -> bool
      hostConfiguration: (HostConfigurator -> HostConfigurator) list }


  let defaultService =
    { start = (fun _ -> true)
      stop = (fun _ -> true)
      hostConfiguration = [] }

  let toAction f = new Action(f)
  let toAction1 f = new Action<_>(f)
  let toFunc f = new Func<_>(f)

  let serviceControl (start: HostControl -> bool) (stop: HostControl -> bool) () =
    { new ServiceControl with
        member x.Start hc =
          start hc
        member x.Stop hc =
          stop hc }

  let createService (hc:HostConfigurator) serviceFunc =
    hc.Service<ServiceControl>(serviceFunc |> toFunc) |> ignore

  let run service =
    let hostFactoryConfigurator hc =
        let createdHc = service.hostConfiguration |> List.fold (fun chc x -> x chc) hc
        serviceControl service.start service.stop
        |> createService createdHc

    hostFactoryConfigurator |> toAction1 |> HostFactory.Run |> int

  let withStart f service =
    {service with start = f}

  let withStop f service =
    {service with stop = f}

  let addHostConfigurationStep step service = {service with hostConfiguration = step :: service.hostConfiguration}

  let addCommandLineDefinition str action = addHostConfigurationStep (fun c -> c.AddCommandLineDefinition(str, action |> toAction1);c)

  let addCommandLineSwitch str action = addHostConfigurationStep (fun c -> c.AddCommandLineSwitch(str, action |> toAction1);c)

  let addDependency dep_name = addHostConfigurationStep (fun c -> c.AddDependency dep_name)

  let beforeInstall f = addHostConfigurationStep (fun c -> c.BeforeInstall(f |> toAction1))

  let afterInstall f = addHostConfigurationStep (fun c -> c.AfterInstall(f |> toAction1))

  let applyCommandLine str = addHostConfigurationStep (fun c -> c.ApplyCommandLine str;c)

  let beforeUninstall f = addHostConfigurationStep (fun c -> c.BeforeUninstall(f |> toAction))

  let afterUninstall f = addHostConfigurationStep (fun c -> c.AfterUninstall(f |> toAction))

  let dependsOn name = addHostConfigurationStep (fun c -> c.DependsOn name)

  let dependsOn_eventlog = addHostConfigurationStep (fun c -> c.DependsOnEventLog())

  let dependsOn_iis = addHostConfigurationStep (fun c -> c.DependsOnIis())

  let dependsOn_mssql = addHostConfigurationStep (fun c -> c.DependsOnMsSql())

  let dependsOn_rabbitmq = dependsOn "RabbitMQ"

  let dependsOn_msmq = addHostConfigurationStep (fun c -> c.DependsOnMsmq())

  let disabled = addHostConfigurationStep (fun c -> c.Disabled())

  let enablePauseAndContinue = addHostConfigurationStep (fun c -> c.EnablePauseAndContinue();c)

  let enableServiceRecovery f = addHostConfigurationStep (fun c -> c.EnableServiceRecovery(f |> toAction1))

  let enableShutdown = addHostConfigurationStep (fun c -> c.EnableShutdown();c)

  let loadHelpTextPrefix asm str = addHostConfigurationStep (fun c -> c.LoadHelpTextPrefix(asm,str))

  let runAs usr pwd = addHostConfigurationStep (fun c -> c.RunAs(usr,pwd))

  let runAsNetworkService = addHostConfigurationStep (fun c -> c.RunAsNetworkService())

  let runAsLocalSystem = addHostConfigurationStep (fun c -> c.RunAsLocalSystem())

  let runAsLocalService = addHostConfigurationStep (fun c -> c.RunAsLocalService())

  let runAsPrompt = addHostConfigurationStep (fun c -> c.RunAsPrompt())

  let helpTextPrefix str = addHostConfigurationStep (fun c -> c.SetHelpTextPrefix str)

  let startAuto = addHostConfigurationStep (fun c -> c.StartAutomatically())

  let startAutoDelayed = addHostConfigurationStep (fun c -> c.StartAutomaticallyDelayed())

  let startManually = addHostConfigurationStep (fun c -> c.StartManually())

  let useEnvBuilder f = addHostConfigurationStep (fun c -> c.UseEnvironmentBuilder(new EnvironmentBuilderFactory(f));c)

  let useHostBuilder f = addHostConfigurationStep (fun c -> c.UseHostBuilder(new HostBuilderFactory(f));c)

  let useServiceBuilder f = addHostConfigurationStep (fun c -> c.UseServiceBuilder(new ServiceBuilderFactory(f));c)

  let useTestHost = addHostConfigurationStep (fun c -> c.UseTestHost())

  /// A module for handling the naming of the service. A part of the fluent configuration
  /// API.
  [<AutoOpen>]
  module Naming =
    let serviceName str = addHostConfigurationStep (fun c -> c.SetServiceName str;c)

    let instanceName str = addHostConfigurationStep (fun c -> c.SetInstanceName str;c)

    let displayName str = addHostConfigurationStep (fun c -> c.SetDisplayName str;c)

    let description str = addHostConfigurationStep (fun c -> c.SetDescription str;c)

    let namingFromAsm asm = addHostConfigurationStep (fun c -> HostConfiguratorExtensions.UseAssemblyInfoForServiceInfo(c, asm);c)

    let namingFromThisAsm = addHostConfigurationStep (fun c -> HostConfiguratorExtensions.UseAssemblyInfoForServiceInfo c;c)

  [<AutoOpen>]
  module Recovery =
    type ServiceRecovery =
      { serviceRecoveryConfigurations: (ServiceRecoveryConfigurator -> ServiceRecoveryConfigurator) list }


    let defaultServiceRecovery =
      { serviceRecoveryConfigurations = []}

    let addServiceRecoveryStep step service = {service with serviceRecoveryConfigurations = step::service.serviceRecoveryConfigurations}

    let withRecovery serviceRecovery =
        let f sc = serviceRecovery.serviceRecoveryConfigurations |> List.fold (fun s x -> x s) sc |> ignore
        addHostConfigurationStep (fun c -> ServiceRecoveryConfiguratorExtensions.EnableServiceRecovery(c, f |> toAction1))

    let restart (span: TimeSpan) =
      addServiceRecoveryStep (fun c -> c.RestartService(int span.TotalMinutes))

    let restartComputer (span: TimeSpan) message =
      addServiceRecoveryStep (fun c -> c.RestartComputer(int span.TotalMinutes, message))

    let runProgram (span: TimeSpan) cmd =
      addServiceRecoveryStep (fun c -> c.RunProgram(int span.TotalMinutes, cmd))

    let setResetPeriod (days: TimeSpan) =
      addServiceRecoveryStep (fun c -> c.SetResetPeriod(int days.TotalDays))

    let onCrashOnly =
      addServiceRecoveryStep (fun c -> c.OnCrashOnly();c)

  /// A module for making constructing times nicer with F#, not a part of the
  /// fluent configuration API.
  module Time =
    let directly = TimeSpan.FromMilliseconds 0.
    let ms (i: int) = TimeSpan.FromMilliseconds(float i)
    let s (i: int) = TimeSpan.FromSeconds (float i)
    let min (i: int) = TimeSpan.FromMinutes (float i)
    let h (i: int) = TimeSpan.FromHours (float i)
    let d (i: int) = TimeSpan.FromDays (float i)

  /// A module that wraps the calls to HostControl, not a part of the fluent
  /// configuration API.
  module HostControl =
    open System

    let requestMoreTime (hc: HostControl) time =
      hc.RequestAdditionalTime time

    let restart (hc: HostControl) =
      hc.Restart()

    let stop (hc: HostControl) =
      hc.Stop()
