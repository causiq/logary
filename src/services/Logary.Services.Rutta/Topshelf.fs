// copyright Henrik Feldt 2014

namespace Topshelf

[<AutoOpen>]
module FSharpApi =

  open System
  open Topshelf
  open Topshelf.HostConfigurators
  open Topshelf.Runtime

  type Service = 
    { Start: HostControl -> bool
      Stop: HostControl -> bool
      HostConfiguration: (HostConfigurator -> HostConfigurator) list }
    static member Default = 
        { Start = (fun _ -> true)
          Stop = (fun _ -> true)
          HostConfiguration = [] }

  let toAction f = new Action(f)
  let toAction1 f = new Action<_>(f)
  let toFunc f = new Func<_>(f)

  let service_control (start : HostControl -> bool) (stop : HostControl -> bool) () =
    { new ServiceControl with
      member x.Start hc =
        start hc
      member x.Stop hc =
        stop hc }
  
  let create_service (hc:HostConfigurator) service_func = 
    hc.Service<ServiceControl>(service_func |> toFunc) |> ignore

  let run service = 
    let hostFactoryConfigurator hc = 
        let createdHc = service.HostConfiguration |> List.fold (fun chc x -> x chc) hc
        service_control service.Start service.Stop
        |> create_service createdHc

    hostFactoryConfigurator |> toAction1 |> HostFactory.Run |> int

  [<Obsolete("This function has been renamed, use run instead", false)>]
  let with_topshelf = run

  let with_start f service = 
    {service with Start = f}

  let with_stop f service = 
    {service with Stop = f}

  let add_host_configuration_step step service = {service with HostConfiguration = step::service.HostConfiguration}
  
  let add_command_line_definition str action = add_host_configuration_step (fun c -> c.AddCommandLineDefinition(str, action |> toAction1);c)

  let add_command_line_switch str action = add_host_configuration_step (fun c -> c.AddCommandLineSwitch(str, action |> toAction1);c)

  let add_dependency dep_name = add_host_configuration_step (fun c -> c.AddDependency dep_name)

  let before_install f = add_host_configuration_step (fun c -> c.BeforeInstall(f |> toAction1))

  let after_install f = add_host_configuration_step (fun c -> c.AfterInstall(f |> toAction1))

  let apply_command_line str = add_host_configuration_step (fun c -> c.ApplyCommandLine str;c)

  let before_uninstall f = add_host_configuration_step (fun c -> c.BeforeUninstall(f |> toAction))

  let after_uninstall f = add_host_configuration_step (fun c -> c.AfterUninstall(f |> toAction))

  let depends_on name = add_host_configuration_step (fun c -> c.DependsOn name)

  let depends_on_eventlog = add_host_configuration_step (fun c -> c.DependsOnEventLog())

  let depends_on_iis = add_host_configuration_step (fun c -> c.DependsOnIis())

  let depends_on_mssql = add_host_configuration_step (fun c -> c.DependsOnMsSql())

  let depends_on_rabbitmq = depends_on "RabbitMQ"

  let depends_on_msmq = add_host_configuration_step (fun c -> c.DependsOnMsmq())

  let disabled = add_host_configuration_step (fun c -> c.Disabled())

  let enable_pause_and_continue = add_host_configuration_step (fun c -> c.EnablePauseAndContinue();c)

  let enable_service_recovery f = add_host_configuration_step (fun c -> c.EnableServiceRecovery(f |> toAction1))

  let enable_shutdown = add_host_configuration_step (fun c -> c.EnableShutdown();c)

  let load_help_text_prefix asm str = add_host_configuration_step (fun c -> c.LoadHelpTextPrefix(asm,str))

  let run_as usr pwd = add_host_configuration_step (fun c -> c.RunAs(usr,pwd))

  let run_as_network_service = add_host_configuration_step (fun c -> c.RunAsNetworkService())

  let run_as_local_system = add_host_configuration_step (fun c -> c.RunAsLocalSystem())

  let run_as_local_service = add_host_configuration_step (fun c -> c.RunAsLocalService())

  let run_as_prompt = add_host_configuration_step (fun c -> c.RunAsPrompt())

  let help_text_prefix str = add_host_configuration_step (fun c -> c.SetHelpTextPrefix str)

  let start_auto = add_host_configuration_step (fun c -> c.StartAutomatically())

  let start_auto_delayed = add_host_configuration_step (fun c -> c.StartAutomaticallyDelayed())

  let start_manually = add_host_configuration_step (fun c -> c.StartManually())

  let use_env_builder f = add_host_configuration_step (fun c -> c.UseEnvironmentBuilder(new EnvironmentBuilderFactory(f));c)

  let use_host_builder f = add_host_configuration_step (fun c -> c.UseHostBuilder(new HostBuilderFactory(f));c)

  let use_service_builder f = add_host_configuration_step (fun c -> c.UseServiceBuilder(new ServiceBuilderFactory(f));c)

  let use_test_host = add_host_configuration_step (fun c -> c.UseTestHost())

  /// A module for handling the naming of the service. A part of the fluent configuration
  /// API.
  [<AutoOpen>]
  module Naming =
    let service_name str = add_host_configuration_step (fun c -> c.SetServiceName str;c)

    let instance_name str = add_host_configuration_step (fun c -> c.SetInstanceName str;c)

    let display_name str = add_host_configuration_step (fun c -> c.SetDisplayName str;c)

    let description str = add_host_configuration_step (fun c -> c.SetDescription str;c)

    let naming_from_asm asm = add_host_configuration_step (fun c -> HostConfiguratorExtensions.UseAssemblyInfoForServiceInfo(c, asm);c)

    let naming_from_this_asm = add_host_configuration_step (fun c -> HostConfiguratorExtensions.UseAssemblyInfoForServiceInfo c;c)

  [<AutoOpen>]
  module Recovery =
    type ServiceRecovery = 
      { ServiceRecoveryConfigurations: (ServiceRecoveryConfigurator -> ServiceRecoveryConfigurator) list }
      static member Default = {ServiceRecoveryConfigurations = []}

    let add_service_recovery_step step service = {service with ServiceRecoveryConfigurations = step::service.ServiceRecoveryConfigurations}

    let with_recovery serviceRecovery = 
        let f sc = serviceRecovery.ServiceRecoveryConfigurations |> List.fold (fun s x -> x s) sc |> ignore
        add_host_configuration_step (fun c -> ServiceRecoveryConfiguratorExtensions.EnableServiceRecovery(c, f |> toAction1))

    let restart (span : TimeSpan) = add_service_recovery_step (fun c -> c.RestartService(int span.TotalMinutes))

    let restart_computer (span : TimeSpan) message  = add_service_recovery_step (fun c -> c.RestartComputer(int span.TotalMinutes, message))

    let run_program (span : TimeSpan) cmd = add_service_recovery_step (fun c -> c.RunProgram(int span.TotalMinutes, cmd))

    let set_reset_period (days : TimeSpan) = add_service_recovery_step (fun c -> c.SetResetPeriod(int days.TotalDays))

    let on_crash_only = add_service_recovery_step (fun c -> c.OnCrashOnly();c)

  /// A module for making constructing times nicer with F#, not a part of the
  /// fluent configuration API.
  module Time =
    let directly = TimeSpan.FromMilliseconds 0.
    let ms (i : int) = TimeSpan.FromMilliseconds(float i)
    let s (i : int) = TimeSpan.FromSeconds (float i)
    let min (i : int) = TimeSpan.FromMinutes (float i)
    let h (i : int) = TimeSpan.FromHours (float i)
    let d (i : int) = TimeSpan.FromDays (float i)

  /// A module that wraps the calls to HostControl, not a part of the fluent
  /// configuration API.
  module HostControl =
    open System

    let request_more_time (hc : HostControl) time =
      hc.RequestAdditionalTime time

    let restart (hc : HostControl) =
      hc.Restart()

    let stop (hc : HostControl) =
      hc.Stop()
