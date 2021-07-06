#r "paket: groupref Build //"

#nowarn "52"
#load "./.fake/build.fsx/intellisense.fsx"
#load "paket-files/build/eiriktsarpalis/snippets/SlnTools/SlnTools.fs"

open System
open System.IO
open Fake.Api
open Fake.BuildServer
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.SystemHelper
open Fake.Tools

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let configuration =
  Environment.environVarOrDefault "CONFIGURATION" "Release"
  |> DotNet.BuildConfiguration.fromString

let release = ReleaseNotes.load "RELEASE_NOTES.md"
let testFramework = "net5.0"
let dotnetExePath = "dotnet"

let githubToken = lazy(Environment.environVarOrFail "GITHUB_TOKEN")
let nugetToken = lazy(Environment.environVarOrFail "NUGET_TOKEN")

let pkgPath = Path.GetFullPath "./pkg"

BuildServer.install [
  Travis.Installer
  AppVeyor.Installer
]

let testProjects =
  !! "src/*.Tests/*.fsproj"
  ++ "src/services/*.Tests/*.fsproj"

let libProjects =
  !! "src/targets/**/*.fsproj"
  ++ "src/adapters/**/*.fsproj"
  ++ "src/ingestion/**/*.fsproj"
  ++ "src/Logary/Logary.fsproj"
  ++ "src/Logary.Prometheus/Logary.Prometheus.fsproj"
  ++ "src/Logary.CSharp/Logary.CSharp.csproj"

Target.create "Clean" <| fun _ ->
  !! "./**/bin/"
  ++ "./**/obj/"
  ++ pkgPath
  |> Shell.cleanDirs
  // This line actually ensures we get the correct version checked in instead of the one previously bundled with 'fake`
  Git.CommandHelper.gitCommand "" "checkout .paket/Paket.Restore.targets"

Target.create "AssemblyInfo" (fun _ ->
  [ yield "Logary", None
    yield "Logary.Tests", None
    yield "Logary.Facade", None
    yield "Logary.Adapters.Facade", Some "adapters"
    yield! Directory.GetDirectories "src/targets" |> Array.map Path.GetFileName |> Seq.map (fun x -> x, Some "targets")
    yield! Directory.GetDirectories "src/adapters" |> Array.map Path.GetFileName |> Seq.map (fun x -> x, Some "adapters")
    yield! Directory.GetDirectories "src/ingestion" |> Array.map Path.GetFileName |> Seq.map (fun x -> x, Some "ingestion")
    yield "Logary.Facade.Tests", None
    yield "Logary.Services.Rutta", Some "services"
  ]
  |> Seq.iter (fun (proj, subPath) ->
    [ AssemblyInfo.Title proj
      AssemblyInfo.Product proj
      AssemblyInfo.Version release.AssemblyVersion
      AssemblyInfo.FileVersion release.AssemblyVersion
    ]
    |> AssemblyInfoFile.createFSharp (
      match subPath with
      | None -> sprintf "src/%s/AssemblyInfo.fs" proj
      | Some path -> sprintf "src/%s/%s/AssemblyInfo.fs" path proj)
  )
)

let replace fromStr toStr path =
   Shell.replaceInFiles [fromStr, toStr] [path]

Target.create "PaketFiles" (fun _ ->
  replace "namespace Logary.Facade" "namespace Libryy.Logging"
          "paket-files/logary/logary/src/Logary.Facade/Facade.fs"
  replace "namespace Logary.Facade" "namespace Cibryy.Logging"
          "paket-files/logary/logary/src/Logary.CSharp.Facade/Facade.cs"
)

/// This also restores.
Target.create "Build" <| fun _ ->
  let msbuildParams =
    { MSBuild.CliArguments.Create() with
        Verbosity = Some Quiet
        NoLogo = true
        Properties = [ "Optimize", "true"; "DebugSymbols", "true"; "Version", release.AssemblyVersion ] }

  let setParams (o: DotNet.BuildOptions) =
    { o with
        Configuration = configuration
        MSBuildParams = msbuildParams }

  DotNet.build setParams "src/Core.sln"

Target.create "Tests" (fun _ ->
  let commandLine (file: string) =
    let projectName = file.Substring(0, file.Length - ".fsproj".Length) |> Path.GetFileName
    let path = Path.GetDirectoryName file
    sprintf "%s/bin/%O/%s/%s.dll --summary" path configuration testFramework projectName
  testProjects
    |> Seq.iter (commandLine >> DotNet.exec id "" >> ignore))

Target.create "Pack" <| fun _ ->
  let args =
    { MSBuild.CliArguments.Create() with
        NoLogo = true
        DoRestore = false
        Properties = [
          "Version", release.NugetVersion
          "PackageReleaseNotes", String.toLines release.Notes
        ]
    }
  let pkgSln = SlnTools.createTempSolutionFile libProjects
  let setParams (p: DotNet.PackOptions) =
    { p with
        OutputPath = Some pkgPath
        Configuration = configuration
        MSBuildParams = args }
  DotNet.pack setParams pkgSln

Target.create "Push" <| fun _ ->
  let setParams (p: Paket.PaketPushParams) =
    { p with
        WorkingDir = pkgPath
        ApiKey = nugetToken.Value }
  // for f in *.nupkg; do dotnet paket push  --api-key $NUGET_TOKEN $f; done
  Paket.push setParams

Target.create "CheckEnv" <| fun _ ->
  ignore (githubToken.Value)
  ignore (nugetToken.Value)

Target.create "Release" <| fun _ ->
  let remote =
      Git.CommandHelper.getGitResult "" "remote -v"
      |> Seq.tryFind (fun s -> s.EndsWith "(push)" && s.Contains "logary/logary")
      |> function None -> "git@github.com:logary/logary.git"
                | Some s -> s.Split().[0]

  Git.Staging.stageAll ""
  Git.CommitMessage.setMessage "" (sprintf "Bump version to %s" release.NugetVersion)
  Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

  Git.Branches.tag "" release.NugetVersion
  Git.Branches.pushTag "" remote release.NugetVersion

  githubToken.Value
  |> GitHub.createClientWithToken
  |> GitHub.draftNewRelease "logary" "logary" release.NugetVersion
                            release.SemVer.PreRelease.IsSome release.Notes
  |> GitHub.publishDraft
  |> Async.RunSynchronously

"CheckEnv"
  ==> "Release"

"Clean"
  ==> "AssemblyInfo"
  ==> "PaketFiles"
  ==> "Build"
  ==> "Tests"
  ==> "Pack"
  ==> "Push"
  ==> "Release"

Target.runOrDefault "Pack"
