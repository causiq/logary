#r "paket: groupref Build //"

#nowarn "52"
#load "./.fake/build.fsx/intellisense.fsx"
#load "paket-files/build/eiriktsarpalis/snippets/SlnTools/SlnTools.fs"

// https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095
#if !FAKE
  #r "Facades/netstandard"
#endif

open System
open System.IO
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.Tools
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Api
open Fake.BuildServer
open Fake.SystemHelper

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let configuration =
  Environment.environVarOrDefault "CONFIGURATION" "Release"
  |> DotNet.BuildConfiguration.fromString
let release = ReleaseNotes.load "RELEASE_NOTES.md"
let description = "Logary is a high performance, multi-target logging, metric and health-check library for mono and .Net."
let tags = "structured logging f# logs logging performance metrics semantic"
let authors = "Henrik Feldt"
let owners = "Henrik Feldt"
let projectUrl = "https://github.com/logary/logary"
let iconUrl = "https://raw.githubusercontent.com/logary/logary-assets/master/graphics/LogaryLogoSquare.png"
let licenceUrl = "https://raw.githubusercontent.com/logary/logary/master/LICENSE.md"
let copyright = sprintf "Copyright \169 %i Henrik Feldt" DateTime.Now.Year

let pkgPath = Path.GetFullPath "./pkg"

let testProjects =
  !! "src/tests/**/*.fsproj"
  ++ "src/services/*.Tests/*.fsproj"
  ++ "src/*.Tests/*.fsproj"

let libProjects =
  !! "src/targets/**/*.fsproj"
  ++ "src/adapters/**/*.fsproj"
  ++ "src/ingestion/**/*.fsproj"
  ++ "src/Logary/Logary.fsproj"
  ++ "src/Logary.CSharp/Logary.CSharp.csproj"

let envRequired k =
  let v = Environment.GetEnvironmentVariable k
  if isNull v then failwithf "Missing environment key '%s'." k
  v

Target.create "Clean" <| fun _ ->
  // This line actually ensures we get the correct version checked in
  // instead of the one previously bundled with 'fake`
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
      AssemblyInfo.Copyright copyright
      AssemblyInfo.Description description
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
  replace "module FsMtParserFull" "module Logary.Internals.FsMtParserFull"
          "paket-files/messagetemplates/messagetemplates-fsharp/src/FsMtParser/FsMtParserFull.fs"

  replace "module TypeShape.Core.Core" "module Logary.Internals.TypeShape.Core.Core"
          "paket-files/eiriktsarpalis/TypeShape/src/TypeShape/TypeShape.fs"

  replace "module TypeShape.Core.Utils" "module Logary.Internals.TypeShape.Core.Utils"
          "paket-files/eiriktsarpalis/TypeShape/src/TypeShape/Utils.fs"

  replace "module Aether" "module Logary.Internals.Aether"
          "paket-files/xyncro/aether/src/Aether/Aether.fs"

  replace "namespace Chiron" "namespace Logary.Internals.Chiron"
          "paket-files/neoeinstein/chiron/src/Chiron/Chiron.fs"

  replace "module YoLo" "module internal Logary.YoLo"
          "paket-files/haf/YoLo/YoLo.fs"

  replace "module Hopac" "namespace Logary.Internals"
          "paket-files/logary/RingBuffer/RingBuffer.fs"

  replace "namespace Logary.Facade" "namespace Libryy.Logging"
          "paket-files/logary/logary/src/Logary.Facade/Facade.fs"

  replace "namespace Logary.Facade" "namespace Cibryy.Logging"
          "paket-files/logary/logary/src/Logary.CSharp.Facade/Facade.cs"
)

Target.create "ProjectVersion" (fun _ ->
  !! "src/*/*.fsproj"
  |> Seq.iter (fun file ->
    printfn "Changing file %s" file
    Xml.poke file "Project/PropertyGroup/Version/text()" release.NugetVersion)
)

Target.create "TCReportVersion" (fun _ ->
  [ yield "version", release.SemVer.ToString()
    yield "major", string release.SemVer.Major
    yield "minor", string release.SemVer.Minor
    yield "build", string release.SemVer.Build
    yield "special", release.SemVer.PreRelease |> Option.map (sprintf "%O") |> function None -> "" | Some x -> x
  ]
  |> Seq.filter (snd >> String.IsNullOrWhiteSpace >> not)
  |> Seq.iter (fun (name, value) -> TeamCity.setParameter (sprintf "ver.%s" name) value)
)

/// This also restores.
Target.create "Build" (fun _ ->
  DotNet.build (fun p -> { p with Configuration = configuration }) "src/Logary.sln"
)

Target.create "Tests" (fun _ ->
  let commandLine (file: string) =
    let projectName = file.Substring(0, file.Length - ".fsproj".Length) |> Path.GetFileName
    let path = Path.GetDirectoryName file
    sprintf "%s/bin/%O/netcoreapp2.2/%s.dll --summary" path configuration projectName
  testProjects
  |> Seq.iter (commandLine >> DotNet.exec id "" >> ignore))

Target.create "Pack" <| fun _ ->
  let args =
    { MSBuild.CliArguments.Create() with
        NoLogo = true
        DoRestore = false
        Properties =
          [ "PackageVersion", release.NugetVersion
            "Authors", authors
            "Owners", owners
            "PackageRequireLicenseAcceptance", "true"
            "Description", description.Replace(",","")
            "PackageReleaseNotes", (release.Notes |> String.toLines).Replace(",","").Replace(";", "—")
            "Copyright", copyright
            "PackageTags", tags
            "PackageProjectUrl", projectUrl
            "PackageIconUrl", iconUrl
            "PackageLicenseUrl", licenceUrl
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
        ApiKey = envRequired "NUGET_TOKEN" }
  Paket.push setParams

Target.create "CheckEnv" <| fun _ ->
  ignore (envRequired "GITHUB_TOKEN")
  ignore (envRequired "NUGET_TOKEN")

Target.create "Release" (fun _ ->
  let gitOwner, gitName = "logary", "logary"
  let gitOwnerName = gitOwner + "/" + gitName
  let remote =
      Git.CommandHelper.getGitResult "" "remote -v"
      |> Seq.tryFind (fun s -> s.EndsWith "(push)" && s.Contains gitOwnerName)
      |> function None -> "git@github.com:logary/logary.git"
                | Some s -> s.Split().[0]

  Git.Staging.stageAll ""
  Git.CommitMessage.setMessage "" (sprintf "Bump version to %s" release.NugetVersion)
  Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

  Git.Branches.tag "" release.NugetVersion
  Git.Branches.pushTag "" remote release.NugetVersion

  GitHub.createClientWithToken (envRequired "GITHUB_TOKEN")
  |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion
      (Option.isSome release.SemVer.PreRelease) release.Notes
  |> GitHub.publishDraft
  |> Async.RunSynchronously
)

"CheckEnv"
  ==> "Release"

"Clean"
  ==> "AssemblyInfo"
  ==> "PaketFiles"
  ==> "ProjectVersion"
  =?> ("TCReportVersion", TeamCity.Environment.Version |> Option.isSome)
  ==> "Build"
  ==> "Tests"
  ==> "Pack"
  ==> "Push"
  ==> "Release"

Target.runOrDefault "Pack"
