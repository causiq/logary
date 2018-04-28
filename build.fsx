#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake
open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let configuration = environVarOrDefault "CONFIGURATION" "Release"
let release = IO.File.ReadAllLines "RELEASE_NOTES.md" |> ReleaseNotesHelper.parseReleaseNotes
let description = "Logary is a high performance, multi-target logging, metric and health-check library for mono and .Net."
let tags = "structured logging f# logs logging performance metrics semantic"
let authors = "Henrik Feldt"
let owners = "Henrik Feldt"
let projectUrl = "https://github.com/logary/logary"
let iconUrl = "https://raw.githubusercontent.com/logary/logary-assets/master/graphics/LogaryLogoSquare.png"
let licenceUrl = "https://raw.githubusercontent.com/logary/logary/master/LICENSE.md"
let copyright = sprintf "Copyright \169 %i Henrik Feldt" DateTime.Now.Year

Target "Clean" (fun _ ->
  !!"./src/**/bin/" ++ "./src/**/obj/" ++ "./artifacts"
  |> CleanDirs)

open AssemblyInfoFile

Target "AssemblyInfo" (fun _ ->
  [ yield "Logary", None
    yield "Logary.Tests", None
    yield "Logary.Facade", None
    yield "Logary.Adapters.Facade", Some "adapters"
    yield! Directory.GetDirectories "src/targets" |> Array.map Path.GetFileName |> Seq.map (fun x -> x, Some "targets")
    yield! Directory.GetDirectories "src/adapters" |> Array.map Path.GetFileName |> Seq.map (fun x -> x, Some "adapters")
    yield! Directory.GetDirectories "src/ingestion" |> Array.map Path.GetFileName |> Seq.map (fun x -> x, Some "ingestion")
    yield "Logary.Facade.Tests", None
    yield "Logary.Services.Rutta", Some "legacy"
  ]
  |> Seq.iter (fun (proj, subPath) ->
    [ Attribute.Title proj
      Attribute.Product proj
      Attribute.Copyright copyright
      Attribute.Description description
      Attribute.Version release.AssemblyVersion
      Attribute.FileVersion release.AssemblyVersion
    ]
    |> CreateFSharpAssemblyInfo (
      match subPath with
      | None -> sprintf "src/%s/AssemblyInfo.fs" proj
      | Some path -> sprintf "src/%s/%s/AssemblyInfo.fs" path proj)
  )
)

let replace fromStr toStr path =
  FileHelper.ReplaceInFiles [fromStr, toStr] [path]

Target "PaketFiles" (fun _ ->
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

Target "ProjectVersion" (fun _ ->
  !! "src/*/*.fsproj"
  |> Seq.iter (fun file ->
    printfn "Changing file %s" file
    XMLHelper.XmlPoke file "Project/PropertyGroup/Version/text()" release.NugetVersion)
)

Target "TCReportVersion" (fun _ ->
  [ yield "version", release.SemVer.ToString()
    yield "major", string release.SemVer.Major
    yield "minor", string release.SemVer.Minor
    yield "build", release.SemVer.Build
    yield "special", release.SemVer.PreRelease |> Option.map (sprintf "%O") |> function None -> "" | Some x -> x
  ]
  |> Seq.filter (snd >> String.IsNullOrWhiteSpace >> not)
  |> Seq.iter (fun (name, value) -> TeamCityHelper.SetTeamCityParameter (sprintf "ver.%s" name) value)
)

Target "Restore" (fun _ ->
  DotNetCli.Restore (fun p -> { p with WorkingDir = "src" })
)

/// This also restores.
Target "Build" (fun _ ->
  DotNetCli.Build (fun p ->
  { p with
      Configuration = configuration
      Project = "src/Logary.sln"
  })
)

Target "Tests" (fun _ ->
  let commandLine (file: string) =
    let projectName = file.Substring(0, file.Length - ".fsproj".Length) |> Path.GetFileName
    let path = Path.GetDirectoryName file
    sprintf "%s/bin/%s/netcoreapp2.0/%s.dll --summary" path configuration projectName
  Seq.concat [
    !! "src/tests/**/*.fsproj"
    !! "src/*.Tests/*.fsproj"
  ]
  |> Seq.iter (commandLine >> DotNetCli.RunCommand id))

let packParameters name =
  [ "--no-build"
    "--no-restore"
    sprintf "/p:Title=\"%s\"" name
    "/p:PackageVersion=" + release.NugetVersion
    sprintf "/p:Authors=\"%s\"" authors
    sprintf "/p:Owners=\"%s\"" owners
    "/p:PackageRequireLicenseAcceptance=false"
    sprintf "/p:Description=\"%s\"" (description.Replace(",",""))
    sprintf "/p:PackageReleaseNotes=\"%O\"" ((toLines release.Notes).Replace(",","").Replace(";", "—"))
    sprintf "/p:Copyright=\"%s\"" copyright
    sprintf "/p:PackageTags=\"%s\"" tags
    sprintf "/p:PackageProjectUrl=\"%s\"" projectUrl
    sprintf "/p:PackageIconUrl=\"%s\"" iconUrl
    sprintf "/p:PackageLicenseUrl=\"%s\"" licenceUrl
  ]
  |> String.concat " "

Target "Pack" (fun _ ->
  !! "src/targets/**/*.fsproj"
  ++ "src/adapters/**/*.fsproj"
  ++ "src/ingestion/**/*.fsproj"
  ++ "src/Logary/Logary.fsproj"
  ++ "src/Logary.CSharp/Logary.CSharp.csproj"
  |> Seq.iter (fun proj ->
    let path = proj.Substring(0, proj.Length - ".fsproj".Length)
    let name = System.IO.Path.GetFileName path
    DotNetCli.RunCommand id (
      sprintf
        "pack %s -c %s -o ./bin %s"
        proj configuration (packParameters name))
  )
)

Target "Push" (fun _ ->
  Paket.Push (fun p -> { p with WorkingDir = "src" }))

let ruttaBinFolder =
  "src/legacy/Logary.Services.Rutta/bin/Release/net461/"

Target "PackageRutta" (fun _ ->
  ignore (Directory.CreateDirectory "artifacts")
  "packages/libzmq_vc120/build/native/bin/libzmq-x64-v120-mt-4_2_30_0.dll"
    |> FileHelper.CopyFile (Path.Combine(ruttaBinFolder, "libzmq.dll"))

  ZipHelper.CreateZip
    // work dir
    ruttaBinFolder
    // file name
    (sprintf "artifacts/Rutta-v%s.zip" (release.SemVer.ToString()))
    "A zip of the Rutta router/shipping service"
    ZipHelper.DefaultZipLevel
    false
    (!! (sprintf "%s/**/*" ruttaBinFolder)))

"Build"
  ==> "PackageRutta"

let envRequired k =
  let v = Environment.GetEnvironmentVariable k
  if isNull v then failwithf "Missing environment key '%s'." k
  v

Target "CheckEnv" (fun _ -> ignore (envRequired "GITHUB_TOKEN"))

// https://github.com/fsharp/FAKE/blob/master/modules/Octokit/Octokit.fsx#L87
#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
Target "Release" (fun _ ->
  let gitOwner, gitName = "logary", "logary"
  let gitOwnerName = gitOwner + "/" + gitName
  let remote =
      Git.CommandHelper.getGitResult "" "remote -v"
      |> Seq.tryFind (fun s -> s.EndsWith "(push)" && s.Contains gitOwnerName)
      |> function None -> "git@github.com:logary/logary.git"
                | Some s -> s.Split().[0]

  Git.Staging.StageAll ""
  Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
  Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

  Git.Branches.tag "" release.NugetVersion
  Git.Branches.pushTag "" remote release.NugetVersion

  Octokit.createClientWithToken (envRequired "GITHUB_TOKEN")
  |> Octokit.createDraft gitOwner gitName release.NugetVersion
      (Option.isSome release.SemVer.PreRelease) release.Notes
  |> Octokit.releaseDraft
  |> Async.RunSynchronously
)

Target "All" ignore

"CheckEnv"
  ==> "Release"

"Clean"
  ==> "AssemblyInfo"
  ==> "PaketFiles"
  ==> "ProjectVersion"
  =?> ("TCReportVersion", TeamCityHelper.TeamCityVersion |> Option.isSome)
  ==> "Build"
  ==> "Tests"
  ==> "Pack"
  ==> "All"
  ==> "Push"
  ==> "Release"

RunTargetOrDefault "All"
