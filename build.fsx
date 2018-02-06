#r @"packages/build/FAKE/tools/FakeLib.dll"
open System
open Fake

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let configuration = environVarOrDefault "CONFIGURATION" "Release"
let release = IO.File.ReadAllLines "RELEASE_NOTES.md" |> ReleaseNotesHelper.parseReleaseNotes
let description = "Logary is a high performance, multi-target logging, metric and health-check library for mono and .Net."
let tags = "structured logging f# logs logging performance metrics semantic"
let authors = "Henrik Feldt & lust4life"
let owners = "Henrik Feldt & lust4life"
let projectUrl = "https://github.com/logary/logary"
let iconUrl = "https://raw.githubusercontent.com/logary/logary-assets/master/graphics/LogaryLogoSquare.png"
let licenceUrl = "https://raw.githubusercontent.com/logary/logary/master/LICENSE.md"
let copyright = sprintf "Copyright \169 %i" DateTime.Now.Year

Target "Clean" (fun _ -> !!"./**/bin/" ++ "./**/obj/" |> CleanDirs)

open AssemblyInfoFile

Target "AssemblyInfo" (fun _ ->
  [ "Logary"
    "Logary.Tests"
    "Logary.Facade"
    "Logary.Facade.Tests"
  ]
  |> Seq.iter (fun proj ->
    [ Attribute.Title proj
      Attribute.Product proj
      Attribute.Copyright copyright
      Attribute.Description description
      Attribute.Version release.AssemblyVersion
      Attribute.FileVersion release.AssemblyVersion
    ]
    |> CreateFSharpAssemblyInfo (sprintf "src/%s/AssemblyInfo.fs" proj)
  )
)

let replace fromStr toStr path =
  FileHelper.ReplaceInFiles [fromStr, toStr] [path]

Target "PaketFiles" (fun _ ->
  replace "module FsMtParserFull" "module Logary.Internals.FsMtParserFull"
          "paket-files/messagetemplates/messagetemplates-fsharp/src/FsMtParser/FsMtParserFull.fs"

  replace "module TypeShape.Core.Core" "module Logary.Internals.TypeShape.Core"
          "paket-files/eiriktsarpalis/TypeShape/src/TypeShape/TypeShape.fs"

  replace "module Aether" "module Logary.Internals.Aether"
          "paket-files/xyncro/aether/src/Aether/Aether.fs"

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

Target "Restore" (fun _ ->
  DotNetCli.Restore (fun p -> { p with WorkingDir = "src" })
)

Target "Build" (fun _ ->
  DotNetCli.Build (fun p ->
  { p with
      Configuration = configuration
      Project = "src/Logary.sln"
  })
)

Target "RunTest" (fun _ ->
  !! "src/tests/**/*.fsproj"
  |> Seq.iter (fun file ->
    let path = file.Substring(0, file.Length - ".fsproj".Length)
    let name = System.IO.Path.GetFileName path
    DotNetCli.RunCommand id (
      sprintf "run %s/bin/%s/netcoreapp2.0/%s.dll --summary"
        path configuration name))
)

let packParameters name =
  [ "--no-build"
    "--no-restore"
    sprintf "/p:Title=\"%s\"" name
    "/p:PackageVersion=" + release.NugetVersion
    sprintf "/p:Authors=\"%s\"" authors
    sprintf "/p:Owners=\"%s\"" owners
    "/p:PackageRequireLicenseAcceptance=false"
    sprintf "/p:Description=\"%s\"" (description.Replace(",",""))
    sprintf "/p:PackageReleaseNotes=\"%O\"" ((toLines release.Notes).Replace(",",""))
    sprintf "/p:Copyright=\"%s\"" copyright
    sprintf "/p:PackageTags=\"%s\"" tags
    sprintf "/p:PackageProjectUrl=\"%s\"" projectUrl
    sprintf "/p:PackageIconUrl=\"%s\"" iconUrl
    sprintf "/p:PackageLicenseUrl=\"%s\"" licenceUrl
  ]
  |> String.concat " "

Target "Pack" (fun _ ->
  !! "src/targets/**/*.fsproj"
  ++ "src/Logary/Logary.fsproj"
  ++ "src/Logary.CSharp/Logary.CSharp.csproj"
  |> Seq.iter (fun proj ->
    DotNetCli.RunCommand id (
      sprintf
        "pack %s -c %s -o ../bin %s"
        proj configuration (packParameters "Http.fs"))
  )
)

Target "Push" (fun _ ->
  Paket.Push (fun p -> { p with WorkingDir = "bin" }))

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"

Target "Release" (fun _ ->
    let gitOwner = "haf"
    let gitName = "expecto"
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

    let user = getUserInput "Github Username: "
    let pw = getUserPassword "Github Password: "

    Octokit.createClient user pw
    |> Octokit.createDraft gitOwner gitName release.NugetVersion
        (Option.isSome release.SemVer.PreRelease) release.Notes
    |> Octokit.releaseDraft
    |> Async.RunSynchronously
)

Target "All" ignore

"Clean"
==> "AssemblyInfo"
==> "PaketFiles"
==> "ProjectVersion"
==> "Restore"
==> "Build"
//==> "RunTest" // until I remove the path base and make it relative
==> "Pack"
==> "All"
==> "Push"
==> "Release"

RunTargetOrDefault "All"
