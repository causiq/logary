#r @"packages/build/FAKE/tools/FakeLib.dll"

open System
open Fake

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let configuration = environVarOrDefault "Configuration" "Release"
let release = IO.File.ReadAllLines "RELEASE_NOTES.md" |> ReleaseNotesHelper.parseReleaseNotes
let description = "A simple, functional HTTP client library for F#"
let tags = "http fsharp client functional"
let authors = "Henrik Feldt"
let owners = "Henrik Feldt"
let projectUrl = "https://github.com/haf/Http.fs"
let iconUrl = "https://raw.githubusercontent.com/haf/Http.fs/releases/v4.x/docs/files/img/logo.png"
let licenceUrl = "https://github.com/haf/Http.fs/blob/master/Licence/License.md"
let copyright = sprintf "Copyright \169 %i" DateTime.Now.Year

Target "Clean" (fun _ -> !!"./**/bin/" ++ "./**/obj/" |> CleanDirs)

open AssemblyInfoFile
Target "AssemblyInfo" (fun _ ->

    [ "HttpFs"
      "HttpFs.UnitTests"
      "HttpFs.IntegrationTests"
    ]
    |> List.iter (fun product ->
        [ Attribute.Title product
          Attribute.Product product
          Attribute.Copyright copyright
          Attribute.Description description
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion
        ] |> CreateFSharpAssemblyInfo (product+"/AssemblyInfo.fs")
    )
)

Target "PaketFiles" (fun _ ->
    FileHelper.ReplaceInFiles ["namespace Logary.Facade","namespace HttpFs.Logging"]
        ["paket-files/logary/logary/src/Logary.Facade/Facade.fs"]
)

Target "ProjectVersion" (fun _ ->
    [
        "HttpFs/HttpFs.fsproj"
    ]
    |> List.iter (fun file ->
        XMLHelper.XmlPoke file "Project/PropertyGroup/Version/text()" release.NugetVersion)
)
let build project framework =
    DotNetCli.Build (fun p ->
    { p with
        Configuration = configuration
        Framework = framework
        Project = project
    })

Target "BuildTest" (fun _ ->
    build "HttpFs.UnitTests/HttpFs.UnitTests.fsproj" "netcoreapp2.0"
    build "HttpFs.UnitTests/HttpFs.UnitTests.fsproj" "net461"
    build "HttpFs.IntegrationTests/HttpFs.IntegrationTests.fsproj" "netcoreapp2.0"
    build "HttpFs.IntegrationTests/HttpFs.IntegrationTests.fsproj" "net461"
)

Target "RunTest" (fun _ ->
    DotNetCli.RunCommand id ("HttpFs.UnitTests/bin/"+configuration+"/netcoreapp2.0/HttpFs.UnitTests.dll --summary --sequenced")
    //Shell.Exec ("HttpFs.UnitTests/bin/"+configuration+"/net461/HttpFs.UnitTests.exe","--summary --sequenced")
    //|> fun r -> if r<>0 then failwith "HttpFs.UnitTests.exe failed"

    DotNetCli.RunCommand id ("HttpFs.IntegrationTests/bin/"+configuration+"/netcoreapp2.0/HttpFs.IntegrationTests.dll --summary --sequenced")
    //Shell.Exec ("HttpFs.IntegrationTests/bin/"+configuration+"/net461/HttpFs.IntegrationTests.exe","--summary --sequenced")
    //|> fun r -> if r<>0 then failwith "HttpFs.IntegrationTests.exe failed"
)

Target "Pack" (fun _ ->
    let packParameters name =
        [
            "--no-build"
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
        ] |> String.concat " "

    DotNetCli.RunCommand id
        ("pack HttpFs/HttpFs.fsproj -c "+configuration + " -o ../bin " + (packParameters "Http.fs"))
)

Target "Push" (fun _ -> Paket.Push (fun p -> { p with WorkingDir = "bin" }))

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
Target "Release" (fun _ ->
    let gitOwner = "haf"
    let gitName = "expecto"
    let gitOwnerName = gitOwner + "/" + gitName
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.tryFind (fun s -> s.EndsWith "(push)" && s.Contains gitOwnerName)
        |> function None -> ("ssh://github.com/"+gitOwnerName) | Some s -> s.Split().[0]

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
==> "BuildTest"
==> "RunTest"
==> "Pack"
==> "All"
==> "Push"
==> "Release"

RunTargetOrDefault "All"