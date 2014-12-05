#I "FAKE/tools"
#I "FSharp.Formatting/lib/net40"
#I "FSharp.Compiler.Service/lib/net40"
#I "RazorEngine/lib/net45"
#I "Microsoft.AspNet.Razor/lib/net45"

#r "FakeLib.dll"
#r "FSharp.Literate.dll"
#r "FSharp.CodeFormat.dll"
#r "FSharp.MetadataFormat.dll"
#r "RazorEngine.dll"
#r "System.Web.Razor.dll"

open Fake
open System.IO
open Fake.FileHelper
open FSharp.Literate
open FSharp.MetadataFormat

// --------------------------------------------------------------------------------------
// Builds the documentation from `.fsx` and `.md` files in the 'docs/content' directory
// (the generated documentation is stored in the 'docs/output' directory)
// --------------------------------------------------------------------------------------

// Binaries that have XML documentation (in a corresponding generated XML file)
let referenceBinaries = [ "Intelliplan.Logary.dll" ]
// Web site location for the generated documentation
let website = "/"

let githubLink = "http://github.com/logary/logary" 
// Specify more information about your project
let info =
  [ "project-name", "logary"
    "project-author", "Henrik Feldt, Intelliplan International AB"
    "project-summary", "A high performance logging library written in F# for the CLR."
    "project-github", githubLink
    "project-nuget", "https://nuget.com/packages/Logary" ]

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let root = website
#else
let root = "file://" + (__SOURCE_DIRECTORY__ @@ "../build")
#endif

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "../src/Logary/bin/Release"
let content    = __SOURCE_DIRECTORY__ @@ "../docs/content"
let files      = __SOURCE_DIRECTORY__ @@ "../docs/files"
let templates  = __SOURCE_DIRECTORY__ @@ "../docs/templates"

let output     = __SOURCE_DIRECTORY__ @@ "../build/api"
let formatting = __SOURCE_DIRECTORY__ @@ "./FSharp.Formatting"
let docTemplate = formatting @@ "templates/docpage.cshtml"

// Where to look for *.csproj templates (in this order)
let layoutRoots =
  [ templates; formatting @@ "templates"
    formatting @@ "templates/reference" ]

// Copy static files and CSS + JS from F# Formatting
let copyFiles () =
  CopyRecursive files output true |> Log "Copying file: "
  ensureDirectory (output @@ "content")
  CopyRecursive (formatting @@ "styles") (output @@ "content") true 
    |> Log "Copying styles and scripts: "

// Build API reference from XML comments
let buildReference () =
  CleanDir (output @@ "reference")
  for lib in referenceBinaries do
    MetadataFormat.Generate
      ( bin @@ lib, output, layoutRoots, 
        parameters = ("root", root)::info,
        sourceRepo = githubLink @@ "tree/master",
        sourceFolder = __SOURCE_DIRECTORY__ @@ ".." @@ "src" @@ "Logary",
        publicOnly = true,
        libDirs    = [ bin ])

// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation () =
  let subdirs = Directory.EnumerateDirectories(content, "*", SearchOption.AllDirectories)
  for dir in Seq.append [content] subdirs do
    let sub = if dir.Length > content.Length then dir.Substring(content.Length + 1) else "."
    Literate.ProcessDirectory
      ( dir, docTemplate, output @@ sub, replacements = ("root", root)::info,
        layoutRoots = layoutRoots )

// Generate
copyFiles()
buildDocumentation()
buildReference()
