import { useRef } from 'react'
import Head from 'next/head';
import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import Code from '../../components/Code'
import DotnetIcon from '../../components/DotnetIcon'

export default function DOTNetQuickstart() {
  const toc =
    [
      { id: "download-.net-core", title: "Download .Net core 2.2", ref: useRef(null) },
      { id: ".net-core", title: ".Net Core 2.2", ref: useRef(null) },
      { id: "create-application", title: "Create your first application", ref: useRef(null) },
      { id: "install-the-package", title: "Install the package", ref: useRef(null) },
      { id: "hello-fsharp", title: "Hello F#", ref: useRef(null) },
      { id: "hello-csharp", title: "Hello C#", ref: useRef(null) },
    ]

  return (
    <DocPage name="dotnet-quickstart" title=".Net Quickstart" icon={<DotnetIcon width={58} />} colour="purple" readingMinutes={4} toc={toc}>
      <Head>
        <title key="title">Logary — .Net Quickstart</title>
      </Head>
      <DocSection {...toc[0]}>
        <h2 className="section-title">Download .Net Core 2.2</h2>
        <p>
          Download the <a href="https://dotnet.microsoft.com/download">.NET Core 2.2 SDK</a> to try .NET Core on your Windows, macOS, or Linux machine. Visit <a href="https://hub.docker.com/_/microsoft-dotnet-core/">dotnet/core</a> if you prefer to use Docker containers.
        </p>
        <p>All .NET Core versions are available at <a href="https://dotnet.microsoft.com/download/archives">.NET Core Downloads</a> if you're looking for another .NET Core version.</p>
      </DocSection>
      <DocSection {...toc[1]}>
        <h2 className="section-title">.Net Core 2.2</h2>
        <p>
          The latest version is <a href="https://docs.microsoft.com/en-us/dotnet/core/whats-new/dotnet-core-2-2">.NET Core 2.2</a> New features include: framework-dependent deployments, startup hooks, AAD authentication with Azure SQL, and support for Windows ARM32.
        </p>
      </DocSection>
      <DocSection {...toc[2]}>
        <h2 className="section-title">Create your first application</h2>
        <p>
          After installing the .NET Core SDK, open a command prompt. Type the following dotnet commands to create and run a C# application.
        </p>
        <p><code>dotnet new console</code></p>
        <p><code>dotnet run</code></p>
        <p>
          After installing the .NET Core SDK, open a command prompt. Type the following dotnet commands to create and run a C# application.
        </p>
        <p><code>Hello World!</code></p>
      </DocSection>
      <DocSection {...toc[3]}>
        <h2 className="section-title">Install the package</h2>
        <p>
          The first step is to install the Logary package(s) from NuGet.
        </p>
        <p>
          <code>paket add logary --version ">= 5.0.0"</code>
        </p>
        <p>Or you're using NuGet/VS...</p>
        <p>
          <code>Install-Package Logary -Version 5.0.0-rc.10</code>
        </p>
      </DocSection>
      <DocSection {...toc[4]}>
        <h2 className="third-title">Hello World (F#)</h2>
        <Code language="fsharp" value={
          preval`
          const fs = require('fs')
          const val = fs.readFileSync(__dirname + '/../../../examples/Logary.ConsoleApp/Program.fs', 'utf8')
          module.exports = val
          `
        } />
      </DocSection>
      <DocSection {...toc[5]}>
        <h2 className="section-title">Hello World (C#)</h2>
        <Code language="csharp" value={
          preval`
          const fs = require('fs')
          const val = fs.readFileSync(__dirname + '/../../../examples/Logary.CSharpExample/Program.cs', 'utf8')
          module.exports = val
          `
        } />
      </DocSection>

    </DocPage>
  )
}