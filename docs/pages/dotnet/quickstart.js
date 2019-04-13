import { useRef } from 'react'
import Head from 'next/head';
import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import Code from '../../components/Code'
import DotnetIcon from '../../components/DotnetIcon'

export default function DOTNetQuickstart() {
  const toc =
    [ { id: "install-the-package", title: "Install the package", ref: useRef(null) },
      { id: "hello-fsharp", title: "Hello F#", ref: useRef(null) },
      { id: "hello-csharp", title: "Hello C#", ref: useRef(null) },
    ]

  return (
    <DocPage name="dotnet-quickstart" title=".Net Quickstart" icon={<DotnetIcon width={58} />} colour="purple" readingMinutes={4} toc={toc}>
      <Head>
        <title key="title">Logary — .Net Quickstart</title>
      </Head>
      <DocSection {...toc[0]}>
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
      <DocSection {...toc[1]}>
        <h2 className="section-title">Hello World (F#)</h2>
        <Code language="fsharp" value={
          preval`
          const fs = require('fs')
          const val = fs.readFileSync(__dirname + '/../../../examples/Logary.ConsoleApp/Program.fs', 'utf8')
          module.exports = val
          `
        } />
      </DocSection>
      <DocSection {...toc[2]}>
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