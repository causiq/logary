import { useRef } from 'react'
import Head from 'next/head';
import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import Code from '../../components/Code'
import DotnetIcon from '../../components/DotnetIcon'
import preval from 'babel-plugin-preval/macro'

export default function DOTNetQuickstart() {
  const toc =
    [
      { id: "create-application", title: "Create your first application", ref: useRef(null) },
      { id: "install-the-package", title: "Install the package", ref: useRef(null) },
      { id: "hello-fsharp", title: "Hello F#", ref: useRef(null) },
      { id: "hello-csharp", title: "Hello C#", ref: useRef(null) },
    ]

  return (
    <DocPage
      name="dotnet-quickstart"
      title=".Net Quickstart"
      icon={<DotnetIcon width={58} />}
      colour="purple"
      readingMinutes={4}
      toc={toc}>

      <Head>
        <title key="title">Logary — .Net Quickstart</title>
      </Head>

      <DocSection {...toc[0]}>
        <h2 className="section-title">Create your first application</h2>
        <p>
          After installing the .NET Core SDK, open a command prompt. Type the following dotnet commands to create and run a C# application.
        </p>
        <p><code>$ dotnet new console -lang F#</code></p>
        <p><code>$ dotnet add package Logary</code></p>
        <p><code>$ dotnet run</code></p>
        <p><code>Hello World!</code></p>
      </DocSection>

      <DocSection {...toc[1]}>
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