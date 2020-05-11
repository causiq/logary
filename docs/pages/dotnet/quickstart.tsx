import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import Code from '../../components/Code'
import DotnetIcon from '../../components/DotnetIcon'
import preval from 'babel-plugin-preval/macro'

export default function DOTNetQuickstart() {
  return (
    <DocPage
      name="dotnet-quickstart"
      title=".Net Quickstart"
      icon={<DotnetIcon width={58} />}
      colour="purple">

      <DocSection title='Getting started' id='getting-started'>
        <p>After installing the .NET Core SDK, open a command prompt. Type the following dotnet commands to create and run a C# application.</p>
        <p><code>$ dotnet new console -lang F#</code></p>
        <p><code>$ dotnet add package Logary</code></p>
        <p><code>$ dotnet run</code></p>
      </DocSection>

      <DocSection title='Hello world (F#)' id='hello-world-fsharp'>
        <Code language="fsharp" value={
          preval`
          const fs = require('fs')
          const val = fs.readFileSync(__dirname + '/../../../examples/Logary.ConsoleApp/Program.fs', 'utf8')
          module.exports = val
          `
        } />
      </DocSection>

      <DocSection title='Hello world (C#)' id='hello-world-csharp'>
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