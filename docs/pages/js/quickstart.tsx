import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import Code from '../../components/Code'

export default function JSQuickstart() {
  return (
    <DocPage name="js-quickstart" title="JavaScript Quickstart" colour="yellow">
      <DocSection title='Install the package' id='installation'>
        <p>
          The first step is to install the <code>Logary</code> package from npm.
        </p>
        <p>
          <code>yarn add logary @logary/plugin-browser @logary/plugin-nextjs @logary/plugin-node @logary/plugin-react</code> or with npm:
          <code>npm add logary @logary/plugin-browser @logary/plugin-nextjs @logary/plugin-node @logary/plugin-react</code>.
        </p>
        <p>
          Remove the ones that are irrelevant for you.
        </p>
      </DocSection>

      <DocSection title='How to use' id='how-to-use'>
        <Code language="js" value={`
        import { info } from 'logary'
        info("Hello world")
        `} />
        <p>You can spawn Rutta server-side as a docker container, to ingest logs:</p>
        <span className="_code">$ docker run -p 10001:10001 --rm -it haaf/rutta router --listener http 0.0.0.0:10001 json --target console://./</span>
        <p>You can choose between the different targets when forwarding the logs (see the main logary repo)</p>
      </DocSection>
    </DocPage>
  )
}