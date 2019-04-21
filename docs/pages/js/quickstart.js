import { useRef } from 'react'
import Head from 'next/head';
import { faJs } from '@fortawesome/free-brands-svg-icons';
import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import Code from '../../components/Code'

export default function JSQuickstart() {
  const toc =
    [
      { id: "install-the-package", title: "Install the package", ref: useRef(null) },
      { id: "how-to-use", title: "How to use", ref: useRef(null) }
    ]

  return (
    <DocPage name="js-quickstart" title="JavaScript Quickstart" faIcon={faJs} colour="yellow" readingMinutes={2} toc={toc}>
      <Head>
        <title key="title">Logary — Javacript Quickstart</title>
      </Head>
      <DocSection {...toc[0]}>
        <h2 className="section-title">Install the package</h2>
        <p>
          The first step is to install the Logary package from npm.
        </p>
        <p>
          <code>npm install --save logary</code>
        </p>
        <p>Or you're using yarn...</p>
        <p>
          <code>yarn add logary</code>
        </p>
      </DocSection>
      <DocSection {...toc[1]}>
        <h2 className="section-title">How to use</h2>
        <Code language="js" value={
              preval`
              const fs = require('fs')
              const val = fs.readFileSync(__dirname + '/../../../examples/JS_QuickStart/Doc1.fs', 'utf8')
              module.exports = val
              `
            } />
        <p>You can spawn Rutta server-side as a docker container, to ingest logs:</p>
        <span className="_code">$ docker run -p 10001:10001 --rm -it haaf/rutta router --listener http 0.0.0.0:10001 json --target console://./</span>
        <p>You can choose between the different targets when forwarding the logs (see the main logary repo)</p>
      </DocSection>
    </DocPage>
  )
}