import { useRef } from 'react'
import Head from 'next/head';
import DocPage from '../components/DocPage'
import DocSection from '../components/DocSection'
import { faCoins } from '@fortawesome/pro-solid-svg-icons'
import { useState } from 'react'
import InputRange from 'react-input-range'
import "react-input-range/lib/css/index.css"

// https://codepen.io/davidchin/pen/GpNvqw
export default function Pricing() {
  const [ cores, setCores ] = useState(8);
  const [ devs, setDevs ] = useState(3);

  const toc =
    [ { id: "calculator", title: "Calculator", ref: useRef(null) },
      { id: "purchase", title: "Purchase license", ref: useRef(null) },
    ]

  const continuousRebate = 0.6; // 60% off the next year

  return (
    <DocPage name="pricing" title="Pricing" faIcon={faCoins} colour="yellow" toc={toc}>
      <Head>
        <title key="title">Logary — Pricing</title>
      </Head>
      <DocSection {...toc[0]}>
        <h2 className="section-title">Calculator</h2>
        <p>
          Logary's pricing is transparent. You don't have to sign up to a newsletter to know what it
          would cost you to run it for your for-profit service. Licenses are yearly and subscription
          based.
        </p>

        <p>
          You can load- and stress-test in a test-/staging-environment, for free, as long as that environment never serves production traffic.
        </p>

        <form>
          <p>
            <label for="cpu-cores">
              Number of cores in total production deployment
            </label>
            <InputRange id="cpu-cores"
              formatLabel={v => v <= 1 ? `${v} core` : `${v} cores`}
              maxValue={30} minValue={1} value={cores} onChange={v => setCores(v)} />
          </p>

          <p>
            <label for="developers">
              Number of developers owning/working on the software (seats)
            </label>
            <InputRange id="developers"
              formatLabel={v => v <= 1 ? `${v} developer` : `${v} developers`}
              maxValue={15} minValue={1} value={devs} onChange={v => setDevs(v)} />
          </p>

          <p className="total" style={{margin: '50px 0 0 0', fontWeight: 'bold'}}>
            Total first year: {cores * 100 + devs * 20 + 250} EUR<br />
            Subsequent years: {(cores * 100 + devs * 20 + 250) * (1.0 - continuousRebate)} EUR
          </p>

          <p>
            Your license will be delivered by e-mail upon purchase.
          </p>
        </form>
      </DocSection>
      <DocSection {...toc[1]}>
        <h2 className="section-title">Purchase license</h2>

      </DocSection>
    </DocPage>
  )
}