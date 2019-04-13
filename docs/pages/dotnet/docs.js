import { useRef } from 'react'
import Head from 'next/head';
import { faJs } from '@fortawesome/free-brands-svg-icons';
import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'

export default function DotnetDocs() {
  const toc =
    [ { id: "TODO", title: "TODO", ref: useRef(null) } ]

  return (
    <DocPage name="js-quickstart" title="TODO" faIcon={faJs} colour="yellow" readingMinutes={2} toc={toc}>
      <Head>
        <title key="title">Logary — TODO</title>
      </Head>
      <DocSection {...toc[0]}>
        <h2 className="section-title">TODO</h2>
      </DocSection>
    </DocPage>
  )
}