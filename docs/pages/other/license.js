
import { useRef } from 'react'
import Head from 'next/head';
import { faBalanceScale } from '@fortawesome/pro-light-svg-icons';
import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import Code from '../../components/Code'
var low = require('lowlight')
var tree = low.highlight('js', '"use strict";').value

// import stn from '../../images/SinkTargetNames.png'




export default function License() {

  const toc =
  [ 
    { id: "license", title: "License", ref: useRef(null) },
  ]
  
  return (
    <DocPage name="all-target" title="License" faIcon={faBalanceScale} colour="orange" readingMinutes={1} toc={toc}>
      <Head>
        <title key="title">License</title>
      </Head>
      <DocSection {...toc[0]}>
        <a href="https://github.com/logary/logary/blob/master/LICENSE.md">GPL v3, Apache 2.0 and MIT depending on library</a>
      </DocSection>
    </DocPage>
  )
}
