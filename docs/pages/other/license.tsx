
import { useRef } from 'react'
import { faBalanceScale } from '@fortawesome/fontawesome-free';
import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'

export default function License() {

  const toc = [
    { id: "logary-licencing", title: "Logary licencing", ref: useRef(null) } ,
    { id: "terms-of-service", title: "Terms of Service (ToS)", ref: useRef(null) } ,
    { id: "commercial-license", title: "Commercial License", ref: useRef(null) }
  ]

  return (
    <DocPage name="all-target" title="License" faIcon={faBalanceScale} colour="orange" readingMinutes={1} toc={toc}>

      <DocSection {...toc[0]}>
        <h2 className="section-title">Logary licencing</h2>
        <p></p>
        <p>This section defines the licenses for each subcomponent of Logary.</p>
        <h3>Logary Library (Logary-lib)</h3>
          <p>Present at src/Logary[,Tests,PerfTests] and src/adapters/*. Each being a .Net library that you link your own code/software to.</p>
          <p>Non-profits and non-profit projects are free to use without payment, subject to the Terms of Service defined below.</p>
          <p>For-profit or commercial entities must purchase licenses to use Logary for their production environments, subject to the Terms of Service defined below.</p>
        <h3>Logary Dash</h3>
          <p>Present at src/services/Dash and src/targets/Logary.Targets.SSE; a web app and an EventSource/SSE based target.</p>
          <p>Non-profits and non-profit projects are free to use without payment, subject to the Terms of Service defined below.</p>
          <p>For-profit or commercial entities must purchase licenses to use Dash for their production environments, subject to the Terms of Service defined below.</p>
        <h3>Targets</h3>
          <p>Present at src/targets/*, being .Net libraries that you link your own software to, or add as plugins to Rutta.</p>
          <p>Targets incorporated into the Logary code-base cannot be used without using the Logary library, but their code is free to modify, extend and sell under the Apache 2.0 license, unless specified otherwise on a per target basis (see below).</p>
          <p>The following targets' licenses must always be purchased, independent of the for-profit status of the purchasing entity:</p>
          <ul>
            <li>Mixpanel (under <span className="_code"> src/targets/Logary.Targets.Mixpanel </span>) — commercial user analytics, segmentation</li>
            <li>Opsgenie (under <span className="_code"> src/targets/Logary.Targets.Opsgenie) </span> — commercial alerting and monitoring</li>
          </ul>
          <p>All other targets (and their code-bases) are licensed as Apache 2.0 (Appendix A contains links to Apache 2.0).</p>
        <h3>Rutta</h3>
          <p>Present at src/services/*, being an executable program and docker container.</p>
          <p>Rutta is dual-licensed as GPL v3, or as a commercial license. Purchasing a commercial license for Rutta lets you use and extend its source code for your own commercial purposes and not open source that change.</p>
          <p>Present at src/services/*, being an executable program and docker container.</p>
          <p>GPL v3 means that when you want to extend Rutta for your own purposes, you must send a pull request with that change in order to make your improvement available for others to use. This is so that free software can continue to be free. If you don't want to send a pull request with your change, you must purchase the commercial license.</p>
        <h3>Library Façade</h3>
          <p>Present at <span className="_code"> src/Logary[,CSharp].Facade[,.Tests] </span>. Also called the Façade, in short.</p>
          <p>The Façade is Apache 2.0 licensed.</p>
          <p>The Library Façade is distinct from the Façade Adapter, since the Façade is Apache 2.0-licensed code, whilst the Façade Adapter (in <span className="_code"> src/adapters/Logary.Adapters.Facade </span>) doesn't work without Logary-lib.</p>
        <h3><span className="_code"> logary </span> JavaScript library</h3>
          <p>MIT-licensed, see <a href="https://github.com/logary/logary-js/blob/master/LICENSE">logary/logary-js/LICENSE.</a> </p>
          <p>This NPM package is published at <a href="https://www.npmjs.com/package/logary">logary/logary-js/LICENSE.</a> </p>
      </DocSection>

      <DocSection {...toc[1]}>
        <h2 className="section-title">Terms of Service (ToS)</h2>
        <p></p>
        <h3>1. Jurisdiction</h3>
          <p>This software's license shall be interpreted in accordance with the laws of Sweden.</p>
        <h3>2. Submission of Contributions</h3>
          <p>2.1. When you (Contributor) submit a pull request or otherwise seek to include your code in the Logary project, you waive all your intellectual property rights, including your copyright and patent claims for the submission. You agree that the modification that you submit may be relicensed as Logary's authors see fit.</p>
          <p>2.2. Unless otherwise stated by Logary's authors, your contribution will be licensed as per the component it is for.</p>
        <h3>3. Limitation of Warranty</h3>
          <p>3.1. Logary and all of its services, adapters, libraries and related software are provided ‘as is’ and ‘as available’ without warranty of any kind. Your use of the software is solely your responsibility. Logary's authors do not grant you any warranties, express or implied or otherwise, as to the accessibility, quality, fitness for any particular purpose, suitability or accuracy of the software, to the extent permitted by applicable law.</p>
          <p>3.2. We advice you to ascertain the fitness and functionality of the software yourself by fully testing it for your purposes and reading its source code.</p>
        <h3>4. Limitation of Liability</h3>
          <p>4.1. To the extent permitted by applicable law, Logary's authors are not liable for any direct nor indirect loss suffered by you, unless they has been found guilty of gross negligence. This limitation of liability includes, but is not limited to, loss of production or sales, loss of profit and cost of capital. Logary's author's liability under this section shall, in any event, be limited to an amount of half a price base amount according to the 1962:381 law about common insurance (Sv: Lagen om Allmänna försäkringar).</p>
        <h3>5. Indemnification</h3>
          <p>You will indemnify and hold Logary's authors harmless from any claim or demand, including reasonable attorneys' fees, made by any third party due to or arising out of your breach of these Terms, our policies, or your violation of any law or the rights of a third party, to the extent permitted by applicable law.</p>
        <h3>6. Collection of statistics / environment data</h3>
          <p>You agree that Logary may collect statistics on its usage and runtime environment (operating system name and version, IP-address, the hostname that is running Logary components, linked library identifiers, etc), by means of communication with Logary authors' servers.</p>
        <h3>7. Changes to the terms</h3>
          <p>Logary's authors may make changes to these Terms from time to time. When we introduce changes we will commit the most current version to this document, and if a revision of the Terms is material and you have made a purchase of the commercial license, we will notify you of the new Terms (either by e-mail or a Github notification) as appropriate. If you do not agree to the modified terms, you should discontinue your use of Logary.</p>
      </DocSection>
      <DocSection {...toc[2]}>
        <h2 className="section-title">Commercial License</h2>
        <p></p>
        <p>This section defines the terms of the Commercial License.</p>
        <p>The Commercial License is subject to the Terms of Service (ToS) above. It applies to a Work or Software, its source code and the running/deployment of the software.</p>

        <h3>1. Pricing</h3>
        <p>The commercial license must be purchased in order to apply to the Software.</p>
          <h4>1.1 Initial purchase</h4>
          <p>The price in EUR (€) is calculated as follows:</p>
          <span className="_code">price = C * 100 + D * 20 + 250</span>
          <p>where</p>
          <ul>
            <li>C: # of cores in total production deployment</li>
            <li>D: # of developers owning/working on the software</li>
          </ul>
          <p>You can load- and stress-test in a test-/staging-environment, for free, as long as that environment never serves production traffic.</p>
          <p>If you change the number of cores you deploy on, or number of developers, please order a delta to your license.</p>

          <h4>1.2 Subsequent years</h4>
          <p>You must renew the license(s) yearly, at a 60% discount of the initial price that a new customer would pay at the instant when the license expires.</p>
          <p>So for example, if you have 5 different services, each with access to 10 cores, and your team is 5 backend and 2 frontend developers, you pay 10 * 100 + 5 * 20 + 250 = €1,350 euro the first year, and €540 every subsequent year if the number of cores used in production and the number of developers "owning"/building the software is the same.</p>

          <h4>1.3 Means of Payment</h4>
          <p>You may choose to pay via invoice (SWIFT/IBAN bank transfer, or via a lightning network invoice), or through credit/debit card.</p>

          <h4>1.4 Terms of Payment</h4>
          <p></p>
          <h5>1.4.1 Invoice</h5>
          <p>The invoice must be paid within 10 days of its issuance.</p>
          <h5>1.4.2 Credit/Debit Card</h5>
          <p>Credit/Debit card payments are immediate.</p>

        <h3>2. Allowances</h3>
          <p>By purchasing this license you may make changes to the Software without open sourcing or sending a pull request with those changes. Furthermore, you are free to link the Software to your own software, under the terms of this license.</p>
        <h3>3. Refunds</h3>
          <p>Unless expressly agreed in writing, before the purchase, there are no refunds, to the extent of the applicable law. If you are purchasing an individual, you have 14 days right to withdrawal according to <a href="https://www.konsumenteuropa.se/en/topics/e-commerce/E-commerce-within-the-EU/right-of-withdrawal-within-the-eu/">Swedish law.</a> </p>
        <h3>4. Order a license</h3>
          <p>Send an e-mail to <span className="_code"> henrik@haf.se </span> with the number of cores and developers you'd like to purchase a license for.</p>
      </DocSection>
    </DocPage>
  )
}
