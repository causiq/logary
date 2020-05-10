import { useRef } from 'react'
import Layout from '../components/Layout'
import Link from 'next/link'
import Head from 'next/head';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import DotnetIcon from '../components/DotnetIcon'
import PrometheusIcon from '../components/PrometheusIcon'
import {
  faFileChartLine, faLifeRing, faChevronDoubleDown, faCoins,
  faAtom, faBalanceScale, faBooks, faSatelliteDish, faBullseye
} from '@fortawesome/fontawesome-free'
import { faJs } from '@fortawesome/free-brands-svg-icons';
import ScrollLink from '../components/ScrollLink'

export default function Index() {
  const howToRef = useRef(null);

  return <Layout name="landing-page">
    <Head>
      <title>Logary v5 — Professional logging, metrics and analytics for your apps</title>
    </Head>
    <section className="cards-section text-center">
      <div className="container">
        <h2 className="title">Professional logging, metrics and analytics for your apps</h2>
        <div className="intro">
          <p>
            Would you like to <i>proactively handle any negative experiences</i> that your users' are having?<br />
            Maybe you're 🤒/😴 giving all your interaction data to large american corporations ®?<br />
            Or do you just want to measure <i>app latency</i> and <i>track revenue</i>?
          </p>

          <p><em>Then you've come to the right place, because Logary solves that for you.</em></p>

          <p>
            Logary is a logging, tracing and metric library for .Net and JS as well as
            a stand-alone, ☁-native log router/ingress called Rutta.
          </p>

          <p><ScrollLink targetRef={howToRef}>How does it work?</ScrollLink></p>

          <div className="cta-container">
            <a className="btn btn-primary btn-cta"
              href="https://www.nuget.org/packages/Logary/5.0.0-rc.10"
              target="_blank">
              <FontAwesomeIcon icon={faChevronDoubleDown} />
              .Net package
            </a>{' '}
            <a className="btn btn-primary btn-cta"
              href="https://www.npmjs.com/package/logary"
              target="_blank">
              <FontAwesomeIcon icon={faChevronDoubleDown} />
              JS package
            </a>
          </div>
        </div>

        <div id="cards-wrapper" className="cards-wrapper row">

          <div className="item item-yellow col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faJs} />
              </div>
              <h3 className="title">JS Quickstart</h3>
              <p className="intro">Learn how to add analytics to your web/react-native apps.</p>
              <Link href="/js/quickstart">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-purple item-2 col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <DotnetIcon width={58} />
              </div>
              <h3 className="title">.Net Core Quickstart</h3>
              <p className="intro">Instrument your backend services for a cloud native architecture.</p>
              <a className="link" href="/dotnet/quickstart"><span></span></a>
            </div>
          </div>

          <div className="item item-pink col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faBullseye} />
              </div>
              <h3 className="title">Targets / Sinks</h3>
              <p className="intro">
                Logary can send logs, metrics and spans to a wide range of targets. Have a look here to see what's available!
              </p>
              <Link href="/dotnet/targets">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-blue col-lg-4 col-6" ref={howToRef}>
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faAtom} />
              </div>
              <h3 id="how-does-it-work" className="title">How does it work?</h3>
              <p className="intro">Let's have a look at this site and how we do analytics here...</p>
              <Link href="/other/tutorial/how?">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-yellow item-2 col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faBooks} />
              </div>
              <h3 className="title">JS Documentation</h3>
              <p className="intro">Read up on how to use the JavaScript library</p>
              <Link href="/js/docs">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-purple item-2 col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faBooks} />
              </div>
              <h3 className="title">.Net Core Documentation</h3>
              <p className="intro">Read up on how to use the library</p>
              <Link href="/dotnet/docs">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-green col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faLifeRing} />
              </div>
              <h3 className="title">FAQs</h3>
              <p className="intro">Layout for FAQ page. Lorem ipsum dolor sit amet, consectetuer adipiscing elit</p>
              {/* <a className="link" href="/other/faqs"><span></span></a> */}
              <Link href="/other/faqs">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-primary col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faFileChartLine} />
              </div>
              <h3 className="title">Tutorials</h3>
              <p className="intro">Learn how to use Logary to visualise the state and interactions with your apps.</p>
              <Link href="/other/tutorial">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-orange col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faBalanceScale} />
              </div>
              <h3 className="title">License &amp; Credits</h3>
              <p className="intro">Layout for license &amp; credits page. Consectetuer adipiscing elit.</p>
              <Link href="/other/license">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-orange col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <PrometheusIcon />
              </div>
              <h3 className="title">Logary Prometheus for .Net Core</h3>
              <p className="intro">With <code>Logary.Prometheus</code> you can expose you app metrics for Prometheus and Grafana.</p>
              <Link href="/dotnet/prometheus">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-blue col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faSatelliteDish} />
              </div>
              <h3 className="title">Logary Rutta</h3>
              <p className="intro">
                A stand-alone, cloud-native log router and ingestion point for HTTP, UDP, ZeroMQ, TCP, and more.
              </p>
              <Link href="/rutta">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-yellow col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faCoins} />
              </div>
              <h3 className="title">Pricing</h3>
              <p className="intro">
                Purchase a commercial license to super-charge your company's logging, metrics and analytics, and improve Logary!
              </p>
              <Link href="/other/pricing">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

        </div>
      </div>
    </section>
  </Layout>
}