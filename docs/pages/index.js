import { useRef } from 'react'
import Layout from '../components/Layout'
import Link from 'next/link'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import {
  faFileChartLine,
  faLifeRing,
  faChevronDoubleDown,
  faCoins
} from '@fortawesome/pro-solid-svg-icons'
import DotnetIcon from '../components/DotnetIcon'
import PrometheusIcon from '../components/PrometheusIcon'
import { faAtom, faBalanceScale, faBooks, faSatelliteDish, faPuzzlePiece } from '@fortawesome/pro-light-svg-icons'
import { faJs } from '@fortawesome/free-brands-svg-icons';
import Head from 'next/head';
import ScrollLink from '../components/ScrollLink'

export default function Index() {
  const howToRef = useRef(null);

  return <Layout name="landing-page">
    <Head>
      <title>Logary — Professional logging, metrics and analytics for your apps</title>
    </Head>
    <section className="cards-section text-center">
      <div className="container">
        <h2 className="title">Professional logging, metrics and analytics for your apps</h2>
        <div className="intro">
          <p>
            Logary is a logging, tracing and metric library for .Net and JS as well as
            a stand-alone, ☁-native log router/ingress called Rutta.
          </p>

          <div className="cta-container">
            <a className="btn btn-primary btn-cta"
              href="https://www.nuget.org/packages/Logary/5.0.0-rc.9"
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

          <p>
            Maybe you're 🤒/😴 giving all your customers' interaction data to large american corporations?
            Then you've come to the right place.
            Scroll to <ScrollLink targetRef={howToRef}>how does it work?</ScrollLink> to learn more.
          </p>
        </div>

        <div id="cards-wrapper" className="cards-wrapper row">

          <div className="item item-yellow col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faJs} />
              </div>
              <h3 className="title">JS Quickstart</h3>
              <p className="intro">Learn how to add analytics to your web/react-native apps.</p>
              <Link href="/logary-js-quickstart">
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
              <a className="link" href="/logary-dotnet-quickstart"><span></span></a>
            </div>
          </div>

          <div className="item item-blue col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faPuzzlePiece} />
              </div>
              <h3 className="title">Targets</h3>
              <p className="intro">
                Logary can send logs, metrics and spans to a wide range of targets. Have a look here to see what's available!
              </p>
              <Link href="/logary-dotnet-targets">
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
              <Link href="/tutorials/how-does-logary-work?">
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
              <Link href="/logary-js-documentation">
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
              <Link href="/logary-dotnet-documentation">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-pink col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faLifeRing} />
              </div>
              <h3 className="title">FAQs</h3>
              <p className="intro">Layout for FAQ page. Lorem ipsum dolor sit amet, consectetuer adipiscing elit</p>
              <a className="link" href="faqs.html"><span></span></a>
            </div>
          </div>

          <div className="item item-primary col-lg-4 col-6">
            <div className="item-inner">
              <div className="icon-holder">
                <FontAwesomeIcon icon={faFileChartLine} />
              </div>
              <h3 className="title">Tutorials</h3>
              <p className="intro">Learn how to use Logary to visualise the state and interactions with your apps.</p>
              <Link href="/tutorials/visualise">
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
              <Link href="/pricing">
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
              <p className="intro">With <em>Logary.Prometheus</em> you can expose you app metrics for Prometheus and Grafana.</p>
              <Link href="/logary-prometheus-dotnet">
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
              <Link href="/logary-rutta">
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
              <Link href="/pricing">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

        </div>
      </div>
    </section>
    <ul id="where-to-go-next-options">

    </ul>
  </Layout>
}