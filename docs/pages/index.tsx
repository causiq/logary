import { useRef } from 'react'
import Layout from '../components/Layout'
import Link from 'next/link'
import DotnetIcon from '../components/DotnetIcon'
import PrometheusIcon from '../components/PrometheusIcon'
import ScrollLink from '../components/ScrollLink'

export default function Index() {
  const howToRef = useRef(null)

  return <Layout name="landing-page" title='Professional logging, metrics and analytics for your apps'>
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
            <a className="btn btn-primary btn-cta arrow-down-circle"
              href="https://www.nuget.org/packages/Logary"
              target="_blank"
              data-track='Click .NET package CTA'>
              .Net package
            </a>{' '}

            <a className="btn btn-primary btn-cta"
              href="https://www.npmjs.com/package/logary"
              target="_blank"
              data-track='Click JS/TS package CTA'>
              JS/TS package
            </a>
          </div>
        </div>

        <div id="cards-wrapper" className="cards-wrapper row">
          <div className="item item-yellow col-lg-4 col-6" data-track='Card: JS Quickstart'>
            <div className="item-inner">
              <div className="icon-holder directions" />
              <h3 className="title">JS Quickstart</h3>
              <p className="intro">Learn how to add analytics to your web/react-native apps.</p>
              <Link href="/js/quickstart">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-purple item-2 col-lg-4 col-6" data-track='Card: .NET Quickstart'>
            <div className="item-inner">
              <div className="icon-holder">
                <DotnetIcon width={58} />
              </div>
              <h3 className="title">.Net Core Quickstart</h3>
              <p className="intro">Instrument your backend services for a cloud native architecture.</p>
              <a className="link" href="/dotnet/quickstart"><span></span></a>
            </div>
          </div>

          <div className="item item-pink col-lg-4 col-6" data-track='Card: Targets'>
            <div className="item-inner">
              <div className="icon-holder paper-plane" />
              <h3 className="title">Targets</h3>
              <p className="intro">
                Logary can send logs, metrics and spans to a wide range of targets. Have a look here to see what's available!
              </p>
              <Link href="/dotnet/targets">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-blue col-lg-4 col-6" ref={howToRef} data-track='Card: How does it work'>
            <div className="item-inner">
              <div className="icon-holder eyeglass" />
              <h3 id="how-does-it-work" className="title">How does it work?</h3>
              <p className="intro">Let's have a look at this site and how we do analytics here...</p>
              <Link href="/other/tutorial/how?">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-yellow item-2 col-lg-4 col-6" data-track='Card: JS/TS documentation'>
            <div className="item-inner">
              <div className="icon-holder diamond" />
              <h3 className="title">JS/TS Documentation</h3>
              <p className="intro">Read up on how to use the JavaScript library</p>
              <Link href="/js/docs">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-purple item-2 col-lg-4 col-6" data-track='Card: .NET documentation'>
            <div className="item-inner">
              <div className="icon-holder badge" />
              <h3 className="title">.Net Core Documentation</h3>
              <p className="intro">Read up on how to use the library</p>
              <Link href="/dotnet/docs">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-green col-lg-4 col-6" data-track='Card: FAQs'>
            <div className="item-inner">
              <div className="icon-holder">
              </div>
              <h3 className="title">FAQs</h3>
              <p className="intro">Layout for FAQ page. Lorem ipsum dolor sit amet, consectetuer adipiscing elit</p>
              {/* <a className="link" href="/other/faqs"><span></span></a> */}
              <Link href="/other/faqs">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-primary col-lg-4 col-6" data-track='Card: Tutorials'>
            <div className="item-inner">
              <div className="icon-holder">
              </div>
              <h3 className="title">Tutorials</h3>
              <p className="intro">Learn how to use Logary to visualise the state and interactions with your apps.</p>
              <Link href="/other/tutorial">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-orange col-lg-4 col-6" data-track='Card: License'>
            <div className="item-inner">
              <div className="icon-holder">
              </div>
              <h3 className="title">License &amp; Credits</h3>
              <p className="intro">Layout for license &amp; credits page. Consectetuer adipiscing elit.</p>
              <Link href="/other/license">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-orange col-lg-4 col-6" data-track='Card: Prometheus for .NET'>
            <div className="item-inner">
              <div className="icon-holder">
                <PrometheusIcon />
              </div>
              <h3 className="title">Logary Prometheus for .NET</h3>
              <p className="intro">With <code>Logary.Prometheus</code> you can expose you app metrics for Prometheus and Grafana.</p>
              <Link href="/dotnet/prometheus">
                <a className="link"><span></span></a>
              </Link>
            </div>
          </div>

          <div className="item item-blue col-lg-4 col-6" data-track='Card: Rutta'>
            <div className="item-inner">
              <div className="icon-holder">
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

          <div className="item item-yellow col-lg-4 col-6" data-track='Card: Pricing'>
            <div className="item-inner">
              <div className="icon-holder">
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