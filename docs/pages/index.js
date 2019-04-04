import Layout from '../components/Layout'
import Link from 'next/link'

export default function Index() {
  return <Layout>
    <p className="intro">
      Logary is a logging, tracing and metric library for .Net and JS as well as
      a stand-alone, cloud-native log router/ingress called Rutta.
    </p>

    <h3 id="where-to-go-next">Where to go next?</h3>

    <ul id="where-to-go-next-options">
      <Link href="/logary-js-quickstart">
        <li>
          <h5>Logary for JS Quickstart</h5>
          <p>Quickstart. A JavaScript library for your web/react-native apps. Learn how to add excellent analytics to your own mobile apps.</p>
        </li>
      </Link>

      <Link href="/logary-dotnet-quickstart">
        <li>
          <h5>Logary for .Net Core Quickstart</h5>
          <p>Quickstart. A .Net Core library for the enterprise/cloud native deployment.</p>
        </li>
      </Link>

      <Link href="/logary-dotnet-documentation">
        <li>
          <h5>Logary for .Net Core Documentation</h5>
          <p>Learn how you can log more effectively.</p>
        </li>
      </Link>

      <Link href="/logary-prometheus-dotnet">
        <li>
          <h5>Logary Prometheus for .Net Core</h5>
          <p>With <code>Logary.Prometheus</code> you can expose you app metrics for Prometheus and Grafana.</p>
        </li>
      </Link>

      <Link href="/logary-dotnet-targets">
        <li>
          <h5>Logary for .Net Core Targets</h5>
          <p>Logary can send logs, metrics and spans to a wide range of targets. Have a look here to see what's available!</p>
        </li>
      </Link>

      <Link href="/logary-rutta">
        <li>
          <h5>Logary Rutta</h5>
          <p>A stand-alone, cloud-native log router and ingestion point for HTTP, UDP, ZeroMQ, TCP, and more.</p>
        </li>
      </Link>

      <Link href="/pricing">
        <li>
          <h5>Pricing</h5>
          <p>Logary for .Net Core costs when used for commercial purposes. View what it would cost for your deployment.</p>
        </li>
      </Link>
    </ul>
  </Layout>
}