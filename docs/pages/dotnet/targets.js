import Link from 'next/link'
import Layout from '../../components/Layout'
import ConfTable from '../../components/ConfTable'

function GooglePubSub() {
  const conf = [
    { name: "projectId",
      type: 'string',
      required: false,
      default: 'None',
      description: 'What GCP project the PubSub broker is in',
    },
    { name: "topic",
      type: 'string | (Message->string)',
      default: '"logs"',
      required: false,
      description: "What topic to publish to. If you provide a callback, you can inspect the Message object and choose what topic you publish to. If that topic doesn't exist, it will be created."
    },
    { name: "shutdownTimeout",
      type: 'Duration',
      default: 'Duration.FromSeconds 5L',
      required: false,
      description: 'How long to wait for the publisher\'s buffers to flush before force-closing it',
    },
    { name: "pubSettings",
      type: 'PublisherServiceApiSettings',
      default: 'client-lib defaults',
      required: false,
      description: 'Sets the publisher settings'
    },
  ]

  return <article id="google-pubsub">
    <h3>Google Pub/Sub</h3>
    <p>
      Supports <a href="https://cloud.google.com/pubsub/docs/overview">Google Pub/Sub</a> as a target of Logary, Shipper and Rutta.
    </p>
    <h4>Configuration</h4>
    <ConfTable conf={conf} />
  </article>
}

export default function Targets() {
  return (
    <Layout>
      <h2>Targets ToC</h2>
      <dl>
        <dt>Google Pub/Sub</dt>
        <dd>
          <Link href="#google-pubsub">
            Docs
          </Link>
        </dd>
      </dl>
      <section id="target-docs">
        <GooglePubSub />
      </section>
    </Layout>
  )
}