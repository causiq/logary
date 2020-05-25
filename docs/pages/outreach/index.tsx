import Calendly from '../../components/Calendly'
import OutreachLayout from '../../components/OutreachLayout'

export default function JSDocs() {
  return (
    <OutreachLayout
      title='Logary Analytics'
      style={{ backgroundColor: '#efffff'}}
      description='How can you improve your decision making process for your SaaS app or e-commerce site?'>
      <main>
        <h1>The big picture is blurry</h1>

        <p>
          Have you experienced the pain of <b>having many different tools</b>, those for marketing,
          further tools for website analytics and the then more tools that assist your ops/dev
          teams observe the system as it's running?
        </p>

        <p>
          We think there's a niche in between the three separate layers of your business; a tool that
          collects and highlights all of the below:
        </p>

        <ol>
          <li>ad/marketing and attribution data</li>
          <li>company-wide Objective Key Results (OKR:s)</li>
          <li>"dev/ops data" — the trinity of tracing, metrics, logs</li>
        </ol>

        <p>so that all three roles can <b>communicate around the same data.</b></p>

        <h2>Does this sound interesting to you?</h2>

        <p>Pick a date 🗓 for an intro; we're interested in your use-case. Below you find my calendar slots for a 20 minute meeting.</p>
        <Calendly />
      </main>

      <style global jsx>{`
        body { background-color: #EFFFFF; }
        .layout { padding: 2em; }
        main { max-width: 450px; margin: 0 auto;}
        h2 { margin: 2em 0 1em; }
        iframe { margin-top: 2em; }
        ol { font-size: 1.2em; }
        p {
          margin: 1.5em 0;
          font-size: 1.05rem;
        }
      `}</style>
    </OutreachLayout>
  )
}