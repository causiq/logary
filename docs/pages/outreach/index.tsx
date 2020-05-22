import Calendly from '../../components/Calendly'
import OutreachLayout from '../../components/OutreachLayout'

export default function JSDocs() {
  return (
    <OutreachLayout
      title='Logary Analytics'
      style={{ backgroundColor: '#efffff'}}
      description='How can you improve your decision making process for your SaaS app or e-commerce?'>
      <main>
        <h1>Heard of the OODA loop?</h1>
        <h2>Find out ✨</h2>
        <p>Most dev teams have logs are are en-route to full observability 👀; this lets them do the "observe" bit of the OODA loop.</p>
        <p>We want teams to be better at the "orient" 🧠 bit of the loop; to understand before deciding. We want to go beyond showing aggregated data.</p>
        <p>Collaborate with your Product Manager (🥩); we want to build logs, metrics and distributed tracing into a single product-metrics oriented product.</p>
        <p>Pick a date 🗓 for an intro; we're interested in your use-case, below you find my calendar slots for a 20 minute meeting.</p>
        <Calendly />
      </main>
      <style global jsx>{`
        body { background-color: #EFFFFF; }
        .layout { padding: 2em; }
        main { max-width: 450px; margin: 0 auto;}
        h2 { margin-bottom: 2em; }
        iframe { margin-top: 2em; }
        p {
          margin: 1.5em 0;
          font-size: 1.05rem;
        }
      `}</style>
    </OutreachLayout>
  )
}