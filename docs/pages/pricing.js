import { useRef } from 'react'
import Head from 'next/head';
import DocPage from '../components/DocPage'
import DocSection from '../components/DocSection'
import { faCoins } from '@fortawesome/pro-solid-svg-icons'
import { useState, useEffect } from 'react'
import InputRange from 'react-input-range'
import { StripeProvider, injectStripe } from 'react-stripe-elements'
import { Elements, CardElement } from 'react-stripe-elements';
import { Button } from 'reactstrap';
import "react-input-range/lib/css/index.css"

const createOptions = (fontSize, padding) => {
  return {
    style: {
      base: {
        fontSize,
        color: '#424770',
        letterSpacing: '0.025em',
        fontFamily: 'Source Code Pro, monospace',
        '::placeholder': {
          color: '#aab7c4',
        },
        padding,
      },
      invalid: {
        color: '#9e2146',
      },
    },
  };
};

const handleBlur = () => {
  console.log('[blur]');
};
const handleChange = (change) => {
  console.log('[change]', change);
};
const handleClick = () => {
  console.log('[click]');
};
const handleFocus = () => {
  console.log('[focus]');
};
const handleReady = () => {
  console.log('[ready]');
};

const CheckoutForm = injectStripe(({ stripe, total }) => {
  const handleSubmit = (ev) => {
    ev.preventDefault();
    if (stripe != null) {
      stripe
        .createToken()
        .then((payload) => console.log('[token]', payload))
    } else {
      console.log("Stripe.js hasn't loaded yet.")
    }
  }

  return (
    <form onSubmit={handleSubmit} className="Checkout">
      <label>
        Card details
        <CardElement
          onBlur={handleBlur}
          onChange={handleChange}
          onFocus={handleFocus}
          onReady={handleReady}
          {...createOptions(12)}
        />
      </label>
      <Button color='primary'>Pay {total} EUR</Button>
    </form>
  );
})

// https://codepen.io/davidchin/pen/GpNvqw
export default function Pricing() {
  const [ cores, setCores ] = useState(8);
  const [ devs, setDevs ] = useState(3);
  const [ stripe, setStripe ] = useState(null);

  useEffect(() => {
    if (typeof window !== 'undefined' && window != null) {
      const key =
        process.env.NODE_ENV === 'production'
          ? 'pk_live_CbFgv1mQ8yvjUrYWhtIR8awZ'
          : 'pk_test_OC5b0afrMpraAlH9frTen9C7';
      setStripe(window.Stripe(key));
    }
  }, [])

  const toc =
    [ { id: "calculator", title: "Calculator", ref: useRef(null) },
      { id: "purchase", title: "Purchase license", ref: useRef(null) },
    ]

  const continuousRebate = 0.6, // 60% off the next year
        total = cores * 100 + devs * 20 + 250,
        totalNextYear = total * (1 - continuousRebate);

  return (
    <DocPage name="pricing" title="Pricing" faIcon={faCoins} colour="yellow" toc={toc}>
      <Head>
        <title key="title">Logary — Pricing</title>
        <script id="stripe-js" src="https://js.stripe.com/v3/" async></script>
      </Head>
      <DocSection {...toc[0]}>
        <h2 className="section-title">Calculator</h2>
        <p>
          Logary's pricing is transparent. You don't have to sign up to a newsletter to know what it
          would cost you to run it for your for-profit service. Licenses are yearly and subscription
          based.
        </p>

        <p>
          You can load- and stress-test in a test-/staging-environment, for free, as long as that environment never serves production traffic.
        </p>

        <form>
          <section>
            <label htmlFor="cpu-cores">
              Number of cores in total production deployment
            </label>
            <InputRange id="cpu-cores"
              formatLabel={v => v <= 1 ? `${v} core` : `${v} cores`}
              maxValue={30} minValue={1} value={cores} onChange={v => setCores(v)} />
          </section>

          <section>
            <label htmlFor="developers">
              Number of developers owning/working on the software (seats)
            </label>
            <InputRange id="developers"
              formatLabel={v => v <= 1 ? `${v} developer` : `${v} developers`}
              maxValue={15} minValue={1} value={devs} onChange={v => setDevs(v)} />
          </section>

          <p className="total" style={{margin: '50px 0 0 0', fontWeight: 'bold'}}>
            Total first year: {total} EUR<br />
            Subsequent years: {totalNextYear} EUR
          </p>

          <p>
            Your license will be delivered by e-mail upon purchase.
          </p>
        </form>
      </DocSection>
      <DocSection {...toc[1]}>
        <h2 className="section-title">Purchase license</h2>
        <StripeProvider stripe={stripe}>
          <Elements>
            <CheckoutForm total={total} />
          </Elements>
        </StripeProvider>
      </DocSection>
    </DocPage>
  )
}