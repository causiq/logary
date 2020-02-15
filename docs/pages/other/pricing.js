import { useRef } from 'react'
import Head from 'next/head';
import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import { Form, Input } from 'reactstrap';
import { calculatePrice, ContinuousRebate, formatMoney } from '../../components/calculatePrice'
import { faCoins } from '@fortawesome/pro-solid-svg-icons'
import { useState, useEffect } from 'react'
import InputRange from 'react-input-range'
import { StripeProvider, injectStripe } from 'react-stripe-elements'
import { Elements, CardElement } from 'react-stripe-elements';
import { Button } from 'reactstrap';
import fetch from 'isomorphic-unfetch'
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

function newCustomer() {
  return {
    companyName: "",
    name: "",
    vatNo: "",
    email: ""
  }
}

const CheckoutForm = injectStripe(({ stripe, cores, devs, years }) => {
  const [ customer, setCustomer ] = useState(newCustomer());
  const [ error, setError ] = useState(null)
  const [ chargePending, setChargePending ] = useState(false)
  const [ chargeComplete, setChargeComplete ] = useState(false)
  const chargeVAT = customer.vatNo == null || customer.vatNo === ""
  const price = calculatePrice(cores, devs, years, ContinuousRebate, chargeVAT)

  const handleSubmit = async (ev) => {
    ev.preventDefault();

    if (stripe == null) {
      console.log("Stripe not loaded yet.")
      return;
    }

    setChargePending(true);

    const tr = await stripe.createToken({
      name: `${customer.companyName} c=${cores}, d=${devs}, y=${years}`
    });

    if (tr.token == null) {
      console.error("failure creating charge", tr.error)
      setError(tr.error)
      setChargePending(false)
      setChargeComplete(false)
      return;
    }

    const body = {
      token: tr.token,
      price,
      cores,
      devs,
      years,
      customer
    }

    console.log("Received token=", tr.token, "posting body=", body)

    const response = await fetch("/charge", {
      method: "POST",
      headers: {"Content-Type": "application/json; charset=utf-8"},
      body: JSON.stringify(body)
    });

    if (response.ok) setChargeComplete(true);
    else setError(null)
  }

  if (chargeComplete) {
    return (
      "Thank you for your purchase! We will e-mail you your license key!"
    )
  }

  return (
    <Form onSubmit={handleSubmit} className="Checkout">

      <Input
        className="StripeElement"
        placeholder="Company name"
        data-cy="customer-companyName"
        value={customer.companyName} onChange={e => setCustomer({
          ...customer,
          companyName: e.target.value
        })}
        required
      />

      <Input
        className="StripeElement"
        data-cy="customer-vatNo"
        placeholder="Your company's VAT No. (optional)"
        value={customer.vatNo} onChange={e => setCustomer({
          ...customer,
          vatNo: e.target.value
        })}
      />

      <Input
        className="StripeElement"
        data-cy="customer-name"
        placeholder="Your name"
        type="text"
        autoComplete="name"
        value={customer.name} onChange={e => setCustomer({
          ...customer,
          name: e.target.value
        })}
        required
      />

      <Input
        className="StripeElement"
        placeholder="Your e-mail"
        data-cy="customer-email"
        type="email"
        autoComplete="email"
        value={customer.email} onChange={e => setCustomer({
          ...customer,
          email: e.target.value
        })}
        required
      />

      <CardElement {...createOptions(12)} />

      {error != null
        ? <p className="text-danger">{error.message}</p>
        : null}

      <p>
        {price.chargeVAT
          ? <>
            Subtotal: {formatMoney(price.totalExclVAT)}
            <br/>VAT (25%): {formatMoney(price.totalVAT)}
            <br/>Total: {formatMoney(price.total)}
          </>
          : <>
            Total: {formatMoney(price.total)}
          </>}
      </p>

      <Button
        data-cy='pay'
        color='primary'
        disabled={price.total.amount <= 0 || !stripe || chargePending}>
        Pay {formatMoney(price.total)}
      </Button>
    </Form>
  );
})

function useStripe(setStripe) {
  const stripeKey = process.env.NODE_ENV === 'production' ? 'pk_live_jLZwxPipS9vhFeNXVjOXshuZ' : 'pk_test_9z9OjSCGtTSgPcj8nCZWNNUy';
  useEffect(() => {
    if (typeof window !== 'undefined' && window != null) {
      if (window.Stripe != null) {
        setStripe(window.Stripe(stripeKey));
      } else {
        document.querySelector('#stripe-js').addEventListener('load', () => {
          setStripe(window.Stripe(stripeKey));
        })
      }
    }
  }, [])
}

// https://stripe.com/docs/recipes/elements-react
// https://codepen.io/davidchin/pen/GpNvqw
export default function Pricing() {
  const [ cores, setCores ] = useState(8)
  const [ devs, setDevs ] = useState(3)
  const [ years, setYears ] = useState(2)
  const [ stripe, setStripe ] = useState(null)
  const price = calculatePrice(cores, devs, years, ContinuousRebate, false)
  useStripe(setStripe);

  const toc =
    [ { id: "calculator", title: "Calculator", ref: useRef(null) },
      { id: "purchase", title: "Purchase license", ref: useRef(null) },
    ]

  return (
    <StripeProvider stripe={stripe}>
      <DocPage name="pricing" title="Pricing" faIcon={faCoins} colour="primary" toc={toc}>
        <Head>
          <title key="title">Logary — Pricing</title>
          <script id="stripe-js"  key="stripe" src="https://js.stripe.com/v3/" async></script>
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

            <section>
              <label htmlFor="years">
                Number of years to buy a license for
              </label>
              <InputRange id="years"
                formatLabel={v => v <= 1 ? `${v} year` : `${v} years`}
                maxValue={15} minValue={1} value={years} onChange={v => setYears(v)} />
            </section>

            <p className="total" style={{margin: '50px 0 0 0', fontWeight: 'bold'}}>
              Total (for {years} years): {formatMoney(price.totalExclVAT)}
            </p>
            <p>
              Subsequently: {formatMoney(price.nextYear)}/year
            </p>

            <p>
              Your license will be delivered by e-mail 📧 upon purchase.
            </p>
          </form>
        </DocSection>
        <DocSection {...toc[1]}>
          <h2 className="section-title">Purchase license</h2>
          <p>
            This form reflects your selection above. You're purchasing
            a license for {cores} cores, {devs} developers for {years} years. This sets up
            a subscription, so that after {years === 1 ? "the first year" : `${years} years`}, you'll
            be charged {formatMoney(price.nextYear)} at the beginning of the year.
          </p>
          <Elements>
            <CheckoutForm
              devs={devs}
              cores={cores}
              years={years} />
          </Elements>
        </DocSection>
      </DocPage>
    </StripeProvider>
  )
}
