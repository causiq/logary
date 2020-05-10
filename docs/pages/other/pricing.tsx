import React, { useState, useRef, useCallback, useMemo, ReactElement, FormEvent } from 'react'
import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import { Input, Button } from 'reactstrap'
import { faCoins } from '@fortawesome/fontawesome-free'
import InputRange from 'react-input-range'
import fetch from 'isomorphic-unfetch'
import { Elements, CardElement, useStripe, useElements } from '@stripe/react-stripe-js'
import { loadStripe, StripeError, StripeCardElementChangeEvent, PaymentMethod } from '@stripe/stripe-js'
import { calculatePrice, ContinuousRebate, formatMoney } from '../../components/calculatePrice'
import { ChargeRequest, Customer } from '../api/charge'

const emptyCustomer: Customer = { companyName: "", name: "", vatNo: "", email: "" }

type Status =
  | { type: 'initial' }
  | { type: 'error', error: StripeError }
  | { type: 'card-filled-out' }
  | { type: 'pending-paymentMethod' }
  | { type: 'pending-charge', paymentMethod: PaymentMethod }
  | { type: 'completed' }

const initialStatus: Status = { type: 'initial' }

function nextFromCardEvent(_: Status, event: StripeCardElementChangeEvent): Status {
  if (event.error != null) {
    return {
      type: 'error',
      error: event.error
    }
  }

  if (event.complete) {
    return {
      type: 'card-filled-out'
    }
  }

  return {
    type: 'initial'
  }
}

type CheckoutFormProps = Readonly<{ cores: number; devs: number; years: number }>

const useEffect = typeof window === 'undefined' ? React.useEffect : React.useLayoutEffect

const CheckoutForm = ({ cores, devs, years }: CheckoutFormProps): ReactElement => {
  const stripe = useStripe()
  const elements = useElements()

  const [ customer, setCustomer ] = useState(emptyCustomer)
  const [ status, setStatus ] = useState<Status>(initialStatus)

  const chargeVAT = customer.vatNo == null || customer.vatNo === ""
  const price = useMemo(() => calculatePrice(cores, devs, years, ContinuousRebate, chargeVAT), [ cores, devs, years ])

  const handleCardChange = useCallback((e: StripeCardElementChangeEvent) => setStatus(nextFromCardEvent(status, e)), [ status, setStatus ])

  useEffect(() => {
    if (status.type === 'error') {
      // @ts-ignore magic
      elements.getElement('card').focus()
    }
  }, [ status ])

  const handleSubmit = useCallback(async (ev: FormEvent<HTMLFormElement>) => {
    console.log('handling submit')
    ev.preventDefault()

    if (stripe == null || elements == null) {
      console.log("Stripe not loaded yet.")
      return
    }

    if (status.type === 'error') return

    if (status.type === 'card-filled-out') {
      setStatus({ type: 'pending-paymentMethod' })
    }

    const cardE = elements.getElement(CardElement)

    if (cardE == null) {
      throw new Error('elements.getElement(CardElement) => null')
    }

    const { error, paymentMethod } = await stripe.createPaymentMethod({
      type: 'card',
      card: cardE,
      billing_details: {
        email: customer.email,
        name: customer.name
      },
      metadata: {
        cores, devs, years,
        timestamp: Date.now(),
        clientTime: new Date().toString(),
        vatNo: customer.vatNo,
        companyName: customer.companyName
      }
    })

    if (error != null) {
      setStatus({ type: 'error', error })
      return
    }
    else if (paymentMethod != null) {
      setStatus({ type: 'pending-charge', paymentMethod })
      // continue
    }
    else {
      throw new Error('Stripe\'s TypeScript types indicate paymentMethod and also error can be null, but IRL it should be either or')
      // crash
    }

    const body: ChargeRequest = {
      paymentMethod,
      price,
      cores,
      devs,
      years,
      customer
    }

    const response = await fetch('/api/charge', {
      method: 'POST',
      headers: { "content-type": "application/json; charset=utf-8" },
      body: JSON.stringify(body)
    });

    const responseBody = await response.json()

    if (response.ok) setStatus({ type: 'completed' })
    else setStatus({ type: 'error', error: responseBody.payload })
  }, [ stripe, elements, status, setStatus ])

  if (status.type === 'completed') {
    return <>
      Thank you for your purchase! We will e-mail you your license key!
    </>
  }

  return (
    <form onSubmit={handleSubmit} className="Checkout">

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

      <CardElement
        options={{
          iconStyle: 'solid',
          style: {
            base: {
              fontSize: '12px',
              color: '#424770',
              letterSpacing: '0.025em',
              fontFamily: 'Source Code Pro, monospace',
              '::placeholder': {
                color: '#aab7c4',
              },
            },
            invalid: {
              color: '#9e2146',
            },
          }
        }}
        onChange={handleCardChange}/>

      {status.type === 'error'
        ? <p className="text-danger">{status.error.message}</p>
        : null}

      <p>
        {price.chargeVAT
          ? <>
            Subtotal: {formatMoney(price.totalExclVAT)}
            <br />VAT (25%): {formatMoney(price.totalVAT)}
            <br />Total: {formatMoney(price.total)}
          </>
          : <>
            Total: {formatMoney(price.total)}
          </>}
      </p>

      <Button
        type='submit'
        data-cy='pay'
        color='primary'
        disabled={
          price.total.amount <= 0
          || !stripe
          || status.type === 'pending-charge'
          || status.type === 'pending-paymentMethod'}>
        {status.type.indexOf('pending-') !== -1
          ? 'Processing...'
          : `Pay ${formatMoney(price.total)}`}
      </Button>
    </form>
  )
}

const stripeP = loadStripe(process.env.NODE_ENV === 'production' ? 'pk_live_jLZwxPipS9vhFeNXVjOXshuZ' : 'pk_test_9z9OjSCGtTSgPcj8nCZWNNUy')

export default function Pricing() {
  const [cores, setCores] = useState(8)
  const [devs, setDevs] = useState(3)
  const [years, setYears] = useState(2)
  const price = calculatePrice(cores, devs, years, ContinuousRebate, false)

  const toc =
    [{ id: "calculator", title: "Calculator", ref: useRef(null) },
    { id: "purchase", title: "Purchase license", ref: useRef(null) },
    ]

  return (
    <Elements stripe={stripeP}>
      <DocPage name="pricing" title="Pricing" faIcon={faCoins} colour="primary" toc={toc}>
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
              <InputRange
                formatLabel={v => v <= 1 ? `${v} core` : `${v} cores`}
                maxValue={30}
                minValue={1}
                value={cores}
                onChange={v => typeof v === 'number' ? setCores(v) : null} />
            </section>

            <section>
              <label htmlFor="developers">
                Number of developers owning/working on the software (seats)
              </label>
              <InputRange
                formatLabel={v => v <= 1 ? `${v} developer` : `${v} developers`}
                maxValue={15}
                minValue={1}
                value={devs}
                onChange={v => typeof v === 'number' ? setDevs(v) : null} />
            </section>

            <section>
              <label htmlFor="years">
                Number of years to buy a license for
              </label>
              <InputRange
                formatLabel={v => v <= 1 ? `${v} year` : `${v} years`}
                maxValue={15}
                minValue={1}
                value={years}
                onChange={v => typeof v === 'number' ? setYears(v) : null} />
            </section>

            <p className="total" style={{ margin: '50px 0 0 0', fontWeight: 'bold' }}>
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
          <CheckoutForm devs={devs} cores={cores} years={years} />
        </DocSection>
      </DocPage>
    </Elements>
  )
}
