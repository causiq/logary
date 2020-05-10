import { calculatePrice, formatMoney, ContinuousRebate, equal } from '../../components/calculatePrice'
import { NextApiRequest, NextApiResponse } from 'next'
import { parseOneAddress } from "email-addresses"
import { PaymentMethod } from '@stripe/stripe-js'

const stripeKey = process.env.LOGARY_STRIPE_SECRET_KEY
if (stripeKey == null) throw new Error("Missing env var LOGARY_STRIPE_SECRET_KEY")
const stripe = require("stripe")(stripeKey)
stripe.setApiVersion('2019-03-14')

/**
 * https://stripe.com/docs/billing/subscriptions/payment
 * @param {*} token Stripe Source id or Token https://stripe.com/docs/api#tokens https://stripe.com/docs/api#sources
 * @param {companyName, name, email, vatNo?} customer
 * @param {address,name,local,domain,parts} parsedEmail
 * @returns https://stripe.com/docs/api/customers/create
 */
const getOrCreateCustomer = async (token: string, customer: Record<string, any>, parsedEmail: Record<string, any>) => {
  console.debug("Listing customers based on the e-mail provided.")
  const res = await stripe.customers.list({ limit: 1, email: parsedEmail.address })
  if (res.data.length > 0) {
    const existing = res.data[0]
    if (token === existing.default_source) {
      console.info(`Found existing customer with id ${existing.id}, reusing as tokens were identical.`)
      return existing;
    }

    console.info(`Found existing customer with id ${existing.id}, updating source.`)
    return await stripe.customers.update(existing.id, {
      source: token,
      metadata: {
        ...existing.metadata,
        emailName: parsedEmail.name,
        companyName: customer.companyName.trim(),
        updated: Date.now(),
      },
    })
  }

  console.info("Creating new customer.")
  return await stripe.customers.create({
    name: customer.name.trim(),
    email: parsedEmail.address,
    description: `Customer for "${customer.name.trim()}" <${parsedEmail.address}>`,
    metadata: {
      emailName: parsedEmail.name,
      companyName: customer.companyName.trim(),
      created: Date.now(),
    },
    source: token
  });
}

const stringly = (price: any) =>
  Object
    .keys(price)
    .map(k => [ k, typeof price[k] === 'object' ? formatMoney(price[k]) : String(price[k])])
    .reduce((acc, [ k, v ]) => ({ ...acc, [k]: v }), {})

const getProducts = async () =>  {
  const ps = await stripe.products.list({
    active: true,
    limit: 10,
    type: 'service'
  })

  if (ps.data.length !== 2) {
    throw new Error("Expected two active=true, type=service products")
  }

  return {
    cores: ps.data.filter((x: Record<string, any>) => x.name === 'logary_license_cores')[0],
    devs: ps.data.filter((x: Record<string, any>) => x.name === "logary_license_devs")[0]
  }
}

/**
 * https://stripe.com/docs/api/subscriptions/create
 * @param {Stripe.Customer} customer
 */
const createSubscription = async (cores: number, devs: number, price: any, customer: Record<string, any>) => {
  // e.g. ("logary_devs_5", "logary_devs_1") => ...
  // @ts-ignore quick way to sort...
  const sortLatestSuffix = (x, y) => /_(\d{1,})/.exec(y)[1] - /_(\d{1,})/.exec(x)[1]
  const products = await getProducts();
  const cs = await stripe.plans.list({ limit: 50, product: products.cores.id })
  const ds = await stripe.plans.list({ limit: 50, product: products.devs.id })
  console.log('Got cs back', cs)
  console.log('Got ds back', ds)
  const planCore = [ ...cs.data ].sort(sortLatestSuffix)[0]
  const planDev = [ ...ds.data ].sort(sortLatestSuffix)[0]
  const s = await stripe.subscriptions.create({
    customer: customer.id,
    items: [
      { plan: planDev.id, quantity: devs }, // devs 1 test
      { plan: planCore.id, quantity: cores } // cores 1 test
    ],
    billing: "charge_automatically",
    metadata: {
      ...stringly(price)
    },
    expand: [
      "latest_invoice.payment_intent"
    ],
    tax_percent: 100 * price.vatRate
  })
  console.log('Got subscription back', s)
  // TODO: manually attach https://stripe.com/docs/billing/subscriptions/discounts to the subscription
  // after creation to match the money off
  return s;
}

function badReq(res: NextApiResponse, message: string) {
  res.status(400).json({
    type: 'failure',
    payload: { message }
  })
}

export type Customer = Readonly<{
  companyName: string;
  name: string;
  vatNo: string;
  email: string;
}>

export type ChargeRequest = Readonly<{
  paymentMethod: PaymentMethod;
  price: ReturnType<typeof calculatePrice>;
  cores: number;
  devs: number;
  years: number;
  customer: Customer;
}>

type ChargeSuccessResponse = Readonly<{
  type: 'success';
  payload: Readonly<{ code: string; title: string; }>;
}>

type ChargeFailureResponse = Readonly<{
  type: 'failure';
  payload: Readonly<{ code: string; title: string; }>;
}>

export type ChargeResponse =
  | ChargeSuccessResponse
  | ChargeFailureResponse

export default async function charge(req: NextApiRequest, res: NextApiResponse) {
  if (!req.body || !req.body.customer) {
    badReq(res, "Missing body or body.customer value")
    return
  }

  try {
    // validate price
    const chargeVAT = req.body.customer.vatNo == null || req.body.customer.vatNo === "",
          ep = calculatePrice(req.body.cores, req.body.devs, req.body.years, ContinuousRebate, chargeVAT)

    console.log("Calculated price", ep)

    if (!equal(ep.total, req.body.price.total)) {
      console.error("Received value from client", req.body.price, "but calculated it as", ep)
      badReq(res, "Bad amount or not same currency")
      return
    }

    // validate e-mail
    // https://www.npmjs.com/package/email-addresses#obj--addrsparseoneaddressopts
    const email = parseOneAddress({ input: req.body.customer.email, rejectTLD: true, simple: false });
    if (email == null) {
      badReq(res, "Bad e-mail")
      return
    }

    // validate company name
    if (req.body.customer.companyName == null || req.body.customer.companyName.trim().length === 0) {
      badReq(res, "Bad company name")
      return
    }

    const customer = await getOrCreateCustomer(req.body.paymentMethod.id, req.body.customer, email);
    const subscription = await createSubscription(req.body.cores, req.body.devs, ep, customer);
    const outcome = `${subscription.status}|${(subscription.latest_invoice.payment_intent || {}).status}`;
    console.info(`Outcome for ${customer.id}: ${outcome}`)
    // https://stripe.com/docs/billing/subscriptions/payment#initial-charge
    // adapted https://jsonapi.org/format/#error-objects
    switch (outcome) {
      case "active|succeeded": // Outcome 1: Payment succeeds
        res.json({
          type: 'success',
          payload: {
            code: "active|succeeded",
            title: "Thanks!"
          }
        })
        break

      case "incomplete|requires_payment_method": // Outcome 3: Payment fails
        res.json({
          type: 'failure',
          payload: {
            code: "incomplete|requires_payment_method",
            title: "Payment failed"
          }
        })
        break

      case "trialing|": // Outcome 2: Trial starts (we don't have!)
      default:
        throw new Error("Impl error: outcome: " + outcome)
    }
  } catch (err) {
    console.error(err)
    res.status(500).end();
  }
}
