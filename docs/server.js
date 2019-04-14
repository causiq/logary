const express = require('express')
const next = require('next')
const { parse } = require('url')
const routes = require("./routes")
const pricing = require('./components/calculatePrice')

const dev = process.env.NODE_ENV !== 'production'
const stripeKey = process.env.LOGARY_STRIPE_SECRET_KEY
if (stripeKey == null) throw new Error("Missing env var LOGARY_STRIPE_SECRET_KEY")
const stripe = require("stripe")(stripeKey)
stripe.setApiVersion('2019-03-14');
const app = next({ dev })
const handle = app.getRequestHandler()

const remap = actualPath => {
  if (routes.hasOwnProperty(actualPath)) {
    console.log(`Mapping ${actualPath} => ${routes[actualPath]}`)
    return [ true, routes[actualPath] ]
  } else {
    return [ false, null  ];
  }
}

app
  .prepare()
  .then(() => {
    const server = express()
    server.use(require("body-parser").text())
    server.use(require("body-parser").json())
    const port = process.env.PORT || 3000

    server.get('*', (req, res) => {
      const parsedUrl = parse(req.url, true)
      const { pathname, query } = parsedUrl
      const [ shouldRemap, newPath ] = remap(pathname);
      if (shouldRemap) {
        return app.render(req, res, newPath, query)
      } else {
        // console.log('got parsed url ', parsedUrl)
        return handle(req, res, parsedUrl);
      }
    })

    server.post("/charge", async (req, res) => {
      try {
        const chargeVAT = req.body.customer.vatNo == null || req.body.customer.vatNo === "",
              ep = pricing.calculatePrice(req.body.cores, req.body.devs, req.body.years, pricing.ContinuousRebate, chargeVAT)

        if (!pricing.equal(ep.total, req.body.price.total)) {
          logger.error("Received value from client", rq.body.price, "but calculated it as", ep)
          res.status(400).statusMessage("Bad amount or not same currency");
          return;
        }

        console.log(JSON.stringify(req.body))

        const cr = await stripe.charges.create({
          ...ep.total,
          description: req.body.token.name,
          source: req.body.token.id
        });

        res.json({ status: cr.status });
      } catch (err) {
        console.error(err)
        res.status(500).end();
      }
    });

    server.listen(port, err => {
      if (err) throw err
      console.info(`> Ready on http://localhost:${port}`)
    })
  })
  .catch(ex => {
    console.error(ex.stack)
    process.exit(1)
  })