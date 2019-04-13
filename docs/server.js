const express = require('express')
const next = require('next')
const { parse } = require('url')
const routes = require("./routes")

const dev = process.env.NODE_ENV !== 'production'
const stripeKey = process.env.STRIPE_SECRET_KEY
if (stripeKey == null) throw new Error("Missing env var STRIPE_SECRET_KEY")
const stripe = require("stripe")(stripeKey)
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
        let {status} = await stripe.charges.create({
          amount: 2000,
          currency: "usd",
          description: "An example charge",
          source: req.body
        });

        res.json({ status });
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