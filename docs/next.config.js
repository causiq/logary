const withImages = require('next-images')({ target: 'serverless' })
module.exports = {
  ...withImages,
  env: {
    LOGARY_STRIPE_SECRET_KEY: process.env.LOGARY_STRIPE_SECRET_KEY
  }
}