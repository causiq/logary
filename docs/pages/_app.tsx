import 'bootstrap/dist/css/bootstrap.min.css'
import '@fortawesome/fontawesome-svg-core/styles.css'
import "../styles/styles.scss"
import 'highlight.js/styles/atom-one-light.css'
import "react-input-range/lib/css/index.css"

import { config } from '@fortawesome/fontawesome-svg-core'
config.autoAddCss = false

import { withLogary } from '@logary/plugin-nextjs'
import logary from '../components/logary'

if (typeof window !== 'undefined' && 'scrollRestoration' in history) {
  history.scrollRestoration = 'manual'
}

function MyApp({ Component, pageProps }: Record<string, any>) {
  return <Component {...pageProps} />
}

export default withLogary(MyApp, { logary })