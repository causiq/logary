import 'bootstrap/dist/css/bootstrap.min.css'
import '@fortawesome/fontawesome-svg-core/styles.css'
import "../styles/styles.scss"
import 'highlight.js/styles/atom-one-light.css'
import "react-input-range/lib/css/index.css"

import { config } from '@fortawesome/fontawesome-svg-core'
config.autoAddCss = false


if (typeof window !== 'undefined' && 'scrollRestoration' in history) {
  history.scrollRestoration = 'manual'
}

export default function MyApp({ Component, pageProps }: Record<string, any>) {
  return <Component {...pageProps} />
}