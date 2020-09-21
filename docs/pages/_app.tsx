import 'bootstrap/dist/css/bootstrap.min.css'
import "../styles/styles.scss"
import 'highlight.js/styles/atom-one-light.css'
import "react-input-range/lib/css/index.css"
import 'simple-line-icons/dist/styles/simple-line-icons.scss';

import { withLogary } from '@logary/plugin-nextjs'
import logary from '../components/logary'

if (typeof window !== 'undefined' && 'scrollRestoration' in history) {
  history.scrollRestoration = 'manual'
}

function LogaryTechApp({ Component, pageProps }: Record<string, any>) {
  return <Component {...pageProps} />
}

export default withLogary(LogaryTechApp, { logary })