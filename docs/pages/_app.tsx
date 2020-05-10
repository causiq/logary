import 'bootstrap/dist/css/bootstrap.min.css'
import '@fortawesome/fontawesome-svg-core/styles.css'
import "../styles/styles.scss"
import 'highlight.js/styles/atom-one-light.css'
import "react-input-range/lib/css/index.css"

export default function MyApp({ Component, pageProps }) {
  return <Component {...pageProps} />
}