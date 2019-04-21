import Head from 'next/head'
import { withRouter } from 'next/router'
import classNames from 'classnames'
import SiteHeader from './SiteHeader'
import PageHeader from './PageHeader'
import Footer from './Footer'

// Font awesome
import 'bootstrap/dist/css/bootstrap.min.css'
import '@fortawesome/fontawesome-svg-core/styles.css'
import "../styles/styles.scss"
import { library, config } from '@fortawesome/fontawesome-svg-core'
import { faHeart, faSearch, faPaperPlane, faPrintSearch } from '@fortawesome/pro-solid-svg-icons'
import { faGithub } from "@fortawesome/free-brands-svg-icons"
config.autoAddCss = false

library.add(faHeart, faSearch, faGithub, faPaperPlane)

// Syntax highlighting
import Lowlight from 'react-lowlight'
// https://github.com/isagalaev/highlight.js/tree/master/src/languages
import langFs from 'highlight.js/lib/languages/fsharp'
import langCs from 'highlight.js/lib/languages/cs'
import langJavascript from 'highlight.js/lib/languages/javascript'
import langJson from 'highlight.js/lib/languages/json'
import 'highlight.js/styles/atom-one-light.css'
Lowlight.registerLanguage('fsharp', langFs)
Lowlight.registerLanguage('csharp', langCs)
Lowlight.registerLanguage('javascript', langJavascript)
Lowlight.registerLanguage('json', langJson)

import { useState } from 'react'
import { useLunr } from 'react-lunr'
import { index, store } from '../search'
import SearchPage from './SearchPage'

// Actual layout
const Layout = ({ name, title, className = [], router, children, ...rest }) => {
  const [query, setQuery] = useState(null)
  const results = useLunr(query, index, store),
        isSearching = query != null && query.length > 0

  return <div className={classNames(className, name || 'root')} {...rest}>
    <div className='page-wrapper'>
      <Head>
        <meta name="viewport" content="width=device-width, initial-scale=1" key="viewport" />
        <meta charSet="utf-8" key="charset" />
        <meta name="description" content="Logary is a logging, tracing and metric library for .Net and JS as well as a stand-alone, cloud-native log router/ingress called Rutta." key="description" />
        <meta name="author" content="Henrik Feldt" key="author" />
      </Head>
      {router.pathname === "/"
        ? <SiteHeader query={query} setQuery={setQuery} />
        : <PageHeader title={title} query={query} setQuery={setQuery} />}
      {isSearching
        ? <SearchPage query={query} setQuery={setQuery} results={results} />
        : children}
      <Footer />
    </div>
  </div>
}

export default withRouter(Layout)