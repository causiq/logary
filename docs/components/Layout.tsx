import classNames from 'classnames'
import langCs from 'highlight.js/lib/languages/cs'
// https://github.com/isagalaev/highlight.js/tree/master/src/languages
import langFs from 'highlight.js/lib/languages/fsharp'
import langJavascript from 'highlight.js/lib/languages/javascript'
import langJson from 'highlight.js/lib/languages/json'
import langText from 'highlight.js/lib/languages/plaintext'
import langXML from 'highlight.js/lib/languages/xml'
import Head from 'next/head'
import { withRouter } from 'next/router'
import { ReactNode, useEffect, useState } from 'react'
// Syntax highlighting
import Lowlight from 'react-lowlight'
import { useLunr } from 'react-lunr'
import { index, store } from '../public/search'
import Footer from './Footer'
import PageHeader from './PageHeader'
import SearchPage from './SearchPage'
import SiteHeader from './SiteHeader'
import useDrift, { DriftContext } from './useDrift'

Lowlight.registerLanguage('fsharp', langFs)
Lowlight.registerLanguage('fs', langFs)
Lowlight.registerLanguage('csharp', langCs)
Lowlight.registerLanguage('cs', langCs)
Lowlight.registerLanguage('javascript', langJavascript)
Lowlight.registerLanguage('json', langJson)
Lowlight.registerLanguage('xml', langXML)
Lowlight.registerLanguage('text', langText)


type Props = Readonly<{
  name: string;
  title: string;
  className?: string | string[];
  router: any;
  children: ReactNode;
  noChat?: boolean;
}>

function useScriptSource(source: string) {
  useEffect(() => {
    const script = document.createElement('script')
    script.innerHTML = source

    document.body.appendChild(script)
    return () => {
      document.body.removeChild(script)
    }
  }, []);
}

// Actual layout
const Layout = ({ name, title, className = [], router, children, noChat, ...rest }: Props) => {
  const [query, setQuery] = useState<string | null>(null)

  const inDev = typeof window !== 'undefined' && window.location.hostname === 'localhost'
  const drift = useDrift("gvi23z7y7p36", noChat || inDev, x => x)

  useScriptSource(`
    !function(t,e){var o,n,p,r;e.__SV||(window.posthog=e,e._i=[],e.init=function(i,s,a){function g(t,e){var o=e.split(".");2==o.length&&(t=t[o[0]],e=o[1]),t[e]=function(){t.push([e].concat(Array.prototype.slice.call(arguments,0)))}}(p=t.createElement("script")).type="text/javascript",p.async=!0,p.src=s.api_host+"/static/array.js",(r=t.getElementsByTagName("script")[0]).parentNode.insertBefore(p,r);var u=e;for(void 0!==a?u=e[a]=[]:a="posthog",u.people=u.people||[],u.toString=function(t){var e="posthog";return"posthog"!==a&&(e+="."+a),t||(e+=" (stub)"),e},u.people.toString=function(){return u.toString(1)+".people (stub)"},o="capture identify alias people.set people.set_once set_config register register_once unregister opt_out_capturing has_opted_out_capturing opt_in_capturing reset".split(" "),n=0;n<o.length;n++)g(u,o[n]);e._i.push([i,s,a])},e.__SV=1)}(document,window.posthog||[]);
    posthog.init('-3LZUqCSCJq6HgHlXk3e7FoBUHwYA4eSvhU3HM3UUfI', {api_host: 'http://0.0.0.0:8000'})
  `)

  const results = useLunr(query, index, store),
        isSearching = query != null && query.length > 0

  return <div className={classNames(className, name || 'root')} {...rest}>
    <div className='page-wrapper'>
      <Head>
        <meta name="viewport" content="width=device-width, initial-scale=1" key="viewport" />
        <meta charSet="utf-8" key="charset" />
        <meta name="description" content="Logary is a logging, tracing and metric library for .Net and JS as well as a stand-alone, cloud-native log router/ingress called Rutta." key="description" />
        <meta name="author" content="Henrik Feldt" key="author" />
        <link rel="icon" type="image/png" href={require("../public/images/favicon-32x32.png")} sizes="32x32" key="favicon" />
        <link rel="icon" type="image/png" href={require("../public/images/favicon-64x64.png")} sizes="64x64" key="favicon-64x64" />
        <link rel="icon" type="image/png" href={require("../public/images/favicon-96x96.png")} sizes="96x96" key="favicon-96x96" />
        <link rel="icon" type="image/png" href={require("../public/images/icon-200x200.png")} sizes="200x200" key="icon-200x200" />
        <link rel="apple-touch-icon" sizes="200x200" href={require("../public/images/icon-200x200.png")} key="apple-touch-icon-200x200" />
        <link rel="apple-touch-icon" sizes="500x500" href={require("../public/images/icon-500x500.png")} key="apple-touch-icon-500x500" />
        <title>{title}</title>
      </Head>

      <DriftContext.Provider value={drift.current}>
        {router.pathname === "/"
          ? <SiteHeader query={query} setQuery={setQuery} />
          : <PageHeader title={title} query={query} setQuery={setQuery} />}
        {isSearching
          ? <SearchPage query={query} setQuery={setQuery} results={results} />
          : children}
        <Footer />
      </DriftContext.Provider>
    </div>
  </div>
}

export default withRouter(Layout)