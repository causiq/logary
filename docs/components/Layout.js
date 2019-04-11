import Head from 'next/head'
import { withRouter } from 'next/router'
import classNames from 'classnames'
import SiteHeader from './SiteHeader'
import PageHeader from './PageHeader'
import Footer from './Footer'
import 'bootstrap/dist/css/bootstrap.min.css'
import '@fortawesome/fontawesome-svg-core/styles.css'
import "../styles/styles.scss"
import { library, config } from '@fortawesome/fontawesome-svg-core'
import { faHeart, faSearch, faPaperPlane } from '@fortawesome/pro-solid-svg-icons'
import { faGithub } from "@fortawesome/free-brands-svg-icons"
config.autoAddCss = false

library.add(faHeart, faSearch, faGithub, faPaperPlane)

const Layout = ({ name, className = [], router, children, ...rest }) => {
  return <div className={classNames(className, name || 'root')} {...rest}>
    <div className='page-wrapper'>
      <Head>
        <meta name="viewport" content="width=device-width, initial-scale=1" key="viewport" />
        <meta charSet="utf-8" key="charset" />
        <meta name="description" content="Logary is a logging, tracing and metric library for .Net and JS as well as a stand-alone, cloud-native log router/ingress called Rutta." key="description" />
        <meta name="author" content="Henrik Feldt" key="author" />
      </Head>
      {router.pathname === "/" ? <SiteHeader /> : <PageHeader />}
      {children}
      <Footer />
    </div>
  </div>
}

export default withRouter(Layout)