import Layout from '../../components/Layout'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faJs } from '@fortawesome/free-brands-svg-icons';
import { faClock } from '@fortawesome/pro-regular-svg-icons';
import { BashCode } from '../../components/Code'

export default function JSQuickstart() {
  return (
    <Layout name="js-quickstart" className={"body-yellow"}>
      <div className="doc-wrapper">
        <div className="container">

          <div id="doc-header" className="doc-header text-center">
            <h1 className="doc-title">
              <FontAwesomeIcon icon={faJs} /> JS Quickstart
            </h1>
            <div className="meta">
              <FontAwesomeIcon icon={faClock} size="small" /> Expected reading time: 2 minutes
            </div>
          </div>

          <div className="doc-body row">
            <div className="doc-content col-md-9 col-12 order-1">
              <div className="content-inner">
                <section className="doc-section">
                  <h2 className="section-title">Install the package</h2>
                  <p>
                    The first step is to install the Logary package from npm.
                  </p>
                  <BashCode>npm install --save logary</BashCode>
                  <p>Or you're using yarn...</p>
                  <BashCode>yarn add logary</BashCode>
                  <a href="https://github.com/logary/logary-js#how-to-use" title="Continue here..">Continue here...</a>
                </section>
              </div>
            </div>

            <div className="doc-sidebar col-md-3 col-12 order-0 d-none d-md-flex">
              <div id="doc-nav" className="doc-nav">
                <nav id="doc-menu" className="nav doc-menu flex-column sticky">
                  <a className="nav-link scrollto">Install package</a>
                </nav>{/*doc-menu*/}
              </div>
            </div>{/*doc-sidebar*/}
          </div>
        </div>
      </div>
    </Layout>
  )
}