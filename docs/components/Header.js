import Link from 'next/link'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import TweetShare from '../components/TweetShare'
import { Button } from 'reactstrap'
import { faHeart } from '@fortawesome/pro-solid-svg-icons';
import { faGithub } from "@fortawesome/free-brands-svg-icons"
import Router from 'next/router'

const Header = () =>
  <header className="header text-center">
    <div className="container">
      <div className="branding">
        <h1 className="logo">
          <span aria-hidden="true" className="icon_documents_alt icon"></span>
          <span className="text-highlight">Logary</span><span className="text-bold">Analytics</span>
        </h1>
      </div>

      <div className="tagline">
        <p>Logary is awesome for analytics and logging.</p>
        <p>Built with <FontAwesomeIcon icon={faHeart} /> for developers and business analysts.</p>
      </div>

      <div className="main-search-box pt-3 pb-4 d-inline-block">
        <form className="form-inline search-form justify-content-center" action="" method="get">
          <input type="text" placeholder="Enter search terms..." name="search" className="form-control search-input" />
          {/* https://lunrjs.com/guides/getting_started.html */}
          <Button type="submit" className="btn search-btn" value="Search">
            <FontAwesomeIcon icon="search" />
          </Button>
        </form>
      </div>

      <div className="social-container">
        <div className="github-btn mb-2">
          <Button onClick={() => Router.push("https://github.com/logary/logary")}>
            <FontAwesomeIcon icon={faGithub} /> Star Logary
          </Button>{' '}
          <Button onClick={() => Router.push("https://github.com/logary")}>
            <FontAwesomeIcon icon={faGithub} /> Follow @logary
          </Button>
        </div>
        <div>
          <div className="twitter-tweet">
            <TweetShare
              text="Logary can get your company's business analytics bootstrapped in a few hours!"
              via="logarylib" related="henrikfeldt"
              size="large"
              hashtags={["monitoring", "logging", "metrics", "analytics", "pwa", "mobile", "reactNative"]}/>
          </div>
        </div>
      </div>
    </div>
  </header>

export default Header;

/* <nav>
  <ul>
    <li>
      <Link href="/">
        <a title="Home">Home</a>
      </Link>
    </li>
    <li>
      <Link href="/vision">
        <a title="The vision of Logary">Vision</a>
      </Link>
    </li>
    <li>
      <Link href="/logary-dotnet-targets">
        <a title="Available targets in Logary">Targets</a>
      </Link>
    </li>
    <li>
      <Link href="/rutta">
        <a title="Log router and ingestion point">Rutta</a>
      </Link>
    </li>
  </ul>
</nav>*/
