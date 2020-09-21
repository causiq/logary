import Link from 'next/link'
import TweetShare from './TweetShare'
import { Button } from 'reactstrap'
import { useState } from 'react'
import Router from 'next/router'
import LogaryLogo from './LogaryLogo';

const Social = () =>
  <>
    <div className="social-container">
      <div className="github-btn mb-2">
        <Button onClick={() => Router.push("https://github.com/logary/logary")}>
          Star Logary
        </Button>{' '}
        <Button onClick={() => Router.push("https://github.com/logary")}>
          Follow @logary
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
  </>

const SiteHeader = ({ query, setQuery }) => {
  const [ tempQuery, setTempQuery ] = useState(query || '');

  function handleSubmit(evt) {
    evt.preventDefault();
    setQuery(tempQuery)
  }

  return <header className="header text-center">
    <div className="container">
      <div className="branding">
        <Link href="/">
          <a>
            <h1 className="logo">
              <LogaryLogo style={{ width: '220px', height: 'auto'}} />
              <span className="text-highlight visually-hidden">Logary</span><br />
              <span className="text-bold">Analytics</span>
            </h1>
          </a>
        </Link>
      </div>

      <div className="tagline">
        <p>Logary is awesome for analytics and logging.</p>
        <p>Built with ❤️ for developers and business analysts.</p>
      </div>

      <div className="main-search-box pt-3 pb-4 d-inline-block">
        <form
          className="form-inline search-form justify-content-center"
          onSubmit={handleSubmit}>
          <input
            type="text"
            placeholder="Enter search terms..."
            name="search" className="form-control search-input"
            onChange={e => setTempQuery(e.target.value)}
            value={tempQuery}
            />
          <Button type="submit" className="btn search-btn" value="Search">
            Search
          </Button>
        </form>
      </div>

      {query != null && query.length > 0 ? null : <Social />}
    </div>
  </header>
}

export default SiteHeader;
