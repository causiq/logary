import Link from 'next/link'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { Button } from 'reactstrap'
import { useState } from 'react'

const PageHeader = ({ title, query, setQuery }) => {
  const [ tempQuery, setTempQuery ] = useState(query || '');

  function handleSubmit(evt) {
    evt.preventDefault();
    setQuery(tempQuery)
  }

  return <header className="header">
    <div className="container">
      <div className="branding">
        <Link href="/">
          <a>
            <h1 className="logo">
              <span className="text-highlight">Logary</span><span className="text-bold">Analytics</span>
            </h1>
          </a>
        </Link>
      </div>

      <ol className="breadcrumb">
        <li className="breadcrumb-item">
          <Link href="/">
            <a>Start</a>
          </Link>
        </li>
        <li className="breadcrumb-item active">{title}</li>
      </ol>

      <div className="top-search-box">
        <form
          className="form-inline search-form justify-content-center"
          onSubmit={handleSubmit}>
          <input type="text"
            placeholder="Search..."
            name="search"
            className="form-control search-input"
            onChange={e => setTempQuery(e.target.value)}
            value={tempQuery}
            autoFocus
            />
          <Button type="submit" className="btn search-btn" value="Search">
            <FontAwesomeIcon icon="search" />
          </Button>
        </form>
      </div>
    </div>
  </header>
}

export default PageHeader;