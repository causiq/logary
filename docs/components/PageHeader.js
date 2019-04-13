import Link from 'next/link'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { Button } from 'reactstrap'

const PageHeader = ({ title }) =>
  <header className="header">
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
        <form className="form-inline search-form justify-content-center" action="" method="get">
          <input type="text" placeholder="Search..." name="search" className="form-control search-input" />
          <Button type="submit" className="btn search-btn" value="Search">
            <FontAwesomeIcon icon="search" />
          </Button>
        </form>
      </div>
    </div>
  </header>

export default PageHeader;