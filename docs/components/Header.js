import Link from 'next/link'

const Header = () =>
  <nav>
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
  </nav>

export default Header;