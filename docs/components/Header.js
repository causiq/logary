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
    </ul>
  </nav>

export default Header;