import Head from 'next/head'
import Header from './Header'

const layoutStyle = {
  margin: 20,
  padding: 20,
  border: '1px solid #DDD'
}

const Layout = props => (
  <div style={layoutStyle}>
    <Head>
      <meta name="viewport" content="width=device-width, initial-scale=1" key="viewport" />
      <meta charSet="utf-8" key="charset" />
    </Head>
    <Header />
    {props.children}
  </div>
)

export default Layout