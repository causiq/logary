import classNames from 'classnames'
import Head from 'next/head'
import { DetailedHTMLProps, HTMLAttributes, ReactNode } from 'react'

type Props = Readonly<{
  title: string;
  description?: string;
  children?: ReactNode;
}> & Partial<DetailedHTMLProps<HTMLAttributes<HTMLDivElement>, HTMLDivElement>>

// Actual layout
const Layout = ({ title, description, className, children, ...rest }: Partial<Props>) => {
  return <div className={classNames(className, 'layout')} {...rest}>
    <Head>
      <meta name="viewport" content="width=device-width, initial-scale=1" key="viewport" />
      <link rel="icon" type="image/png" href={require("../public/images/favicon-32x32.png")} sizes="32x32" key="favicon" />
      <link rel="icon" type="image/png" href={require("../public/images/favicon-64x64.png")} sizes="64x64" key="favicon-64x64" />
      <link rel="icon" type="image/png" href={require("../public/images/favicon-96x96.png")} sizes="96x96" key="favicon-96x96" />
      <link rel="icon" type="image/png" href={require("../public/images/icon-200x200.png")} sizes="200x200" key="icon-200x200" />
      <link rel="apple-touch-icon" sizes="200x200" href={require("../public/images/icon-200x200.png")} key="apple-touch-icon-200x200" />
      <link rel="apple-touch-icon" sizes="500x500" href={require("../public/images/icon-500x500.png")} key="apple-touch-icon-500x500" />
      <meta charSet="utf-8" key="charset" />
      <meta name="description" content={description} key="description" />
      <meta property="og:description" content={description} />
      <title>{title}</title>
    </Head>

    {children}
  </div>
}

export default Layout