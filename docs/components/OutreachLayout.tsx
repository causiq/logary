import classNames from 'classnames'
import Head from 'next/head'
import { ReactNode } from 'react'

type Props = Readonly<{
  title: string;
  className?: string | string[];
  children?: ReactNode;
  noChat?: boolean;
}>

// Actual layout
const Layout = ({ title, className = [], children, ...rest }: Props) => {
  return <div className={classNames(className, 'layout')} {...rest}>
    <Head>
      <meta name="viewport" content="width=device-width, initial-scale=1" key="viewport" />
      <meta charSet="utf-8" key="charset" />
      <title>{title}</title>
    </Head>

    {children}
  </div>
}

export default Layout