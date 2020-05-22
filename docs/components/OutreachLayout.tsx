import classNames from 'classnames'
import Head from 'next/head'
import { DetailedHTMLProps, HTMLAttributes, ReactNode } from 'react'

type Props = Readonly<{ title: string; children?: ReactNode; }> & Partial<DetailedHTMLProps<HTMLAttributes<HTMLDivElement>, HTMLDivElement>>

// Actual layout
const Layout = ({ title, className, children, ...rest }: Partial<Props>) => {
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