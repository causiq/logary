import { faClock } from '@fortawesome/fontawesome-free';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import Layout from './Layout'
import DocMenu from './DocMenu'
import { IconProp } from '@fortawesome/fontawesome-svg-core';
import { ReactNode } from 'react';

type Props = Readonly<{
  name: string;
  colour: string;
  title: string;
  icon?: ReactNode;
  faIcon?: IconProp;
  children: ReactNode;
  readingMinutes?: number;
  toc: any[]
}>

export default function DocPage({
  name,
  colour,
  title,
  icon,
  faIcon,
  readingMinutes,
  toc,
  children,
  ...rest
}: Props) {
  return <Layout name={name} title={title} className={`body-${colour}`} {...rest}>
    <div className="doc-wrapper">
      <div className="container">

        <div id="doc-header" className="doc-header text-center">
          <h1 className="doc-title">
            {faIcon != null
              ? <FontAwesomeIcon icon={faIcon} />
              : icon} {title}
          </h1>
          {readingMinutes != null
            ? <div className="meta">
                <FontAwesomeIcon icon={faClock} size="1x" />{' '}
                Expected reading time: {readingMinutes} minutes
              </div>
              : null}
        </div>

        <div className="doc-body row">
          <div className="doc-content col-md-9 col-12 order-1">
            {children}
          </div>

          <div className="doc-sidebar col-md-3 col-12 order-0 d-none d-md-flex">
            <div id="doc-nav" className="doc-nav">
              {toc != null ? <DocMenu value={toc} /> : null}
            </div>
          </div>
        </div>
      </div>
    </div>
  </Layout>
}