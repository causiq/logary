import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { IconProp } from '@fortawesome/fontawesome-svg-core'
import Layout from './Layout'
import DocMenu from './DocMenu'
import { ReactNode, Children, isValidElement, cloneElement, useMemo, createRef, RefObject } from 'react'
import { DocSectionProps } from './DocSection'
import useSectionTracking from './useSectionTracking'
import { useRouter } from 'next/router'

type ToCItem = Readonly<{
  title: string;
  id: string;
  ref: RefObject<HTMLElement>;
}>

type Props = Readonly<{
  name: string;
  colour: string;
  title: string;
  icon?: ReactNode;
  faIcon?: IconProp;
  children: ReactNode[];
  estimationMinutes?: number;
  noEstimation?: boolean;
  noChat?: boolean;
  toc?: ToCItem[]
}>

/**
 * Create a table of contents for the doc page; each ToC link can be clicked to support scrolling
 */
function forwardChildren(cs: ReactNode[]): [ReactNode[], ToCItem[]] {
  const nextChildren: ReactNode[] = [], tocItems: ToCItem[] = []

  Children.forEach(cs, child => {
    // @ts-ignore It is either there, and we have a value, or it's null
    const typeName = child.type.displayName

    if (isValidElement(child) && typeName === 'DocSection') {
      const props = child.props as DocSectionProps
      const childRef = createRef<HTMLElement>()
      nextChildren.push(cloneElement(child, {
        ...child.props,
        key: props.id,
        ref: childRef
      }))
      tocItems.push({
        id: props.id,
        title: props.title,
        ref: childRef
      })
    }
    else nextChildren.push(child)
  })

  return [nextChildren, tocItems]
}


export default function DocPage({
  name,
  colour,
  title,
  icon,
  faIcon,
  estimationMinutes,
  noEstimation,
  children: cs,
  ...rest
}: Props) {

  const router = useRouter()

  const [children, toc] = useMemo(() => forwardChildren(cs), [cs])

  // useEffect(() => {
  //   Router.beforePopState(state => { console.log('beforePopState', state); return true })
  //   Router.events.on('routeChangeStart', console.log.bind(console, 'routeChangeStart'))
  //   Router.events.on('routeChangeComplete', console.log.bind(console, 'routeChangeComplete'))
  //   Router.events.on('routeChangeError', console.log.bind(console, 'routeChangeError'))
  //   Router.events.on('beforeHistoryChange', console.log.bind(console, 'beforeHistoryChange'))
  //   Router.events.on('hashChangeStart', console.log.bind(console, 'hashChangeStart'))
  //   Router.events.on('hashChangeComplete', console.log.bind(console, 'hashChangeComplete'))
  // })

  // useEffect(() => {
  //   const handlePop = (e: PopStateEvent) => { console.log(`state: ${JSON.stringify(e.state)}`) }
  //   window.addEventListener('popstate', handlePop)
  //   return () => window.removeEventListener('popstate', handlePop)
  // })

  useSectionTracking(toc, section => {
    if (history.pushState == null) return

    // const pathname = new URL(location.href).pathname
    // const pathAndSearch = section == null ? pathname : `${pathname}#${section.id}`
    // history.pushState({ url: window.location.href, as: router.asPath }, document.title, pathAndSearch)

    // https://nextjs.org/docs/api-reference/next/router
    const asPath = router.asPath.indexOf("#") === -1 ? router.asPath : router.asPath.substring(0, router.asPath.indexOf("#"))
    const nextAsPath = section != null ? `${asPath}#${section.id}` : asPath
    console.log('url aka pathname=', router.pathname, 'asPath=', nextAsPath)
    router.replace(router.pathname, nextAsPath, { shallow: true })
  })

  return <Layout name={name} title={title} className={`body-${colour}`} {...rest}>
    <div className="doc-wrapper">
      <div className="container">

        <div id="doc-header" className="doc-header text-center">
          <h1 className="doc-title">
            {faIcon != null
              ? <FontAwesomeIcon icon={faIcon} />
              : icon} {title} — Logary
          </h1>
          {!noEstimation && <div className="meta">
            {/* <FontAwesomeIcon icon={faClock} size="1x" /> */}
            {' '}
            Reading time: {String(estimationMinutes || Math.round(toc.length * 1.5))} minutes
          </div>}
        </div>

        <div className="doc-body row">
          <div className="doc-content col-md-9 col-12 order-1">
            {children}
          </div>

          {toc && <div className="doc-sidebar col-md-3 col-12 order-0 d-none d-md-flex">
            <div id="doc-nav" className="doc-nav">
              <DocMenu value={toc} />
            </div>
          </div>}
        </div>
      </div>
    </div>
  </Layout>
}