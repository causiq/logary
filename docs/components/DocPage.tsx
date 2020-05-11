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
  children: ReactNode;
  estimationMinutes?: number;
  noEstimation?: boolean;
  noChat?: boolean;
  toc?: ToCItem[]
}>

/**
 * Create a table of contents for the doc page; each ToC link can be clicked to support scrolling
 */
function forwardChildren(cs: ReactNode, matchType: string = 'DocSection'): [ReactNode[], ToCItem[]] {
  const nextChildren: ReactNode[] = [], tocItems: ToCItem[] = []

  Children.forEach(cs, child => {
    // @ts-ignore It is either there, and we have a value, or it's null
    const typeName = child.type?.displayName

    if (isValidElement(child) && typeName === matchType) {
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

// Debug tools:

// import { useEffect } from 'react'

// useEffect(() => {
//   router.beforePopState(state => { console.log('beforePopState', state); return true })
//   router.events.on('routeChangeStart', console.log.bind(console, 'routeChangeStart'))
//   router.events.on('routeChangeComplete', console.log.bind(console, 'routeChangeComplete'))
//   router.events.on('routeChangeError', console.log.bind(console, 'routeChangeError'))
//   router.events.on('beforeHistoryChange', console.log.bind(console, 'beforeHistoryChange'))
//
//   const handlePop = (e: PopStateEvent) => { console.log(`state: ${JSON.stringify(e.state)}`) }
//   window.addEventListener('popstate', handlePop)
//   return () => window.removeEventListener('popstate', handlePop)
// })

// useEffect(() => {
//   const logScroll = (name: string) => (args: any) => console.log(name, args, window.scrollY)
//   const logStart = logScroll('hashChangeStart')
//   const logComplete = logScroll('hashChangeComplete')

//   router.events.on('hashChangeStart', logStart)
//   router.events.on('hashChangeComplete', logComplete)
//   return () => {
//     router.events.off('hashChangeStart', logStart)
//     router.events.off('hashChangeComplete', logComplete)
//   }
// })

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

  useSectionTracking(toc, section => {
    if (history.pushState == null) return

    // https://nextjs.org/docs/api-reference/next/router
    const [ asPath, ] = router.asPath.split("#")
    const nextAsPath = section != null ? `${asPath}#${section.id}` : asPath

    // When we go back, don't update the history state with this information (it would without this check, since a new
    // section has scrolled into view).
    if (nextAsPath === history.state.as) return

    // This is how NextJS uses the History API:
    // https://github.com/zeit/next.js/blob/e91c0fccbb7ab2efa53004728e9f7494c840c264/packages/next/next-server/lib/router/router.ts#L521-L532
    const nextState = { url: router.pathname, as: nextAsPath, options: { shallow: true } }
    //console.log('url aka pathname=', router.pathname, 'asPath=', nextAsPath, 'history.state=', history.state, 'nextState', nextState)

    history.pushState(nextState, '', nextAsPath)

    // Can't use this since it ALWAYS scrolls to the active element, so when the user scrolls up, and we change
    // the hash of the browser, the browser viewport is immediately scrolled to the top of that section:
    //
    // router.replace(router.pathname, nextAsPath, { shallow: true })
    //  => change('replaceState') => scrollToHash
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