import { ScrollTo } from "react-scroll-to"

const ScrollLink = ({ targetRef, children }) => {
  function handleClick(scrollTo) {
    return evt => {
      evt.preventDefault()
      if (targetRef.current != null)  {
        const el = targetRef.current.getBoundingClientRect()
        const y = el.top + window.pageYOffset
        scrollTo({ y, smooth: true })
      }
    }
  }

  return <ScrollTo>
    {({ scrollTo }) =>
      <a href="/" onClick={handleClick(scrollTo)}>
        {children}
      </a>
    }
  </ScrollTo>
}

export default ScrollLink