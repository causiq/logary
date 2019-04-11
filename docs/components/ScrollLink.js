import { ScrollTo } from "react-scroll-to"

const ScrollLink = ({ href = "#", targetRef, children, ...rest }) => {
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
      <a href={href} onClick={handleClick(scrollTo)} {...rest}>
        {children}
      </a>
    }
  </ScrollTo>
}

export default ScrollLink