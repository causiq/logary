import { forwardRef } from 'react'

export const DocSection = forwardRef((props, ref) => {
  const {
    title,
    id,
    children,
    ...rest
  } = props;
  return (
    <section id={id} ref={ref} {...rest} className="doc-section">
      {children}
    </section>
  )
})

export default DocSection;