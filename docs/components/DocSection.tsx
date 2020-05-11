import { forwardRef, ReactNode } from 'react'

export type DocSectionProps = Readonly<{
  title: string;
  id: string;
  children: ReactNode;
}>

const DocSection = forwardRef<HTMLElement, DocSectionProps>((props, ref) => {
  const { title, id, children, ...rest } = props
  return (
    <section id={id} ref={ref} {...rest} className='doc-section'>
      <h2 className='section-title'>{title}</h2>
      {children}
    </section>
  )
})

DocSection.displayName = 'DocSection'

export default DocSection;