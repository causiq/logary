import ScrollLink from './ScrollLink'
import { RefObject } from 'react'

type Value = Readonly<{
  ref?: RefObject<any>;
  title: string;
  id: string;
}>

type Props = Readonly<{
  value: Value[];
}>

export default function DocMenu({ value }: Props) {
  return (
    <nav id="doc-menu" className="nav doc-menu flex-column sticky">
      {value.map(({ ref, id, title, ...rest }) =>
        ref != null
          ? <ScrollLink {...rest} id={`link-to-${id}`} href={`#${id}`} className="nav-link" targetRef={ref} key={id}>{title}</ScrollLink>
          : <a {...rest} className="nav-link" href={id} key={id}>{title}</a>)}
    </nav>
  )
}