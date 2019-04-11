import ScrollLink from './ScrollLink'

export const DocMenu = (props) => {
  const { value } = props;
  return (
    <nav id="doc-menu" className="nav doc-menu flex-column sticky">
      {value.map((x, i) =>
        x.ref != null
          ? <ScrollLink id={x.id} className="nav-link" targetRef={x.ref} key={i}>{x.title}</ScrollLink>
          : <a className="nav-link" href={x.id} key={i}>{x.title}</a>)}
    </nav>
  )
}

export default DocMenu;