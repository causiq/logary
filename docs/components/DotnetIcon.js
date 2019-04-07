import PropTypes from 'prop-types'

const DotnetIcon = (props) =>
  <svg
    version="1.1"
    xmlns="http://www.w3.org/2000/svg"
    xmlnsXlink="http://www.w3.org/1999/xlink"
    x="0px" y="0px"
    viewBox="0 0 512 512"
    xmlSpace="preserve"
    style={{
      width: `${props.width || 64}px`
    }}>
    <rect style={{fill: props.background || '#6800EE'}} className="bg" width="100%" height="100%"/>
    <g>
      <path style={{fill:'#F2F2F2'}} className="text" d="M193.24,439.48c0-5.15,3.48-8.78,8.36-8.78c4.88,0,8.22,3.62,8.22,8.78c0,5.01-3.2,8.78-8.36,8.78
        C196.59,448.25,193.24,444.49,193.24,439.48z"/>
      <path style={{fill:'#F2F2F2'}} className="text" d="M225.42,446.72v-93.89h13.23l30.09,47.5c6.96,11,12.4,20.89,16.86,30.51l0.28-0.14
        c-1.11-12.54-1.39-23.96-1.39-38.58v-39.28h11.42v93.89h-12.26l-29.81-47.64c-6.55-10.45-12.82-21.17-17.55-31.34l-0.42,0.14
        c0.7,11.84,0.98,23.12,0.98,38.72v40.12H225.42z"/>
      <path style={{fill:'#F2F2F2'}} className="text" d="M365.69,402.7h-36.5v33.85h40.67v10.17h-52.79v-93.89h50.7V363H329.2v29.67h36.5V402.7z"/>
      <path style={{fill:'#F2F2F2'}} className="text" d="M405.67,363.14h-28.56v-10.31h69.51v10.31h-28.7v83.58h-12.26V363.14z"/>
    </g>
  </svg>

DotnetIcon.propTypes = {
  width: PropTypes.number,
  background: PropTypes.string
}

export default DotnetIcon;