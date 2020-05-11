import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import PrometheusIcon from '../../components/PrometheusIcon'
import Code from '../../components/Code'
import preval from 'babel-plugin-preval/macro'

export default function Vision() {
  return (
    <DocPage name="js-quickstart" title="Prometheus" icon={<PrometheusIcon />} colour="orange">
      <DocSection title='Prometheus' id='prometheus'>
        <p>
          Logary supports Prometheus. Reference <span className="_code">Logary.Prometheus</span> and create metrics:
        </p>
        <p>TBD</p>
        <Code language="fsharp" value={
          preval`
            const fs = require('fs')
            const val = fs.readFileSync(__dirname + '/../../examples/dotnet/Prometheus.fs', 'utf8')
            module.exports = val
          `
        } />
      </DocSection>
    </DocPage>
  )
}