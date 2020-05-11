import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import rutta from '../../public/images/logary-rutta-router.svg'
import { faSatelliteDish } from '@fortawesome/fontawesome-free'
import use1 from '../../public/images/usage1.png'
import use2 from '../../public/images/usage2.jpg'

export default function Rutta() {
  return (
    <DocPage
      name="rutta"
      title="Rutta – cloud native log router and event/log ingress"
      faIcon={faSatelliteDish}
      colour="blue">

      <DocSection title='Usage' id='usage'>
        <p>Route, forward and print logs anywhere to anything.</p>
        <img src={use1} className="usage"></img>
      </DocSection>

      <DocSection title='With Docker' id='docker'>
        <p><pre>docker run -p 10001:10001 --rm -it haaf/rutta router --listener tcp 0.0.0.0:10001 json --target console://./</pre></p>
        <img src={use2} className="usage"></img>
      </DocSection>

      <DocSection title='In depth' id='in-depth'>
        <p>Rutta is software for shipping Messages between computers. Either from your own services or from Windows Performance Counters. This is useful if you want your services to ship all logs to a central point, before batching it and sending it off to InfluxDb. It's also useful if you want to firewall off a single subnet for certain processing and only have a single point ship logs and metrics.</p>
        <ul>
          <li>v1: Hard-coded supported target types. Initially we'll just support InfluxDB.</li>
          <li>v2: More configurable target configuration that supports any target.</li>
        </ul>
        <p>This service can run in three modes; Shipper, Router and Proxy. Servers can be implemented using Hopac's lightweight servers. Communication is implemented using ZMQ and a binary serialisation format.</p>
        <p>Bindings look may look like this:</p>
        <ul>
          <li><span className="_code">Shipper -> Router</span></li>
          <li><span className="_code">Shipper -> Proxy</span></li>
          <li><span className="_code">Proxy -> Proxy</span></li>
          <li><span className="_code">Proxy -> Router</span></li>
        </ul>
        <p></p>
        <p> <a href="http://api.zeromq.org/3-2:zmq-socket">ZMQ socket reference</a></p>
        <p>On Windows you do <span className="_code"> ./rutta.exe -- --pub-to ... </span> - note the two extra dashes before the parameter list. This is to avoid Topshelf munching the arguments away.</p>
      </DocSection>

      <DocSection title='The Shipper — from env to Proxy or Router' id='shipper'>
        <p>Enables log shipping from hosts that are not directly connected to the router nor to InfluxDB. </p>
        <h5>Pushing Shippers</h5>
        <p>Shippers CONNECT PUSH sockets to the Router's PULL socket. See <a href="https://lists.zeromq.org/pipermail/zeromq-dev/2012-February/015917.html"> http://lists.zeromq.org/pipermail/zeromq-dev/2012-February/015917.html</a></p>
        <p>During network splits, the sending <a href="http://api.zeromq.org/3-2:zmq-socket#toc14">PUSH socket blocks</a>.</p>
        <h5>Pushing Shippers</h5>
        <p>During network splits, the sending XPUSH socket drops messages.</p>
      </DocSection>

      <DocSection title='The Proxy — from Shipper to Router' id='proxy'>
        <p>Proxies take inputs from Shippers or other Proxies that publish Messages using XPUB sockets:</p>
        <p>The Proxy is run this way, by providing a XSUB socket binding and a XPUB socket binding:</p>
        <p>During network splits, the receiving <a href="http://api.zeromq.org/3-2:zmq-socket#toc12"> XSUB socket drops messages.</a></p>
        <p>You can then connect to the Proxy with a Router that routes it to the final Target (like InfluxDB in this example):</p>
        <p>During network splits, the sending <a href="http://api.zeromq.org/3-2:zmq-socket#toc11"> XPUB socket drops messages.</a></p>
      </DocSection>

      <DocSection title='The Router — from Shipper/Proxy to Target' id='router'>
        <p>Implements Fan-In using PULL or SUB of Messages from ZMQ. Forwards internally to a Target.</p>
        <p>V1 only implements the InfluxDB target.</p>
        <h5>Pulling Routers</h5>
        <p>BINDs a PULL socket on a specified NIC/IP and PORT. Configures a single internal Target that pushes the received data.</p>
        <p>During network splits, the listening <a href="http://api.zeromq.org/3-2:zmq-socket#toc15"> PULL socket blocks. </a></p>
        <h5>Subscribing Routers</h5>
        <p>BINDs a SUB socket on a specified NIC/IP and POST. Configures a single internal Target that pushes the received data.</p>
        <p>Serialisation for Rutta is done using <a href="http://mbraceproject.github.io/FsPickler/">FsPickler</a>. Since FsPickler uses a binary format, it should be assumed to break for any given minor upgrade of FsPickler.</p>
        <p>Each ZMQ message contains a Message (see DataModel.fs) in the binary form given by the serialiser chosen.</p>
      </DocSection>

      <DocSection title='Helm chart' id='helm-chart'>
        <h2 className="section-title">Logary Rutta Helm chart</h2>
        <p>The first step is to install the Rutta Helm chart into your Kubernetes cluster.</p>
        <p>Rutta is a high-performance log router/shipper written in F# but configured from the command line. It's "cloud native" in that it runs as a docker container on top of .Net Core</p>
        <p>Rutta is GPLv3 licensed (or alternatively commercially licensed). It's a packaging of Logary as a service, that you can run, either as a sidecar container or as a log router deployment.</p>
        <p>Rutta is completely stateless, so you can run any number of replicas. It runs as a Kubernetes deployment with three replicas by default.</p>
        <p>A common configuration for Rutta is to configure a Stackdriver target, a HTTP ingestion listener as well as a UDP ingestion listener. Your apps send UDP log messages to the UDP endpoint and your frontends (native apps and web sites) send HTTP messages to the HTTP endpoint. Rutta when batch-ships these log messages into Stackdriver.</p>
        <p>By default this chart exposes a HTTP listener/endpoint and prints to console; in order for it to log to Stackdriver, AliYun or AppInsights, you have to configure those explicitly in the values file.</p>
        <p>Have a look at the values.yaml file in order to get an idea of what you can configure.</p>

        <h3 className="section-title">Install the chart</h3>
        <p>
          The first step is to install the Rutta Helm chart into your Kubernetes cluster.
        </p>
        <code><pre>{
          `helm install https://github.com/logary/logary/tree/master/src/services/rutta-helm-chart && \\
    --name rutta && \\
    --namespace monitoring`
        }</pre></code>
        <p>
          Or if you've downloaded the chart;
        </p>
        <code><pre>{
          `helm install ./rutta-helm-chart && \\
    --name rutta && \\
    --namespace monitoring`
        }</pre></code>
        <p>
          If you use a local values file, you can upgrade the chart.
        </p>
        <code><pre>{
          `helm upgrade --debug --install rutta && \\
    ./rutta-helm-chart && \\
    --namespace monitoring && \\
    --values values/rutta.yaml`
        }</pre></code>
      </DocSection>

      <DocSection title='Router mode' id='rutta-router-mode'>
        <h2 className="section-title">Rutta Router mode</h2>
        <p>The router mode lets you take inputs from a `listener` (tcp, udp, ...), interpret it with a `codec` and then send it to a `target`.</p>
        <img src={rutta} alt="Rutta in Router mode" />
      </DocSection>
    </DocPage>
  )
}