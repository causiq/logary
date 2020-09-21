import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import rutta from '../../public/images/logary-rutta-router.svg'
import use1 from '../../public/images/usage1.png'
import use2 from '../../public/images/usage2.jpg'
import Code from '../../components/Code'

export default function Rutta() {
  return (
    <DocPage
      name="rutta"
      title="Rutta ᜑ high performance log router 🦋"
      colour="blue">

      <DocSection title='Usage' id='usage'>
        <p>Route, forward and print logs anywhere to anything.</p>
        <img src={use1} className="usage"></img>
      </DocSection>

      <DocSection title='With Docker' id='docker'>
        <Code lang='text' value='docker run -p 10001:10001 --rm -it logary/rutta:latest router --listener tcp 0.0.0.0:10001 json --target console://./' />
        <img src={use2} className="usage"></img>
      </DocSection>

      <DocSection title='With Kubernetes' id='k8s'>
        <p>Logary Rutta works great for shipping logs from nodes into a central location.</p>

        <p>
          You can either deploy Logary Rutta as a DaemonSet, which will cause it to appear on every node in your
          cluster. The Kubernetes code is available in <a href='https://github.com/logary/logary/tree/master/src/services/Logary.Services.Rutta/k8s'>
            src/service/Logary.Services.Rutta/k8s
          </a>
        </p>

        <Code lang='text' value='kustomize build k8s/as-daemonset | kubectl apply -f -' />

        <p>or you can deploy it as a load-balanced Deployment that is used by multiple nodes;</p>

        <Code value={`
# useful when you only have one node and you're testing:
kustomize build k8s/as-deployment | kubectl apply -f -

# alternative, for production:
kustomize build k8s/as-deployment-with-scaling | kubectl apply -f -
        `}/>

        <p>In your kustomization.yaml file, you might have:</p>

        <Code value={`
apiVersion: kustomize.config.k8s.io/v1beta1
kind: Kustomization

resources:
- github.com/logary/logary/src/services/Logary.Services.Rutta/k8s/as-deployment
# alt:
# - github.com/logary/logary/src/services/Logary.Services.Rutta/k8s/as-deployment-with-scaling

namespace: logary`} />

        It's no harder than that!
      </DocSection>

      <DocSection title='In depth' id='in-depth'>
        <p>Rutta is GPLv3 licensed, that you can run, either as a sidecar container or as a log router deployment or as a daemonset.</p>
        <p>Rutta is completely stateless, so you can run any number of replicas. It runs as a Kubernetes deployment with three replicas by default.</p>
        <p>A common configuration for Rutta is to configure a Stackdriver target, a HTTP ingestion listener as well as a UDP ingestion listener. Your apps send UDP log messages to the UDP endpoint and your frontends (native apps and web sites) send HTTP messages to the HTTP endpoint. Rutta when batch-ships these log messages into Stackdriver.</p>
        <p>By default this chart exposes a HTTP listener/endpoint and prints to console; in order for it to log to Stackdriver, AliYun or AppInsights, you have to configure those explicitly in the values file.</p>
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

      <DocSection title='Router mode' id='rutta-router-mode'>
        <p>The router mode lets you take inputs from a `listener` (tcp, udp, ...), interpret it with a `codec` and then send it to a `target`.</p>
        <img src={rutta} width="100%" alt="Rutta in Router mode" />
      </DocSection>
    </DocPage>
  )
}