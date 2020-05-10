
import { useRef } from 'react'
import Head from 'next/head';
import { faPuzzlePiece } from '@fortawesome/fontawesome-free';
import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import Code from '../../components/Code'
import wdill from '../../public/images/What does it look like.png'
import wdill1 from '../../public/images/What does it look like1.png'
import wdill2 from '../../public/images/What does it look like2.png'
import mixpanel from '../../public/images/mixpanel.png'
import opsgenie from '../../public/images/OpsGenie.png'
import elmah_io from '../../public/images/elmah.io.png'
import wdill3 from '../../public/images/What does it look like3.png'
import sumologic from '../../public/images/SumoLogic.png'
import preval from 'babel-plugin-preval/macro'

export default function Targets() {

  const toc =
  [
    { id: "influxdb-target", title: "InfluxDb Target", ref: useRef(null) },
    { id: "file-target", title: "File target (alpha level)", ref: useRef(null) },
    { id: "stackdriver-target", title: "Stackdriver target", ref: useRef(null) },
    { id: "jtt", title: "Jaeger Tracing target", ref: useRef(null) },
    { id: "alst", title: "AliYun Log Service target", ref: useRef(null) },
    { id: "maait", title: "Microsoft Azure Application Insights target", ref: useRef(null) },
    { id: "commercial-targets", title: "Commercial Targets", ref: useRef(null) },
  ]

  return (
    <DocPage name="all-target" title="Target / Sinks" faIcon={faPuzzlePiece} colour="pink" readingMinutes={2} toc={toc}>
      <Head>
        <title key="title">Target / Sinks</title>
      </Head>
      <DocSection {...toc[0]}>
        <h2 className="section-title">InfluxDb Target</h2>
        <p>
          Suppose you're measuring values coming from a car. This is what that could look like:
        </p>
        <ul>
          <li>Events will be logged to InfluxDb like such:<span className="_code"> "pointName, event=template, ctx1=ctxval1, ctx2=ctxval2 field1=fieldval1, field2=fieldval2 value=1i 14566666xxxx"</span></li>
          <br></br>
          <li>In other words, fields will be influx values and context fields will be influx tags.</li>
          <br></br>
          <li>The timestamp of the Message will be at the end as the timestamp of the sent line</li>
          <br></br>
          <li>
            Events will be logged in these influx measure names, so that you could e.g. put <span className="_code"> "event_fatal" </span>as an annotation in Grafana:

            <ul>
            <br></br>
              <li><span className="_code"> event_verbose </span></li>
              <li><span className="_code"> event_debug </span></li>
              <li><span className="_code"> event_info</span></li>
              <li><span className="_code"> event_warn </span></li>
              <li><span className="_code"> event_error </span></li>
              <li><span className="_code"> event_fatal </span></li>
            </ul>
          </li>
        </ul>
      </DocSection>
      <DocSection {...toc[1]}>
        <h2 className="section-title">File target (alpha level)</h2>
        <p>
          Logary's file target is primarily geared towards systems that are running on single machines as it prints a human-readable format, rather than a machine- readable one.
        </p>

        <h3>Configuration</h3>
          <p>
            The default configuration of the file target rotates log files greater than 200 MiB and deletes log files when the configured folder size is larger than 3 GiB.
          </p>
          <p>
            Folders that don't exist when the target starts are automatically created on target start-up in the current service's security context. Should the calls to create the folder fail, the target is never started, but will restart continuously like any ther Logary target.
          </p>
          <Code language="fsharp" value={
            preval`
            const fs = require('fs')
            const val = fs.readFileSync(__dirname + '/../../../examples/TargetCode/Doc1.fs', 'utf8')
            module.exports = val
            `
          } />
          <p>
            Or in C#:
          </p>
          <Code language="csharp" value={
            preval`
            const fs = require('fs')
            const val = fs.readFileSync(__dirname + '/../../../examples/TargetCode/Doc2.cs', 'utf8')
            module.exports = val
            `
          } />

        <h3>Policies & specifications</h3>
          <p>You can specify a number of deletion and rotation policies when configuring the file target. The deletion policies dictate when the oldest logs should be deleted, whilst the rotation policies dictates when the files should be rotated (thereby the previous file archived).</p>
          <p>Furthermore, you can specify a naming specification that dictates how the files should be named on disk.</p>
          <ul>
            <li>Deletion of files happen directly when at least one deletion policy has triggered.</li>
            <br></br>
            <li>Rotation of files happen directly when at least one rotation policy has triggered.</li>
            <br></br>
            <li>Naming specifications should automatically be amended with sequence number, should that be required.</li>
          </ul>

        <h3>Performance</h3>
          <p>The <span className="_code"> File </span> target is a performance-optimised target. Logging always happens on a separate thread from the caller, so we try to reach a balance between throughput and latency on ACKs.</p>
          <p>On Windows, overlapped IO is not used, because the files are opened in Append mode, should have equivalent performance. This means we should have similar performance on Linux and Windows.</p>
          <p>The formatters used for the <span className="_code">File</span> target should be writing to<span className="_code"> TextWriter</span> instances to avoid creating extra string copies in memory.</p>

        <h3>Handling of errors</h3>
          <p>The file target is thought as a last-chance target, because by default, logs should be shipped from your nodes/machines to a central logging service. It can also be nicely put to use for local console apps that need to log to disk.</p>
          <ul>
            <li>Non-target-fatal <span className="_code">IOExceptions</span>, for example when NTFS ACKs file deletes but still keeps the file listable and available for some duration afterwards are retried on a case-by-case basis. Internal Warn-level messages are logged.</li><br></br>
            <li>Fatal <span className="_code">IOExceptions</span> – more other cases; directory not found, file not found, etc. are not retried. The target should crash and restart. Its current batch is then retried forever, while logging internal Fatal-level exceptions.</li>
          </ul>

        <h3>Invariants</h3>
          <ul>
            <li>The File target is modelled as a transaction log and trades speed against safety that the contents have been written to disk, but does not do the bookkeeping required to use FILE_FLAG_NO_BUFFER.</li>
            <br></br>
            <li>Fatal level events are automatically flushed/fsync-ed.</li>
            <br></br>
            <li>Only a single writer to a file is allowed at any given time. This invariant exists because atomic flushes to files are only possible on Linux up to the page size used in the page cache.</li>
            <br></br>
            <li>Only asynchronous IO is done, i.e. the Logary worker thread is not blocked by calls into the operating system. Because of the overhead of translating callbacks into Job/Alt structures, we try to write as much data as possible on every call into the operating system. This means that Messages to be logged can be ACKed in batches rather than individually.</li>
            <br></br>
            <li>If your disk collapses while writing log messages (which happens once in a while and happens frequently when you have thousands of servers), the target should save its last will and then retry a configurable number of times after waiting an exponentially growing duration between each try. It does this by crashing and letting the supervisor handle the failure. After exhausting the tries, the batch of log messages is discarded.</li>
            <br></br>
            <li>If there are IO errors on writing the log messages to disk, there's no guarantee that there won't be duplicate log lines written; however, they're normally timestamped, so downstream log ingestion systems can do de-duplication. This is from the batched nature of the File target.</li>
            <br></br>
          </ul>

        <h3>Overview of buffers</h3>
          <ol type="1">
            <li>
              You write a Message from your call-site, this message is synchronised upon between the sending thread and the receiving thread using Hopac.

              <ol type="I">
              <br></br>
                <li>If you use one of the logWithAck functions, placing the message in the RingBuffer can be awaited (or NACKed)</li>
                <li> If you use the logSimple function, the synchronisation is hoisted onto the concurrency scheduler's pending queue and raced with a timeout to be discarded if the logging subsystem is overwhelmed.</li>
              </ol>
            </li>
            <br></br>
            <li>Once the Message is in the RingBuffer of the File target, it's either removed by itself, or as part of a batch, to be serialised to string.</li>
            <br></br>
            <li>The serialisation function reads through the values of the message and uses the formatter function to write those values into a TextWriter. The TextWriter is normally a StreamWriter writing to a FileStream. This means no extra strings need be created through concatenation.</li>
            <br></br>
            <li>Depending on the inProcBuffer configuration flag, the TextWriter either supports buffering, which buffers the string inside the CLR process, or writes directly to the underlying file handle, which transitions the data to the kernel's ioctl subsystem. By default we don't buffer here.</li>
            <br></br>
            <li>Depending on the flushToDisk configuration flag, the FileStream is or is not called with Flush(true), which forces a disk synchronisation. By default we let the page cache buffer these writes, to trade safety against throughput. This is similar to how most other targets work.</li>
            <br></br>
            Depending on the writeThrough flag; Messages written with the File target is only ACKed when they are durably on disk. Defaults to true.
            <br></br>
          </ol>
          <p>Note that disposing Logary, e.g. during application exit flushes all buffers.</p>

        <h3>Notes on FILE_FLAG_NO_BUFFERING</h3>
          <p>I've been considering supporting <a href="https://docs.microsoft.com/en-us/windows/desktop/FileIO/file-buffering"> NO_BUFFERING </a>but this would require callers to possibly wait for the 4096 bytes buffer to fill up before ACKing messages. However, for low-throughput logging, where each log line may be around, say, 240 bytes of text, having the NO_BUFFERING flag set may end up losing us more than it gains us.</p>
          <h5>References</h5>
            <ul>
              <li><a href="https://support.microsoft.com/en-us/kb/99794">https://support.microsoft.com/en-us/kb/99794</a></li>
              <br></br>
              <li><a href="https://stackoverflow.com/questions/317801/win32-write-to-file-without-buffering">https://stackoverflow.com/questions/317801/win32-write-to-file-without-buffering</a></li>
              <br></br>
              <li><a href="https://winntfs.com/2012/11/29/windows-write-caching-part-2-an-overview-for-application-developers/">https://winntfs.com/2012/11/29/windows-write-caching-part-2-an-overview-for-application-developers/</a></li>
              <br></br>
              <li><a href="https://msdn.microsoft.com/en-us/library/windows/desktop/cc644950(v=vs.85).aspx">https://msdn.microsoft.com/en-us/library/windows/desktop/cc644950(v=vs.85).aspx</a></li>
              <br></br>
              <li><a href="https://msdn.microsoft.com/en-us/library/windows/desktop/aa363772(v=vs.85).aspx">https://msdn.microsoft.com/en-us/library/windows/desktop/aa363772(v=vs.85).aspx</a></li>
              <br></br>
              <li><a href="https://stackoverflow.com/questions/8692635/how-do-disable-disk-cache-in-c-sharp-invoke-win32-createfile-api-with-file-flag">https://stackoverflow.com/questions/8692635/how-do-disable-disk-cache-in-c-sharp-invoke-win32-createfile-api-with-file-flag</a></li>
              <br></br>
              <li><a href="https://stackoverflow.com/questions/122362/how-to-empty-flush-windows-read-disk-cache-in-c">https://stackoverflow.com/questions/122362/how-to-empty-flush-windows-read-disk-cache-in-c</a></li>
              <br></br>
              <li><a href="https://ayende.com/blog/174785/fast-transaction-log-windows">https://ayende.com/blog/174785/fast-transaction-log-windows</a></li>
              <br></br>
            </ul>

          <h5>Example runs</h5>
            <p>These runs illustrate the above points in a more direct manner. In all of these cases we're writing 10K events to disk.</p>
            <pre>inProcBuffer = false, flushToDisk = true, caller awaits all acks at the end</pre>
            <p>This is the safest option and takes 1.3 seconds to log, format and write 10K messages.</p>
            <pre>
              <code>
              I 2016-11-08T11:04:00.6125063+00:00: Event 1 [Logary.Samples.main]
              <br></br>
                number => 1
                <br></br>
              ...
              <br></br>
              [12:04:02 DBG] Flushing to disk.
              <br></br>
              ...
              <br></br>
              I 2016-11-08T11:04:02.0201345+00:00: Event 9402 [Logary.Samples.main]
              <br></br>
                number => 9402
                <br></br>
              [12:04:02 DBG] Flushing to disk.
              <br></br>
              I 2016-11-08T11:04:02.0201345+00:00: Event 9403 [Logary.Samples.main]
              <br></br>
                number => 9403
                <br></br>
              I 2016-11-08T11:04:02.0201345+00:00: Event 9404 [Logary.Samples.main]
              <br></br>
                number => 9404
                <br></br>
              ...
              <br></br>
              I 2016-11-08T11:04:02.0891350+00:00: Event 10000 [Logary.Samples.main]
              <br></br>
                number => 10000
                <br></br>
              [12:04:02 DBG] Flushing to disk.
              <br></br>
              ...
              <br></br>
              </code>
            </pre>
            <p>The interleaved flushes shows the batching functionality of the File target in action.</p>
            <pre>inProcBuffer = false, flushToDisk = true, caller awaits all ack after each</pre>
            <p>This example represents the worst-case usage of the safest configuration.</p>
            <pre>
              <code>
              I 2016-11-08T11:14:42.9071732+00:00: Event 1 [Logary.Samples.main]
              <br></br>
                number => 1
                <br></br>
              <br></br>
              [12:14:42 DBG] Flushing to disk.
              <br></br>
              I 2016-11-08T11:14:42.9711735+00:00: Event 2 [Logary.Samples.main]
              <br></br>
                number => 2
                <br></br>
                [12:14:42 DBG] Flushing to disk.
              <br></br>
              I 2016-11-08T11:04:02.0201345+00:00: Event 9403 [Logary.Samples.main]
              <br></br>
                number => 3
                <br></br>
                [12:14:42 DBG] Flushing to disk.
                <br></br>
              I 2016-11-08T11:04:02.0201345+00:00: Event 9404 [Logary.Samples.main]
              <br></br>
                number => 4
                <br></br>
                [12:14:42 DBG] Flushing to disk.
                <br></br>
                ...
              <br></br>
              I 2016-11-08T11:15:04.7635448+00:00: Event 10000 [Logary.Samples.main]
                <br></br>
                number => 10000
                <br></br>
                [12:15:04 DBG] Flushing to disk.
              <br></br>
              </code>
            </pre>
            <p>With this configuration, the File target would still batch other threads' Messages but since this example has a single thread producer, there's only a single Message available for the target every loop.</p>
            <pre>inProcBuffer = true, flushToDisk = false, writeThrough=false caller awaits all acks at the end</pre>
            <p>This is the least safe and most speedy option. Useful when you're shipping logs away from the node and configure those shippers in a safer manner. In this case, .Net and the operating system and the device drivers decide when to flush.</p>
            <p>On exit/dispose of Logary, all targets are always flushed.</p>
            <pre>
              <code>
                [12:32:05 INF] Event 1
                <br></br>
                ...
                <br></br>
                [12:32:06 INF] Event 10000
                <br></br>
                [12:32:48 DBG] Shutting down Logary.
                <br></br>
                ...
                <br></br>
                [12:32:48 DBG] Flushing to disk.
                <br></br>
              </code>
            </pre>
            <p>In this example, the actual time taken is dominated by the time to generate the messages.</p>
        <h3>Work to be done</h3>
          <ul>
            <li>Unit test rotation code</li>
            <li>Then enable rotation</li>
            <li>Harden against exceptions during writes – mock FileSystem</li>
          </ul>
      </DocSection>
      <DocSection {...toc[2]}>
        <h2 className="section-title">Stackdriver target</h2>
        <p>
          Development has been sponsored by <a href="https://www.tradera.com/?utm_source=logary"> Tradera.com. </a>
        </p>
        <p>
          Logary also includes a logging target for <a href="https://cloud.google.com/stackdriver"> Google Cloud Stackdriver. </a>
        </p>
        <h3>Configuration</h3>
          <p>The target can be configured like so:</p>
          <Code language="fsharp" value={
            preval`
            const fs = require('fs')
            const val = fs.readFileSync(__dirname + '/../../../examples/TargetCode/Doc3.fs', 'utf8')
            module.exports = val
            `
          } />
          <p>Then, within withTargets:</p>
          <pre>Stackdriver.create conf "target-name"</pre>

        <h3>Further work</h3>
          <ul>
            <li>batching</li>
            <li>
              flushing
              <ul>
                <li>the underlying library doesn't provide a flush mechanism yet</li>
              </ul>
            </li>
          </ul>
      </DocSection>
      <DocSection {...toc[3]}>
        <h2 className="section-title">Jaeger Tracing target</h2>
        <ul>
          <br></br>
          <li><a href="https://www.jaegertracing.io/"></a> https://www.jaegertracing.io/</li>
        </ul>
        <h3>Install jaeger tracing</h3>
          <ul>
            <br></br>
            <li><a href="https://www.jaegertracing.io/download/"></a> https://www.jaegertracing.io/download/</li>
          </ul>
        <h3>Usage</h3>
          <p>add ambientSpanId middleware to the target, if you want to use ambient span</p>
          <pre>jaegerTargetConf |> TargetConf.middleware Middleware.ambientSpanId</pre>
          <p>then create span for some tracing, log message as usual:</p>
          <Code language="fsharp" value={
            preval`
            const fs = require('fs')
            const val = fs.readFileSync(__dirname + '/../../../examples/TargetCode/Doc4.fs', 'utf8')
            module.exports = val
            `
          } />
          <p>if not using ambient span, you can use Message.setSpanId for log message and Span.setParentSpanInfo for childSpan creation.</p>
          <Code language="fsharp" value={
            preval`
            const fs = require('fs')
            const val = fs.readFileSync(__dirname + '/../../../examples/TargetCode/Doc5.fs', 'utf8')
            module.exports = val
            `
          } />
        <h3>What does it look like?</h3>
          <img className="wdill" src={wdill} alt="What does it look like" />
      </DocSection>
      <DocSection {...toc[4]}>
        <h2 className="section-title">AliYun Log Service target</h2>
        <h3>Usage</h3>
          <Code language="fsharp" value={
              preval`
              const fs = require('fs')
              const val = fs.readFileSync(__dirname + '/../../../examples/TargetCode/Doc6.fs', 'utf8')
              module.exports = val
              `
            } />
        <h3>What does it look like?</h3>
          <img className="wdill" src={wdill2} alt="What does it look like" />
          <img className="wdill" src={wdill1} alt="What does it look like" />
      </DocSection>
      <DocSection {...toc[5]}>
        <h2 className="section-title">Microsoft Azure Application Insights target</h2>
        <p>
          Target for <a href="https://docs.microsoft.com/en-us/azure/azure-monitor/overview"> Microsoft Azure AppInsights </a> logs the events as TRACE-messages (or Events/Metrics with a different
           MappingConfiguration). You need to set the API-key first. Then when you go to Azure Portal Application Insights and
           <span className="_code"> Overview -> Search </span> you should be able to find the targets from there. Metrics goes to <span className="_code"> Metrics Explorer -> Add Chart ->
           Custom. </span> <a href="https://docs.microsoft.com/en-us/azure/azure-monitor/app/create-new-resource">More info...</a>
        </p>
      </DocSection>
      <DocSection {...toc[6]}>
        <h2 className="section-title">Commercial Targets</h2>
        <p>Logary is a production-grade logging and metrics library. We've also built targets that integrate with external paid services. These are listed here.</p>
        <h3>Mixpanel</h3>
          <img src={mixpanel} alt="" />
          <p>Learn how people use your app with the world's most advanced mobile & web analytics.</p>
          <p>[Purchase today](mailto:henrik@haf.se?subject=Logary Mixpanel Target)</p>
          <h5>Features</h5>
            <ul>
              <li>Ship logs from your iOS, Android app</li>
              <br></br>
              <li>Ship logs and handle user identification and unique-id tracking from web</li>
              <br></br>
              <li>Use your own domain and server (over HTTPS)</li>
              <br></br>
              <li>Logary listens on your server and forwards your events into Mixpanel</li>
              <br></br>
              <li>Add granular server-side event filtering/enriching/correlation for better insights before shipping them onwards.</li>
              <br></br>
              <li>Log web app usage even when Mixpanel is blocked client-side</li>
              <br></br>
            </ul>
          <h5>What's included?</h5>
            <p>We like open source – so in the purchase the reference source is provided so that it can be debugged like the rest of Logary.</p>
            <p>Send an e-mail to purchase</p>
            <p>This assumes you have an account at <a href="https://mixpanel.com/">Mixpanel.</a> </p>

        <h3>OpsGenie</h3>
          <img src={opsgenie} alt="" />
          <p>You can't rely on any one notification method for critical alerts. Get alert notifications via iOS & Android push, SMS, and phone calls; escalate automatically to team members if the alert is not acknowledged.</p>
          <p>The Logary target for OpsGenie ensures that you can bring in your Logging and Metrics into your daily operations.</p>
          <h5>Features</h5>
            <ul>
              <li>Connect using your own API key</li>
              <br></br>
              <li>Make Logary events into new alerts</li>
              <br></br>
              <li>Supports custom 'enrichers' to let you specify e.g. user, teams, recipients, tags, entity and notes, to name a few.</li>
              <br></br>
              <li>Ready to use from both F# and C#</li>
              <br></br>
              <li>Use derived metrics to create load-level alerts</li>
              <br></br>
              <li>Stay on top of your infrastructure</li>
              <br></br>
              <li>Avoid blacklisting your transactional e-mail service</li>
              <br></br>
            </ul>
          <p>This assumes you have an account at <a href="https://www.opsgenie.com/"> OpsGenie.</a></p>

        <h3>elmah.io</h3>
          <a href="https://elmah.io/"> <img src={elmah_io} ></img> </a>
          <p></p>
            <p><span className="_code"> source https://www.nuget.org/api/v2 </span></p>
            <p><span className="_code"> nuget Logary.Targets.Elmah.Io </span></p>
          <p>OR:</p>
            <p><span className="_code"> Install-Package Logary.Targets.Elmah.Io </span></p>
          <h5>Usage</h5>
            <p>Configure elmah.io just like you would any normal target.</p>
            <Code language="fsharp" value={
              preval`
              const fs = require('fs')
              const val = fs.readFileSync(__dirname + '/../../../examples/TargetCode/Doc7.fs', 'utf8')
              module.exports = val
              `
            } />
            <br></br>
            <p>Or from C#:</p>
            <Code language="fsharp" value={
              preval`
              const fs = require('fs')
              const val = fs.readFileSync(__dirname + '/../../../examples/TargetCode/Doc8.cs', 'utf8')
              module.exports = val
              `
            } />
            <img src={wdill3}></img>
            <p></p>
            <p>You'll get the same view by logging this Message:</p>
            <Code language="fsharp" value={
              preval`
              const fs = require('fs')
              const val = fs.readFileSync(__dirname + '/../../../examples/TargetCode/Doc9.fs', 'utf8')
              module.exports = val
              `
            } />
            <p>This assumes you have an account at <a href="https://elmah.io/"> elmah.io.</a></p>

        <h3>SumoLogic (community-contributed)</h3>
        <a href="https://www.sumologic.com/"><img src={sumologic}></img></a>
        <p></p>
        <p>SumoLogic is a hosted service (at about 99 USD per month) that unifies logging, metrics, analytics and dashboards in a single service. As such it's a perfect Target for Logary, since Logary supports both logs and metrics.</p>
        <p>Have a look at @neoeinstein's <a href="https://neoeinstein.github.io/Logary.Targets.SumoLogic/"> Logary.Targets.SumoLogic </a> for the official docs and a sample of how to use it.</p>
        <span className="_code"> source https://www.nuget.org/api/v2 </span>
        <br></br>
        <span className="_code"> nuget Logary.Targets.SumoLogic </span>
        <p></p>

        <h3>Want your SaaS-logging service as a Target?</h3>
        <p>Absolutely! You have two options;</p>
        <ol type="1">
          <p><li>Send a PR with your target that is of equivalent quality as the rest of the code-base, including documentation, code-doc, the C# builder API and a sample in this file. Then keep that code up-to-date when Logary evolves and your SaaS service changes its APIs.</li></p>
          <p><li>Send me an e-mail and I'll target the target for you. Pricing: a small initial fee and then a monthly maintenance fee, you'll have a beautiful way of getting logs and metrics to your servers!.</li></p>
          <p>This is by far the easiest option and ensures that your Target is stable and easy to use for your customers. I'll even write some Markdown/HTML-formatted docs for your site about how to use Logary with your target.</p>
        </ol>

      </DocSection>
    </DocPage>
  )
}

//