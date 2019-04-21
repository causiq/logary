
import { useRef } from 'react'
import Head from 'next/head';
import { faLifeRing } from '@fortawesome/pro-light-svg-icons';
import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import Code from '../../components/Code'
var low = require('lowlight')
var tree = low.highlight('js', '"use strict";').value

// import stn from '../../images/SinkTargetNames.png'




export default function FAQs() {

  const toc =
  [ 
    { id: "faq", title: "FAQ", ref: useRef(null) },
    { id: "ctnal", title: "Comparison to NLog and log4net", ref: useRef(null) },
    { id: "ctcmm", title: "Comparison to Codahale metrics & Metrics.NET", ref: useRef(null) },
    { id: "cws", title: "Comparison with Serilog", ref: useRef(null) },
    
  ]
  
  return (
    <DocPage name="all-target" title="FAQs" faIcon={faLifeRing} colour="green" readingMinutes={2} toc={toc}>
      <Head>
        <title key="title">FAQs</title>
      </Head>
      <DocSection {...toc[0]}>
        <h2 className="section-title">FAQ</h2>
        <h3>Getting MissingMethodException from FSharp.Core</h3>
          <p>You need to add a rebind to the latest F# version in your executable:</p>
          <Code language="xml" value={
              preval`
              const fs = require('fs')
              const val = fs.readFileSync(__dirname + '/../../../examples/FAQs/Doc1.xml', 'utf8')
              module.exports = val
              `
            } />

        <h3>Getting MissingMethodException from Hopac.Core</h3>
          <p>Inspect the version specified in the <a href="https://www.nuget.org/packages/Logary/"> Logary package </a> and ensure that you have that exact version installed. Hopac is currently pre-v1 so it is often doing breaking changes between versions.</p>
        
        <h3>Is v5.0.x a stable version?</h3>
          <p>It's stable to run. The API is alpha.</p>
        
        <h3>Isn't v4.0.x supposed to be API-stable?</h3>
          <p>We're not doing pre-release versions because they make it impossible for other packages to be released as stable versions. But we need to work through Logary in production; as such you can imagine that qvitoo is taking the risk and cost of making v4.0 RTM as stable and reliable as can be.</p>

        <h3>Why does Logary depend on FParsec?</h3>
          <p>For tow reasons;</p>
          <ol type="1">
            <li>we use Chiron for json formatting which depend on FParsec</li>
            <li>Aether is vendored in Logary.Utils.Aether and depend on it.</li>
          </ol>
          <p>We previously depended on Newtonsoft.Json, but that library is often depended on from other packages and we want Logary to be as free of dependencies as possible, in order to make it as stable as possible.</p>
        
        <h3>Why do you depend on Hopac?</h3>
          <p>Hopac supports a few things that async doesn't:</p>
          <ol type="1">
            <li>Rendezvous and selective concurrency primitives (select A or B)</li>
            <li>Negative ACKs instead of CancellationToken-s</li>
          </ol>
          <p>We also wanted support for synchronous rendezvous between channels/job/alts/promises/etc. This still supports asynchronous operations towards the outside. Together it makes for an excellent choice for cooperating 'agents', like the Registry and Supervisor and Target Instance that we have in the library.</p>
          <p>Besides the technical upsides, it's a good thing there's a book written about the concurrency model that Hopac implements – <a href="https://www.amazon.com/Concurrent-Programming-ML-John-Reppy/dp/0521714729/"> Concurrent Programming in ML </a> which lets us get developers up to speed quickly.</p>
          <p>Finally, our unit tests sped up 30x when porting from Async. The performance boost is a nice feature of a logging framework and comes primarily from less GC collection and the 'hand off' between synchronising concurrency primitives being synchronously scheduled inside Hopac rather than implemented using Thread/Semaphore/Monitor primitives on top of the ThreadPool.</p>
        
        <h3>How do I use Hopac from C#?</h3>
          <p>You're better off following the examples in C# and using the Task-wrapped public APIs than going spelunking into the dire straits of Hopac and F#.</p>
          <p>Just pull in Logary.CSharp to make this happen. You'll also have to open the Logary namespace.</p>

        <h3>What's logVerboseWithAck, logWithAck and how does it differ from logSimple?</h3>
          <p>To start with, if you're new to Logary, you can use logSimple and it will work like most other logging frameworks. So what are those semantics exactly?</p>
          <p>Logary runs its targets concurrently. When you log a Message, all targets whose Rules make it relevant for your Message, receives the Message, each target tries to send that Message to its, well, target.</p>
          <p>Because running out of memory generally is unwanted, each target has a <a href="https://github.com/logary/RingBuffer"> RingBuffer </a> that <a href="https://github.com/logary/logary/blob/4987c421849464d23b61ea4b64f8e48a6df21f12/src/Logary/Internals_Logger.fs#L13-L21"> the messages are put into </a> when you use the Logger. Unless all targets' RingBuffer accept the Message, the call to log doesn't complete. This is similar to how other logging frameworks work.</p>
          <p>But then, what about the call to log? Behind the scenes it calls lockWithAck and tries to commit to the returned Alt [Promise [unit]] (the outer Alt, that is). If the RingBuffer is full then this Alt cannot be committed to, so there's code that drops the log message after 5000 ms.</p>
          <p>Hence; logSimple tries its best to log your message but if you app crashes directly after calling logSimple or your Logstash or other target infrastructure is down, you cannot be sure everything is logged. The decision was made that it's more important that your app keeps running than that all targets you have configured successfully log your Messages.</p>
          <h5>logWithAck – so what's up with Promise?</h5>
            <p>The outer Alt ensures that the Message has been placed in all configured targets' RingBuffers.</p>
            <p>The inner Promise that the Message has successfully been written from all Targets that received it. It ensures that your logging infrastructure has received the message.</p>
            <p>It's up to each target to deal with Acks in its own way, but a 'best-practices' Ack implementation can be seen in the RabbitMQ target. It's a best-practices Ack implementation because RabbitMQ supports publisher confirms (that serve as Acks), asynchronous publish and also durable messaging.</p>
          <h5>How do Promises work with C#?</h5>
            <p>The C# signature of the above functions is as follows:</p>
            <Code language="cs" value={
              preval`
              const fs = require('fs')
              const val = fs.readFileSync(__dirname + '/../../../examples/FAQs/Doc2.cs', 'utf8')
              module.exports = val
              `
            } />
            <p>and can be used like so:</p>
            <Code language="cs" value={
              preval`
              const fs = require('fs')
              const val = fs.readFileSync(__dirname + '/../../../examples/FAQs/Doc3.cs', 'utf8')
              module.exports = val
              `
            } />
      </DocSection>
      <DocSection {...toc[1]}>
        <h2 className="section-title">Comparison to NLog and log4net</h2>
        <p>
          Why Logary instead of one of the classic logging frameworks?
        </p> 
        <ul>
          <li>You get semantic logging with Logary</li>
          <br></br>
          <li>More targets to choose from</li>
          <br></br>
          <li>Larger community of target writers</li>
          <br></br>
          <li>Easier to write targets; they can crash and that's handled by Logary internally</li>
          <br></br>
          <li>Support for zero-dependency usage through Logary.Facade</li>
          <br></br>
          <li>Better/more extensive Rule-based hierarchies</li>
          <br></br>
          <li>Targets can be decoupled from the network and Ack is a first-level primitive</li>
          <br></br>
          <li>You get back an <span className="_code"> Alt (Promise (unit) )</span> that you can use to synchronise your calling code for when the log message is required to be durable; you can't do this with NLog or log4net</li>
          <br></br>
          <li>There's an object model you can use from the calling code</li>
          <br></br>
          <li>Logary is F#, so it's easier to keep bug-free relative to many other languages</li>
          <br></br>
          <li>Logary doesn't keep static state around; easy to refactor, easy to extend</li>
          <br></br>
        </ul>
      </DocSection>
      <DocSection {...toc[2]}>
        <h2 className="section-title">Comparison to Codahale metrics & Metrics.NET</h2>
        <p>Why Logary rather than Metrics.NET, the primary alternative?</p> 
        <p>In order to understand the differences, you first need to understand the vocabulary. Logary uses the name <span className="_code"> Message </span> to mean either an <span className="_code">Event </span> , a <span className="_code"> Gauge </span> or a <span className="_code"> Derived </span>. This comes from analysing the different sorts of things one would like to ship from an app.</p> 
        <p>Starting with an <span className="_code"> Event </span>; this is the main value when you're logging (in fact, it's Logary.PointValue.Event(template:string) that you're using.) An event is like a Gauge at a particular instant on the global timeline with a value of 1 (one).</p> 
        <p>Which brings us to what a <span className="_code"> Gauge </span> is. It's a specific value at an instant. It's what you see as a temporature on a thermometer in your apartment, e.g. <span className="_code"> 10.2 degrees celcius </span>. In the International System of Units (SI-Units), you could say it's the same as 283.2 K. Logary aims to be the foundational layer for all your metrics, so it uses these units. A <span className="_code"> Gauge </span> value of your temperature could be created like so <span className="_code"> Message.gaugeWithUnit Kelvin (Float 283.2) </span> or <span className="_code"> Gauge (Float 283.2, Kelvin) </span>.</p> 
        <p>A <span className="_code"> Derived metric </span>, like <span className="_code"> Kelvin/s </span> is useful if you're planning on writing a thermostat to control the temperature. A change in target temperature causes a rate of change.</p> 
        <p>Another sample metric could be represented by the name <span className="_code"> [| "MyApp"; "API" "requests" |] </span> and <span className="_code"> PointValue </span> of <span className="_code"> Derived (Float 144.2, Div (Scalar, Seconds)) </span>, if the API is experiencing a request rate of 144.2 requests per second.</p> 
        <p>Armed with this knowledge, we can now do a mapping between Codahale's metrics and those of Logary:</p> 
        <ul>
          <li><span className="_code"> Gauges </span> (measuring instantaneous values) -> <span className="_code"> PointValue.Gauge(.., ..) </span>.</li>
          <br></br>
          <li><span className="_code"> Timers </span> (measuring durations) -> <span className="_code"> PointValue.Gauge(.., Scaled(Seconds, 10e9) </span> (in nanoseconds)</li>
          <br></br>
          <li><span className="_code"> Meter </span> (measuring rates) -> <span className="_code"> PointValue.Derived(.., Div(Scalar, Seconds)) </span> or <span className="_code"> PointValue.Derived(.., Div(Other "requests", Seconds))</span> </li>
          <br></br>
          <li><span className="_code"> Counters </span> (counting events) -> <span className="_code"> PointValue.Event("User logged in")</span></li>
          <br></br>
          <li><span className="_code"> Histograms </span> (tracking value distributions) -> <span className="_code"> PointValue.Derived </span> (with suffixes) and <span className="_code"> Reservoirs. </span></li>
          <br></br>
        </ul>
        <p>Metrics like the above are taken from different sources:</p>
        <ul>
          <li>At call site (e.g. "<span className="_code"> Event </span> happened", or "it took <span className="_code"> 50 ns to connect </span>")</li>
          <br></br>
          <li>At a process level, derived from<span className="_code"> Gauge </span> and <span className="_code"> Event </span> from different call-sites in your app (e.g. "The 99.9th percentile of '[time] ns to connect' is 145 ns").</li>
          <br></br>
          <li>At process level, taken from the operating system (Process is using 36.3% of CPU)</li>
          <br></br>
          <li>At a system level (e.g. the CPU utilisation is 0.352% – which can be represented as</li>
          <br></br>
          <Code language="fsharp" value={
              preval`
              const fs = require('fs')
              const val = 'let mhz = Div(Scaled(Hz, 1e-6)) in Gauge(Fraction (1300, 36800), Div(mhz, mhz))'
              module.exports = val
              `
            } />
          <p>as collected by Rutta's Shipper from a compute node.</p>
        </ul>
        <p>The aim of Logary is to connect values from call-sites, to configurable derivations, such as percentiles(, potentially again to derivations), and finally to targets which can then store them.</p>
      </DocSection>
      <DocSection {...toc[3]}>
        <h2 className="section-title">Comparison with Serilog</h2>
        <p></p>
        <ul>
          <li>Both support structured logging</li>
          <li>Both run on .Net</li>
          <li>Logary is based on cooperative multithreading whilst Serilog is mostly lock-free concurrent</li>
          <li>Logary was built from running high-throughput distributed systems 24/7 in
          production and has learnt its lessons similar to Serilog.</li>
          <li>Logary can be run in multi-instance mode without using any global shared
          state (aka. statics), which is similar to Serilog</li>
          <li>Serilog's Enrichers = Logary's middleware</li>
          <li>Serilog's Sink = Logary's Target</li>
          <li>Targets in Logary tend to use IO completion ports/async-as-"green threads", AFAIK
          Sinks are running and calling out using synchronous/blocking APIs to a larger extent</li>
          <li>Logary supports flushing all targets (<code>LogManager.Flush</code>)</li>
          <li>Logary supports flushing a single target (<code>Target.flush</code>)</li>
          <li>Logary supports backpressure (F#: <code>Alt&lt;_&gt;</code> , C#: <code>Task</code>) returned from
          <code>logWithAck</code>.</li>
          <li>Logary further supports backpressure by waiting for all targets to flush a
          particular message (e.g. you should always block on Fatal messages to finish
          logging) through <code>Alt&lt;Promise&lt;unit&gt;&gt;</code>/<code>Task&lt;Task&gt;</code> in C# (same method as
          above).</li>
          <li>Logary's C# API doesn't support misconfiguring Logary, because it's been
          built with chaining types together (going beyond the <code>return this</code> pattern) –
          similar to Serilog but with a more callback-oriented API.</li>
          <li>Logary supports Metrics – Gauges, Derived values, Histograms, Reservoirs</li>
          <li>Logary supports Health checks out of the box</li>
          <li>Logary has built-in support for Windows Performance Counters metrics shipping
          through <code>Logary.Metrics.WinPerfCounters</code>.</li>
          <li>Logary provides a Facade for your libraries to depend on, to avoid dependency
          hell forcing you to upgrade all your libraries whenever Logary changes (which
          it does often, in order to improve!) – a single <code>Facade.[fs,cs]</code>-file that
          you version control yourself.</li>
          <li>Logary supports Targets that batch, out of the box, similar to Serilog. The
          Target is responsible for choosing how many <code>Messages</code> it can send at once.</li>
          <li>Logary supports Targets that fail by restarting them</li>
          <li>Logary supports Targets' last will – use to handle poison Messages</li>
          <li>Logary is written in F#, Serilog in C#</li>
          <li>Logary has a C# API <code>Logary.CSharp</code>. Serilog doesn't have a F# API</li>
          <li>Logary supports adding structured data to metrics</li>
          <li>Logary's InfluxDb target supports fields and can batch multiple Windows
          Performance Counters or metrics into a single Measurement</li>
          <li>Logary has a JS-counterpart, <em>logary-js</em> which lets you log into
          <code>Logary.Services.Rutta</code> on the server; events and metrics.</li>
          <li>Logary has paid support available if you need it.</li>
          <li>Logary supports the side-kick pattern, where you outsource your shipping of
          logs from your main process to a co-process through Rutta's Shipper and
          Router.</li>
          <li>Logary has <code>TimeScope</code> and the ability to instrument your code for sending
          timing information.</li>
          <li>Logary has preliminary support for Zipkin.</li>
          <li>Both are awesome and @nblumhardt is an awesome dude. Use whichever you feel
          most comfortable with!</li>
        </ul>
      </DocSection>

    </DocPage>
  )
}
