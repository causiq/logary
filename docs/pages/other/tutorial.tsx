import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import Code from '../../components/Code'

export default function Tutorials() {
  return (
    <DocPage name="js-quickstart" title="Tutorials" colour="primary">

      Clone it like above. Ensure you can build it. Open <span className="_code"> Logary.sln </span>. Make a change, send a PR towards master. To balance the app.config files, try <span className="_code"> mono tools/paket.exe install --redirects --clean-redirects --createnewbindingfiles</span>

      <DocSection title='File guidelines – module vs static method' id='file-guidelines'>
        <p>Declare your interfaces in a <span className="_code"> MyIf.fs </span> and its module in <span className="_code"> MyIfModule.fs </span> with a ([CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)])).</p>
        <p>Place these files as high as possible. Place the module as close to and below the non-module file as possible.</p>
        <p>If it's plausible that one would like to use point-free programming with your functions, place them in a module. (E.g. Message versus MessageModule)</p>
        <p>Factory methods go on the types for value types. For stateful objects they go on the module, named <span className="_code"> create </span> which may or may not return a <span className="_code"> Job[] </span> of some public state type with a private/internal constructor. (E.g. PointName versus Engine)</p>
      </DocSection>

      <DocSection title='File guidelines – plural vs singular' id='file-guidelines-plural'>
        <p>All implementations go in a plural namespace. E.g. <span className="_code"> Logary.Metrics.Ticked </span> has:</p>
        <ul>
          <li><span className="_code">Logary</span> – core namespace</li>
          <li><span className="_code">Metrics</span> – namespace for all metrics implementations</li>
          <li><span className="_code">Ticked</span> – a module and/or type that implements Logary-based ticking/ scheduling.</li>
        </ul>
        <p>Another example: <span className="_code"> Logary.Target </span>, which is a module that implements logic about the life-cycle of target instances. It's used to start/stop/pause and shutdown targets. It's singular.</p>
      </DocSection>

      <DocSection title='Namespace guidelines' id='namespaces'>
        <p>– <span className="_code">Logary.Internals</span> or not</p>
        <p>Things that end-users of the library are not likely to configure should go in <span className="_code">Logary.Internals</span> . Examples include <span className="_code"> Logary.Internals.Globals </span> and <span className="_code"> Logary.Internals.RuntimeInfo </span> (which is configured with the config API instead).</p>
      </DocSection>

      <DocSection title='RuntimeInfo &amp; internal logging' id='ri-and-internal-logging'>
        <p>The <span className="_code"> RuntimeInfo </span> and internal <span className="_code"> Logger </span> should be propagated to the modules and objects that you build your solution out of. This guideline is mostly about the registry's implementation and its interaction with config and services.</p>
      </DocSection>

      <DocSection title='A new function?' id='functions'>
        <p>Generally, keep your functions to a single responsibility and compose functions instead of extending existing functions. Composition happens through currying and partial application of 'state' or 'always-this-value' values. For state it can both go through the Services abstraction (start/pause/stop/etc) or by closing over the state with a function.</p>
        <p>If you find that your functions are getting larger than 3 lines of code, you should probably extract part of the function. By 'default' it's better to be 1% less performant but 5% more readable, after this list of priorities:</p>
        <ol type="1">
          <li>Correct</li>
          <li>CReadable/SRPorrect</li>
          <li>Fast/efficient/performant</li>
        </ol>
      </DocSection>

      <DocSection title='Opening namespaces' id='opening-namespaces'>
        <p>Prefer to open a namespace over fully qualifying a type.</p>
        <p>Prefer to open fully qualified over partial.</p>
        <Code language="fsharp" value="open Logary.Internals open Logary.Internals.Supervisor" />
        <p>Instead of</p>
        <Code language="fsharp" value="open Logary.Internals open Supervisor" />
      </DocSection>

      <DocSection title='Starting Hopac Jobs' id='hopac-jobs'>
        <p>A module function like <span className="_code"> MyModule.create: Conf -&gt; Job[T] </span> should not start the server loop. Instead, just return a cold job (that can be started multiple time) and let the composition "root", such as the <span className="_code"> Registry </span>, perform the composition and lifetime handling.</p>
      </DocSection>

      <DocSection title='Writing a new target' id='new-target'>
        <p>Are you thinking of creating a new Target for Logary? It's a good idea if you can't find the right Target for your use case. It can also be useful if you have an internal metrics or log message engine in your company you wish to ship to.</p>
        <ol type="1">
          <li>Create a new .net 4.5.1 class library in F#, under target and add that to Logary.sln.</li>
          <li>Copy the code from Logary's Targets/Noop.fs, which contains the basic structure. There are more docs in this file, to a file named MyTarget.fs in your new project.</li>
          <li>Add a nuget reference (or project reference if you're intending to send a PR) to Logary</li>
          <li>Write your Target and your Target's tests to ensure that it works</li>
        </ol>
        <ul>
          <li>Remember to test when the call to your server throws exceptions or fails</li>
          <li>You should use <a href="https://github.com/haf/Http.fs"> Http.fs </a> as the HTTP client if it's a HTTP target</li>
        </ul>

        <h5>Target guidelines</h5>
        <p>When writing the Target, it's useful to keep these guidelines in mind.</p>
        <ul>
          <li>It should be able to handle shutdown messages from the shutdown channel</li>
          <li>It should not handle 'unexpected' exceptions, like network loss or a full
          disk by itself, but instead crash with an exception – the Logary supervisor
            will restart it after a short duration.</li>
          <li>Things that are part of the target API, like different response status codes
            of a REST API should be handled inside the Target.</li>
          <li>Don't do blocking calls;
            <ul>
              <li>Convert <code>Task&lt;_&gt;</code> and <code>Async&lt;_&gt;</code> to <code>Job&lt;_&gt;</code> by using the Hopac
            <a href="https://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.fromAsync" rel="nofollow">conversion methods</a></li>
              <li>If you need to block, use <a href="https://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Job.Scheduler.isolate" rel="nofollow">Scheduler.isolate</a> so that your
            blocking call doesn't stop all Targets.</li>
            </ul>
          </li>
          <li>Choose whether to create a target that can re-send crashing messages by
            choosing between <code>TargetUtils.[willAwareNamedTarget, stdNamedTarget]</code></li>
          <li>You can choose between consuming Messages one-by-one through
            <a href="https://github.com/logary/RingBuffer"><code>RingBuffer.take</code></a> or in batches with <code>RingBuffer.takeBatch</code></li>
          <li>If you take a batch and the network call to send it off fails, consider
            sending the batch to the <code>willChannel</code> and throw an exception. Your target
            will be re-instantiated with the batch and you can now send the messages
            one-by-one to your target, throwing away poison messages (things that always
            crash).</li>
          <li>If your target throws an exception, the batch of Messages or the Message
            you've taken from the <code>RingBuffer</code> will be gone, unless you send it to the
            <em>will</em> channel.</li>
          <li>Exiting the loop will cause your Target to shut down. So don't catch
            <em>all</em> exceptions without recursing afterwards. The supervisor does <em>not</em>
            restart targets that exit on their own.</li>
          <li>If your target can understand a service name, then you should always add the
            service name from <code>RuntimeInfo.serviceName</code> as passed to your loop function.</li>
          <li>The <code>RuntimeInfo</code> contains a simple internal logger that you can assume
            always will accept your Messages. It allows you to debug and log exceptions
            from your target. By default it will write output to the STDOUT stream.</li>
          <li>If you don't implement the last-will functionality, a caller that awaits the
            Promise in <code>Alt&lt;Promise&lt;unit&gt;&gt;</code> as returned from <code>logWithAck</code>, will block
            forever if your target ever crashes.</li>
          <li>If you need to do JSON serialisation, consider using <code>Logary.Utils.Chiron</code>
            and <code>Logary.Utils.Aether</code>, which are vendored copies of
            <a href="https://github.com/xyncro/chiron">Chiron</a> and <a href="https://github.com/xyncro/aether">Aether</a>. Have a look at the
            <a href="https://github.com/logary/logary/blob/master/src/targets/Logary.Targets.Logstash/Targets_Logstash.fs">Logstash Target</a> for an example.</li>
        </ul>
        <h5>Publishing your target</h5>
        <p>When your Target is finished, either ping <a href="https://github.com/haf">@haf</a> on
          github, <a href="https://twitter.com/henrikfeldt" rel="nofollow">@henrikfeldt</a> on twitter, or send a PR
          to this README with your implementation documented. I can assist in
          proof-reading your code, to ensure that it follows the empirical lessons learnt
          operating huge systems with Logary.</p>
      </DocSection>
    </DocPage>
  )
}