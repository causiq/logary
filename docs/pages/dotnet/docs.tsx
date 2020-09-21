import DocPage from '../../components/DocPage'
import DocSection from '../../components/DocSection'
import Code from '../../components/Code'
import stn from '../../public/images/SinkTargetNames.png'
import { DotNet } from '../../components/samples'

const Sample = ({ fileName, language = 'fsharp' }: { fileName: string, language?: 'fsharp' | 'text' }) =>
  <Code value={DotNet[fileName]} language={language} />

export default function DotnetDocs() {
  return (
    <DocPage
      name=".net-documentation"
      title=".Net Core Documentation"
      colour="purple">

      <DocSection title='An example' id='an-example'>
        <p>Suppose you're measuring values coming from a car. This is what that could look like:</p>
        <Sample fileName='Car.fs' />
      </DocSection>

      <DocSection title='Passing values around' id='passing-values-around'>
        <p>
          Context is generally classified into these categories: (you can try these code on test.fsx in Logary.Tests)
        </p>

        <h3>Fields</h3>
        <p>
          prefix with "_fields."
        </p>
        <p>
          Fields are the structured data when you use structure logging like (https://messagetemplates.org/), there are mainly two style to achieve this.
        </p>
        <Sample fileName='MessageWriter.fs' />

        <p>Results in:</p>
        <Sample fileName='MessageWriter.output.txt' language='text' />

        <h3>Gauges</h3>
        <p>prefix with "_logary.gauge."</p>

        <p>which value is Gauge(float, units). An instantaneous value. Imagine the needle showing the speed your car is going or a digital display showing the same instantaneous metric value of your car's speed.</p>

        <p>you can add gauges with one message, or use gauge as the message. The difference between them is, if you use gauges as the message, the value in message are auto generate by gauges when formatting them :</p>
        <Sample fileName='Doc4.fs' />

        <h3>Tags</h3>
        <p>prefix with "_logary.tags"</p>

        <p>which value is a set , tags are help with identity one type message when you do some pipeline processing.</p>
        <Sample fileName='Doc5.fs' />

        <h3>SinkTargetNames</h3>
        <p>prefix with "_logary.sink.targets"</p>

        <p>They are generally are set by Events Processing, you can define which targets (sinks) your message will go. if not set, message will go to all targets and let the targets themself to decide whether or not to accept it.</p>
        <Sample fileName='Doc6.fs' />

        <img src={stn} alt="SinkTargetNames" />
        <p>this will only show on LiterateConsole, not normal Console.</p>

        <h3>SinkTargetNames</h3>
        <p>things you don't want to show on the message value, but show on the backstore. e.g: some structured data not belong the message template or data you can use in the EventProcessing Pipeline.</p>
        <Sample fileName='Doc7.fs' />
      </DocSection>

      <DocSection title='Filtering logs' id='filtering-logs'>
        <p>A logger have a minimum level which message's level below it is not processed when logging these message. Can give us Low overhead logging – evaluate your Message only if a level is switched on. Especially when you use logging api with message factory.</p>
        <p>A logger's minimum level are config through Config.loggerMinLevel "a.b.*" LogLevel.Fatal on logary conf (usually globally) use a specific name or some hierarchy path. And can be switch on fly logm.switchLoggerLevel ("a.b.*", LogLevel.Info),this will only affect the loggers (its name, not its instance) which have been created beafore. e.g. the default level is Error on prod, use a pipe line detect an error message, switch to Info for 5 mins then change it back. can be use for auto collecting more useful info when things goes wrong.</p>
        <Sample fileName='Doc8.fs' />
      </DocSection>

      <DocSection title='Log levels' id='log-levels'>
        <p>The highest log level is Fatal, which should be reserved for things that make your service/process crash. Things like; "my disk is full and I'm a database trying to start", or "I'm a 2-tier service built with a database, that I cannot do any work without" warrant the Fatal level.</p>
        <p>The next level is Error, which should be reserved for what you consider to be edge-cases. E.g. if the data received from a socket is corrupt, or there was an unhandled exception that you as a programmer did not have in your mental model while writing the code. These events should be logged at the Error level.</p>
        <p>At this level human beings are normally directly alerted.</p>
        <p>Warn is for things like 100 failed password attempts within 5 minutes, for one of your users, or a temporary network glitch while communicating with a "resource" such as your database.</p>
        <p>If these events for an anomaly persist over time, humans may be alerted.</p>
        <p>At Info level, I like to put events and gauges that measure company-relevant stuff, like when users sign in, sign up, an integration has to perform a retry or a service was started/restarted.</p>
        <p>Debug level is the default level and the work-horse. You normally log all metrics at this level.</p>
        <p>Verbose is the level when you want that little extra. Not normally enabled.</p>
      </DocSection>

      <DocSection title='Logging from modules' id='logging-from-modules'>
        <p>Let's say you have a module in your F# code that you want to log from. You can either get a logger like shown in Hello World, or you can do something like this:</p>
        <Sample fileName='Doc9.fs' />
        <p>If you want to name your logger with a specific name, you can use Logging.getLoggerByName instead. (This is different for the Facade file)</p>
      </DocSection>

      <DocSection title='Logging from classes' id='logging-from-classes'>
        <p>Similarly, sometimes you want to log from a class, and perhaps log some metrics too.</p>
        <Sample fileName='Doc10.fs' />

        <p>In this example you learnt how to send arbitrary metrics to Logary (the gauge) and also how to time how long certain method calls take in your system.</p>
        <p>Make it a habit to log these sort of gauges all over your code base while you write your code, to make it much easier to understand the system as it develops.</p>
        <p>In fact, the more you do this, the more use you will have of Logary and of the dashboard you put up in Kibana (via Logstash) or Grafana (via InfluxDb). Put it up on a big TV in your office and you'll develop a second sense of whether the system is doing well or not, just from looking at the graphs.</p>
      </DocSection>

      <DocSection title='Structured logging and interpolation' id='structured-logging-interpolation'>
        <p>The templates syntax can be found here: <a href="https://messagetemplates.org/#syntax"></a></p>
        <p>Message Templates are a superset of standard .NET format strings, so any format string acceptable to string.Format() will also be correctly processed by logary.</p>
        <ul>
          <li>
            Property names are written between and braces
          </li>
          <li>Formats that use numeric property names, like 0 and 1 exclusively, will be matched with the Format method's parameters by treating the property names as indexes; this is identical to string.Format()'s behaviour </li>
          <li>If any of the property names are non-numeric, then all property names will be matched from left-to-right with the Format method's parameters</li>
          <li>Property names may be prefixed with an optional operator, @ or $, to control how the property is serialised</li>
          <li>Property names may be suffixed with an optional format, e.g. :000, to control how the property is rendered; these format strings behave exactly as their counterparts within the string.Format() syntax</li>
        </ul>
      </DocSection>

      <DocSection title='Per-process event processing' id='per-process-event-processing'>
        <p>Sometimes you need a metric that runs continuously over time. A Ticker can be seems as a metric, it can be auto triggered or by manually. A ticker can be chained in an pipe line (EventProcessing).</p>
        <p>We have some windows performance counter metrics that you can use.</p>
        <p>But you sometimes want to chain metrics from events or gauges happening inside your own application.</p>
        <p>We have some windows performance counter metrics that you can use.</p>
        <Sample fileName="Doc11.fs" />
        <p>By wrapping it up like this, you can drastically reduce the amount of code a given service sends by pre-computing much of it.</p>
        <p>It's also a good sample of reservoir usage; a fancy name of saying that it's an algorithm that works on more than one gauge at a time, to produce a derived metric.</p>
      </DocSection>
    </DocPage>
  )
}