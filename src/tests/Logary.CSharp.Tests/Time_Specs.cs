// ReSharper disable InconsistentNaming
// ReSharper disable UnusedMember.Local
// ReSharper disable FieldCanBeMadeReadOnly.Local
// ReSharper disable UnusedMember.Global

namespace Logary.Specs
{
  using System.Text;
  using System.IO;

  using Configuration;
  using Machine.Specifications;
  using NodaTime;
  using TextWriter = Logary.Targets.TextWriter;

  public class When_using_Time_CSharp_methods
  {
    Establish context = () =>
    {
      writer = new StringWriter(new StringBuilder());
      manager = LogaryFactory.New(
        "Logary Specs",
        with => with.Target<TextWriter.Builder>(
          "sample string writer",
          t => t.Target.WriteTo(writer, writer)))
        .Result;
    };

    Cleanup cleanup = () =>
    {
      manager.Dispose();
      writer.Dispose();
    };

    private Because reason = () =>
      {
        var logger = manager.GetLogger("Timing Sleep(100)");

        logger.Time(() => System.Threading.Thread.Sleep(100));

        manager.FlushPending(Duration.FromSeconds(8L)).Wait();
        subject = writer.ToString();
      };

    It output_should_contain_gauge = () => subject.ShouldContain("gauge");

    static LogManager manager;
    static StringWriter writer;
    static string subject;
  }
}
