using System.IO;
using Machine.Specifications;
using NodaTime;

// ReSharper disable InconsistentNaming
// ReSharper disable UnusedMember.Local
// ReSharper disable FieldCanBeMadeReadOnly.Local
// ReSharper disable UnusedMember.Global

namespace Logary.CSharp.Tests
{
    public class When_using_TimeScope
    {
        Establish context_setting_up_logging =
            () => manager = LogaryTestFactory.GetManager(out output);

        Because using_TimeScope = () =>
        {
            using (subject.TimeScope("Running op"))
            {
                using (subject.TimeScope("Inner"))
                {
                }
            }

            manager.FlushPending(Duration.FromSeconds(8L)).Wait();

            template = output.ToString();
        };

        static string template;

        It should_contain_Running_op_message = () => template.ShouldContain("Running op");
        It should_contain_Inner_message = () => template.ShouldContain("Inner");

        Cleanup afterwards = () =>{
        //  manager.DisposeAsync().ToTask().Wait();
        };

        static Logger subject = Log.Create<When_using_TimeScope>();
        static LogManager manager;
        static StringWriter output;
    }
}