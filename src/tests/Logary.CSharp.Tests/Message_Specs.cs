using Machine.Specifications;
using Microsoft.FSharp.Collections;
using NodaTime;

// ReSharper disable InconsistentNaming
// ReSharper disable UnusedMember.Local
// ReSharper disable FieldCanBeMadeReadOnly.Local
// ReSharper disable UnusedMember.Global

namespace Logary.CSharp.Tests
{
    public class When_changing_properties_on_Message
    {
        static Message subject;
        static string template;

        Establish context = () =>
            {
                subject = new Message(
                    PointName.NewPointName(new[] {"a", "b", "c"}),
                    PointValue.NewEvent("initial message"),
                    HashMap.empty<string, object>(),
                    LogLevel.Warn,
                    SystemClock.Instance.GetCurrentInstant().ToUnixTimeTicks()*100L);
                template = subject.SetEvent("Hello World").value.template;
            };

        It should_allow_changing_template =
            () => template.ShouldEqual("Hello World");

        It should_allow_changing_ts =
            () => subject
                .SetTimestamp(NodaTime.Instant.FromUnixTimeTicks(4567))
                .GetTimestamp()
                .ShouldEqual(NodaTime.Instant.FromUnixTimeTicks(4567));
    }
}