using System.Collections.Generic;
using Machine.Specifications;
using NodaTime;

// ReSharper disable InconsistentNaming
// ReSharper disable UnusedMember.Local
// ReSharper disable FieldCanBeMadeReadOnly.Local
// ReSharper disable UnusedMember.Global

namespace Logary.Specs
{
    public class When_changing_properties_on_Message
    {
        static Message subject;
        static string template;

        Establish context = () =>
            {
                subject = new Message(
                    PointName.NewPointName(new[] { "a", "b", "c" }),
                    PointValue.NewEvent("initial message"),
                    Microsoft.FSharp.Collections.MapModule.Empty<PointName, Field>(),
                    Microsoft.FSharp.Collections.MapModule.Empty<string, Value>(),
                    LogLevel.Warn,
                    SystemClock.Instance.Now.Ticks * 100L);
                subject.SetEvent("Hello World").value.TryGetEvent(out template).ShouldBeTrue();
            };

        It should_allow_changing_template =
            () => template.ShouldEqual("Hello World");

        It should_allow_changing_ts =
            () => subject
                .SetTimestamp(Instant.FromTicksSinceUnixEpoch(4567))
                .GetTimestamp()
                .ShouldEqual(Instant.FromTicksSinceUnixEpoch(4567));
    }
}
