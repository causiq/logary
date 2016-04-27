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

        Establish context = () =>
            {
                subject = new Message(
                    PointName.NewPointName(new[] { "a", "b", "c" }),
                    PointValue.NewEvent("initial message"),
                    new Dictionary<string, object>(),
                    LogLevel.Warn,
                    SystemClock.Instance.Now.Ticks * 100L);
            };

        It should_allow_changing_template =
            () => subject.SetEvent("Hello World").ShouldEqual("Hello World");

        It should_allow_changing_ts =
            () => subject
                .SetTimestamp(Instant.FromTicksSinceUnixEpoch(4567))
                .timestamp
                .ShouldEqual(Instant.FromTicksSinceUnixEpoch(4567));
    }
}
