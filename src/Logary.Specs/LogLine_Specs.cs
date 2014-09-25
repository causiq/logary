using System.Collections.Generic;
using Machine.Specifications;
using NodaTime;

// ReSharper disable InconsistentNaming
// ReSharper disable UnusedMember.Local
// ReSharper disable FieldCanBeMadeReadOnly.Local
// ReSharper disable UnusedMember.Global

namespace Logary.Specs
{
    public class When_changing_properties_on_LogLine
    {
        static LogLine subject;

        Establish context = () =>
            {
                subject = LogLineModule.Create(
                    "initial message",
                    new Dictionary<string, object>(), LogLevel.Warn,
                    new[] { "another tag" }, "a.b.c", null, Instant.FromTicksSinceUnixEpoch(1234));
            };

        It should_allow_changing_message =
            () => subject.SetMessage("Hello World").message.ShouldEqual("Hello World");
        It should_allow_changing_ts =
            () => subject
                .SetTimestamp(Instant.FromTicksSinceUnixEpoch(4567))
                .timestamp
                .ShouldEqual(Instant.FromTicksSinceUnixEpoch(4567));
    }
}
