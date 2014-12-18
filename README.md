# Logary v2.1

Logary is a high performance, multi-target logging, metric, tracing and
health-check library for mono and .Net.

Follow Logary at twitter: [@logarylib](https://twitter.com/logarylib)

Chat and support and get support:
[![Gitter chat](https://badges.gitter.im/logary.png)](https://gitter.im/logary/logary)

If you like the code, buy me a beer!
[![Flattr this](https://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=haf&url=https%3A%2F%2Fgithub.com%2Flogary%2Flogary)

Logary v2.1 aims to be compatible with the latest Mono and .Net 4.5. It is
compiled with open source F# 3.0. [Logary is continously built on
CentOS](https://tc-oss.intelliplan.net/project.html?projectId=Logary&tab=projectOverview).

``` powershell
Install-Package Intelliplan.Logary
```

At [https://logary.github.io/](https://logary.github.io) you can find the full documentation.

## Why?

Logary is the next generation logging framework. It observes some facts that it
successfully builds its conceptual model from! It's written using functional
programming in F# with only a single field 'global state' to facilitate logging
with initialise-once static readonly fields. It never throws runtime exceptions
if the configuration validates and never blocks the call-site.


## Target Maintainers Wanted!

Are you interested in maintaining a target? Let [me know](mailto:henrik@haf.se)
or file a PR demonstrating your work.

## Building

Assuming you have Ruby 1.9.3 or later installed:

``` bash
git clone git://github.com/logary/logary.git
cd logary
git submodule update --init
bundle && bundle exec rake
```

## License

[Apache 2.0][apache]

 [apache]: https://www.apache.org/licenses/LICENSE-2.0.html
