FROM fsharp
RUN apt-get update && \
    apt-get install -y rake bundler
ENV LANG=C.UTF-8 \
    BF=/logary-build \
    TMPDIR=/tmp
RUN mkdir -p $BF/
WORKDIR $BF
COPY ["Gemfile*", "$BF/"]
RUN bundle
COPY [".semver", ".git", "paket.*", "Rakefile", "$BF/"]
COPY src "$BF/src/"
COPY examples "$BF/examples"
COPY tools "$BF/tools"
RUN echo "Running in $(pwd)"
RUN mono tools/paket.bootstrapper.exe
RUN mono tools/paket.exe restore
RUN ruby -pi.bak -e "gsub(/module Aether/, 'module Logary.Utils.Aether')" paket-files/xyncro/aether/src/Aether/Aether.fs && \
    ruby -pi.bak -e "gsub(/module Chiron/, 'module Logary.Utils.Chiron')" paket-files/xyncro/chiron/src/Chiron/Chiron.fs && \
    ruby -pi.bak -e "gsub(/module.* YoLo/, 'module internal Logary.YoLo')" paket-files/haf/YoLo/YoLo.fs && \
    ruby -pi.bak -e "gsub(/module Hopac/, 'namespace Logary.Internals')" paket-files/logary/RingBuffer/RingBuffer.fs && \
    ruby -pi.bak -e "gsub(/namespace Logary.Facade/, 'namespace Libryy.Logging')" paket-files/logary/logary/src/Logary.Facade/Facade.fs
RUN xbuild /property:Configuration=Release src/Logary.sln
CMD mono src/tests/Logary.Tests/bin/Release/Logary.Tests.exe --sequenced
