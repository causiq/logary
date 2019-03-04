#-alpine
FROM microsoft/dotnet:2.2-sdk as build

#RUN apk add --no-cache --update bash git vim zeromq protobuf protobuf-dev ca-certificates && \
#    rm -f /var/cache/apk/*

RUN DEBIAN_FRONTEND=noninteractive apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y locales && \
    locale-gen C C.UTF-8 && \
    DEBIAN_FRONTEND=noninteractive dpkg-reconfigure locales && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y git vim libzmq3-dev libczmq4 libprotobuf-c-dev libprotobuf-dev libprotobuf10 ca-certificates && \
    ldconfig

ENV LC_ALL=C.UTF-8

WORKDIR /build/
COPY paket.dependencies paket.lock ./

RUN dotnet tool install --tool-path $PWD/.paket Paket
RUN .paket/paket restore

COPY . .
RUN dotnet restore src/Logary.sln
RUN ./fake.sh build
RUN dotnet publish -c Release -r linux-x64 src/services/Logary.Services.Rutta
RUN find 'src/targets' -type f \
    \( -name 'Logary.Targets.*.dll' \
       -or -name 'Logary.Targets.*.pdb' \
       -or -name 'Logary.Targets.*.xml' \) \
    -not -path '*/obj/*' \
    -exec cp {} src/services/Logary.Services.Rutta/bin/Release/netcoreapp2.2/linux-x64/publish/ \;

#-alpine
FROM microsoft/dotnet:2.2-runtime

#RUN apk add --no-cache --update zeromq protobuf protobuf-dev ca-certificates && \
#    rm -f /var/cache/apk/*
RUN DEBIAN_FRONTEND=noninteractive apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y locales && \
    locale-gen C C.UTF-8 && \
    DEBIAN_FRONTEND=noninteractive dpkg-reconfigure locales && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y libzmq3-dev libczmq4 libprotobuf-c-dev libprotobuf10 ca-certificates && \
    ldconfig

WORKDIR /logary/
COPY --from=build /build/src/services/Logary.Services.Rutta/bin/Release/netcoreapp2.2/linux-x64/publish/* /logary/

ENTRYPOINT ["dotnet", "/logary/rutta.dll"]
CMD []
