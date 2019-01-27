#-alpine
FROM microsoft/dotnet:2.2-sdk as build

#RUN apk add --no-cache --update bash git vim zeromq protobuf protobuf-dev ca-certificates && \
#    rm -f /var/cache/apk/*

RUN DEBIAN_FRONTEND=noninteractive apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y locales && \
    locale-gen C C.UTF-8 && \
    DEBIAN_FRONTEND=noninteractive dpkg-reconfigure locales && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y git vim libczmq4 libprotobuf-c-dev libprotobuf-dev libprotobuf10 ca-certificates

ENV LC_ALL=C.UTF-8

WORKDIR /build/
COPY paket.dependencies paket.lock ./

RUN dotnet tool install --tool-path $PWD/.paket Paket
RUN .paket/paket restore

COPY . .
RUN dotnet restore src/Logary.sln
RUN ./fake.sh build

#-alpine
FROM microsoft/dotnet:2.2-runtime

#RUN apk add --no-cache --update zeromq protobuf protobuf-dev ca-certificates && \
#    rm -f /var/cache/apk/*
RUN DEBIAN_FRONTEND=noninteractive apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y locales && \
    locale-gen C C.UTF-8 && \
    DEBIAN_FRONTEND=noninteractive dpkg-reconfigure locales && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y libczmq4 libprotobuf-c-dev libprotobuf10 ca-certificates

WORKDIR /logary/
COPY --from=build /build/src/services/Logary.Services.Rutta/bin/Release/netcoreapp2.2/publish/* /logary/

ENTRYPOINT ["dotnet", "/logary/rutta.dll"]
CMD []
