FROM microsoft/dotnet:2.2-sdk as build

#RUN apk add --no-cache --update bash git vim zeromq protobuf protobuf-dev ca-certificates && \
#    rm -f /var/cache/apk/*

RUN DEBIAN_FRONTEND=noninteractive apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y locales && \
    locale && \
    locale -a && \
    locale-gen C C.UTF-8 && \
    DEBIAN_FRONTEND=noninteractive dpkg-reconfigure locales && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y git vim libczmq4 libprotobuf-c-dev libprotobuf-dev libprotobuf10 ca-certificates

ENV LC_ALL=C.UTF-8

WORKDIR /build/
COPY . .

RUN ./fake.sh build

#-alpine
FROM microsoft/dotnet:2.2-runtime

RUN apk add --no-cache --update zeromq protobuf protobuf-dev ca-certificates && \
    rm -f /var/cache/apk/*

WORKDIR /logary/
COPY --from=build /build/src/services/Logary.Services.Rutta/bin/Release/netcoreapp2.2/publish/* /logary/

ENTRYPOINT ["dotnet", "/logary/rutta.dll"]
CMD []
