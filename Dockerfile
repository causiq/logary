FROM microsoft/dotnet:2.2-sdk as build

#RUN apk add --no-cache --update bash git vim zeromq protobuf protobuf-dev ca-certificates && \
#    rm -f /var/cache/apk/*

RUN apt-get update && \
    apt-get install -y git vim libczmq4 libprotobuf-c-dev libprotobuf-dev libprotobuf10 ca-certificates locales && \
    locale-gen en_US.UTF-8 && \
    dpkg-reconfigure locales

ENV LANGUAGE=en_US.UTF-8 \
    LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8

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
