FROM microsoft/dotnet:2.2-sdk as build #-alpine as build

#RUN apk add --no-cache --update bash git vim zeromq protobuf protobuf-dev ca-certificates && \
#    rm -f /var/cache/apk/*
RUN apt-get install -y

WORKDIR /build/
COPY . .

RUN ./fake.sh build

FROM microsoft/dotnet:2.2-runtime #-alpine

RUN apk add --no-cache --update zeromq protobuf protobuf-dev ca-certificates && \
    rm -f /var/cache/apk/*

WORKDIR /logary/
COPY --from=build /build/src/services/Logary.Services.Rutta/bin/Release/netcoreapp2.2/publish/* /logary/

ENTRYPOINT ["dotnet", "/logary/rutta.dll"]
CMD []
