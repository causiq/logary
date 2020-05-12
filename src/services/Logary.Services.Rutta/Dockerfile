FROM mcr.microsoft.com/dotnet/core/sdk:3.1-alpine as builder

RUN apk add --no-cache --update bash git vim zeromq protobuf protobuf-dev ca-certificates && \
    rm -f /var/cache/apk/*

ENV LC_ALL=C.UTF-8

WORKDIR /build/
COPY paket.dependencies paket.lock ./

RUN dotnet tool install --tool-path $PWD/.paket Paket
RUN .paket/paket restore

COPY . .

RUN dotnet restore src/Logary.sln
RUN dotnet publish -c Release -r linux-x64 src/services/Logary.Services.Rutta

RUN find 'src/targets' -type f \
    \( -name 'Logary.Targets.*.dll' \
       -or -name 'Logary.Targets.*.pdb' \
       -or -name 'Logary.Targets.*.xml' \) \
    -not -path '*/obj/*' \
    -exec cp {} src/services/Logary.Services.Rutta/bin/Release/netcoreapp3.1/linux-x64/publish/ \;

FROM alpine:3.9.6 as runner

RUN apk add --no-cache --update zeromq protobuf protobuf-dev ca-certificates && \
    rm -f /var/cache/apk/*

USER logary

WORKDIR /logary/
COPY --from=builder /build/src/services/Logary.Services.Rutta/bin/Release/netcoreapp3.1/linux-x64/publish/* /logary/

ENTRYPOINT ["dotnet", "/logary/rutta.dll"]
CMD []