# --------
FROM mcr.microsoft.com/dotnet/core/sdk:5.0-alpine as builder

WORKDIR /build/

ENV LC_ALL=C.UTF-8

RUN apk add --no-cache --update alpine-sdk zeromq protobuf protobuf-dev ca-certificates && \
    curl --silent -L -o /etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub && \
    curl --silent -LO https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.31-r0/glibc-2.31-r0.apk && \
    apk add glibc-2.31-r0.apk && \
    rm -f /var/cache/apk/*

# Required to do `dotnet restore`
COPY .config ./.config
COPY Makefile paket.dependencies paket.lock ./

# Tried to optimise restore, but like all things MSFT it's not built to hack
#COPY .tmp .

# First restore 'paket', then restore the packages/ folder
RUN dotnet tool restore && \
    dotnet paket restore

# Tried to optimise restore, but like all things MSFT it's not built to hack
#&& \
#dotnet restore -r linux-musl-x64 src/services/Logary.Services.Rutta/Logary.Services.Rutta.fsproj

COPY . .

RUN make version_files && \
    dotnet publish -c release -r linux-musl-x64 src/services/Logary.Services.Rutta -o /app

# Now, copy all targets to Logary Rutta, so they are available
RUN find 'src/targets' -type f \
    \( -name 'Logary.Targets.*.dll' \
       -or -name 'Logary.Targets.*.pdb' \
       -or -name 'Logary.Targets.*.xml' \) \
    -not -path '*/obj/*' \
    -exec cp {} /app/ \;

# -------
FROM mcr.microsoft.com/dotnet/core/runtime:3.1-alpine3.11 as runner

WORKDIR /app

ENV USER=logary
ENV UID=12345
ENV GID=23456

# https://github.com/grpc/grpc/issues/21446
# curl --silent -L -o /etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub && \
# curl --silent -LO https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.31-r0/glibc-2.31-r0.apk && \
# apk add glibc-2.31-r0.apk && \

RUN apk update --no-cache && \
    apk add libc6-compat zeromq protobuf protobuf-dev ca-certificates && \
    addgroup -S "$USER" && \
    adduser \
      --disabled-password \
      --gecos "" \
      --home "$(pwd)" \
      --ingroup "$USER" \
      --no-create-home \
      --uid "$UID" \
      "$USER" && \
    rm -f /var/cache/apk/*

COPY --from=builder /app .

ENTRYPOINT ["dotnet", "/app/rutta.dll"]

CMD []
