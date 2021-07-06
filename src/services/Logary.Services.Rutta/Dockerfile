# --------
FROM mcr.microsoft.com/dotnet/sdk:5.0 as builder

WORKDIR /build/

ENV LC_ALL=C.UTF-8

RUN apt-get update

RUN apt-get install -y --no-install-recommends \
    git \
    build-essential \
    libtool \
    autoconf \
    automake \
    pkg-config \
    unzip \
    libkrb5-dev \
    uuid-dev

# Required to do `dotnet restore`
COPY .config ./.config
COPY Makefile paket.dependencies paket.lock ./

# Tried to optimise restore, but like all things MSFT it's not built to hack
#COPY .tmp .

# First restore 'paket', then restore the packages/ folder
RUN dotnet tool restore && \
    dotnet paket restore

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
FROM mcr.microsoft.com/dotnet/runtime:5.0 as runner

WORKDIR /app

ENV USER=logary
ENV UID=12345
ENV GID=23456

RUN useradd --user-group --system --create-home --home-dir /app $USER && \
    chown -R $USER:$USER /app

# https://packages.debian.org/search?searchon=sourcenames&keywords=zeromq
RUN apt-get update && \
    apt-get install -y libprotobuf-dev libprotoc17 libprotobuf17 protobuf-compiler ca-certificates libzmq3-dev && \
    rm -rf /var/lib/apt/lists/*

USER $USER

COPY --from=builder /app .

ENTRYPOINT ["dotnet", "/app/rutta.dll"]

CMD []
