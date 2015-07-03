FROM gliderlabs/alpine:edge
MAINTAINER Henrik Feldt <henrik@haf.se>

ENV APP_PREFIX /app
ENV LISTEN_PORT=8081

RUN echo "## prepare build environment" && \
    apk update && \
    apk upgrade -U && \
    apk add --update ca-certificates && \
    apk add mono \
        --update-cache \
        --repository http://dl-3.alpinelinux.org/alpine/edge/testing/ \
        --allow-untrusted && \
    rm -rf /tmp/* /var/cache/apk/*

RUN mozroots --import --sync --machine

COPY bin $APP_PREFIX/bin
WORKDIR $APP_PREFIX/bin
ENTRYPOINT /usr/bin/env mono $APP_PREFIX/bin/heka.exe