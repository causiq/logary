FROM fsharp:4.1
WORKDIR /app

RUN apt-get update -y && \
    apt-get install -y libzmq3 libzmq3-dev && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get clean

ADD ./bin/Release/net461/* /app/

ENTRYPOINT ["mono", "/app/rutta.exe"]
CMD []
