# Heka with Docker Compose

This example shows how you can set up a monitoring infrastructure in 3 minutes
with docker.

We will do that, then use Logary to send metrics and loglines into this
infrastructure. You'll need docker and docker Compose installed to run this
example. (see below)

```
./prepare.sh
docker-compose up
```

## Where's haaf/heka?

[haf/docker-heka](https://github.com/haf/docker-heka/blob/master/Dockerfile)

## Don't have Docker Compose?

See [installation instructions](https://docs.docker.com/compose/)

## On Windows?

Too bad.