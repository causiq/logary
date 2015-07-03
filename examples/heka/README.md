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

## On OS X?

Forward docker port, start CoreOS, map with:

```
echo "$shared_folders = { '/Users/you/dev' => '/home/core/shared' }" >>config.rb
vagrant reload core-01
```

Now run the container and map the folders:

```
docker run --rm -it \
  --name heka \
  -p 5565:5565 \
  -v /home/core/shared/logary/logary/examples/heka/heka.d:/etc/heka.d \
  -v /home/core/shared/logary/logary/examples/heka/data:/var/log/heka \
  haaf/heka:latest
```

Now call heka.exe:

```
mono bin/heka.exe
```