.PHONY: dotnet_build image push

TAG_VERSION_SUFFIX := $(shell ../../../tools/version.sh)

image:
	(cd ../../.. && \
  docker build -t logary/rutta:latest -t logary/rutta:$(TAG_VERSION_SUFFIX) \
    -f src/services/Logary.Services.Rutta/Dockerfile \
    .)

push:
	docker push logary/rutta:latest
	docker push logary/rutta:$(TAG_VERSION_SUFFIX)
	docker push logary/rutta-curl:latest
	docker push logary/rutta-curl:$(TAG_VERSION_SUFFIX)