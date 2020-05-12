.PHONY: image push

TAG_VERSION_SUFFIX := $(shell ../../../tools/version.sh)

image:
	docker build -t logary/rutta:latest -t logary/rutta:$(TAG_VERSION_SUFFIX) .
	docker build -t logary/rutta-curl:latest -t logary/rutta-curl:$(TAG_VERSION_SUFFIX) ../rutta-helm-chart/rutta-curl

push:
	docker push logary/rutta:latest
	docker push logary/rutta:$(TAG_VERSION_SUFFIX)
	docker push logary/rutta-curl:latest
	docker push logary/rutta-curl:$(TAG_VERSION_SUFFIX)