.PHONY: prepare_restore build_image image test_kustomize deploy_dev deploy_as_daemonset deploy_as_deployment push release

TAG_VERSION_SUFFIX := $(shell ../../../tools/version.sh)
COMMIT_SHA := $(shell git rev-parse HEAD)
COMMIT_SHA_SHORT := $(shell git rev-parse --short HEAD)

image: build_image

# Copies all *(fs|cs)proj-files to .tmp/ so they can be restored inside the Docker container
prepare_restore:
	(cd ../../.. && \
	mkdir -p .tmp && \
	find src -type f \( \
		-name "*.fsproj" \
		-or -name "*.csproj" \
		-or -name "*.sln" \
		\) \
		-exec sh -c "mkdir -p .tmp/\$$(dirname {}); cp {} .tmp/\$$(dirname {})/" \;)

build_image:
	(cd ../../.. && \
	docker build -t logary/rutta:latest \
		-t logary/rutta:$(COMMIT_SHA) \
		-t logary/rutta:$(TAG_VERSION_SUFFIX) \
		-f src/services/Logary.Services.Rutta/Dockerfile \
		.)

test_kustomize:
	@echo "Testing DaemonSet"
	kustomize build k8s/as-daemonset
	@echo ""
	@echo "Testing Deployment"
	kustomize build k8s/as-deployment
	@echo ""
	@echo "Testing Deployment with Scaling"
	kustomize build k8s/as-deployment-with-scaling
	@echo ""
	@echo "Testing Deployment with Istio"
	kustomize build k8s/with-istio
	@echo ""
	@echo "Tests pass!"

deploy_as_daemonset:
	kustomize build k8s/as-daemonset | kubectl apply -f -

deploy_as_deployment:
	kustomize build k8s/as-deployment | kubectl apply -f -

simple_consumer:
	kubectl run kafka-consumer -ti --image=strimzi/kafka:latest-kafka-2.4.0 --rm=true --restart=Never -- bin/kafka-console-consumer.sh --bootstrap-server $(KAFKA_CLUSTER_NAME)-kafka-bootstrap.kafka.svc:9092 --topic strimzi-tutorial --from-beginning

simple_producer:
	 kubectl run kafka-producer -ti --image=strimzi/kafka:latest-kafka-2.4.0 --rm=true --restart=Never -- bin/kafka-console-producer.sh --broker-list $(KAFKA_CLUSTER_NAME)-kafka-bootstrap.kafka.svc:9092 --topic strimzi-tutorial

push:
	docker push logary/rutta:latest
	docker push logary/rutta:$(COMMIT_SHA)
	docker push logary/rutta:$(TAG_VERSION_SUFFIX)

release: image push
