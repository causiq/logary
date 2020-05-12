.PHONY: dotnet_build image test_kustomize deploy_as_daemonset deploy_as_deployment push release

TAG_VERSION_SUFFIX := $(shell ../../../tools/version.sh)

image:
	(cd ../../.. && \
	docker build -t logary/rutta:latest -t logary/rutta:$(TAG_VERSION_SUFFIX) \
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
	@echo "Tests pass!"

deploy_as_daemonset:
	kustomize build k8s/as-daemonset | kubectl apply -f -

deploy_as_deployment:
	kustomize build k8s/as-deployment | kubectl apply -f -

push:
	docker push logary/rutta:latest
	docker push logary/rutta:$(TAG_VERSION_SUFFIX)

release: image push