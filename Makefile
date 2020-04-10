GEN=./api
ENVOY_TAG=v1.13.1

build:
	stack build --fast #&& stack run

download:
	git clone -b common-protos-1_3_1 \
		https://github.com/googleapis/googleapis.git protos/googleapis
	git clone -b v0.3.0-java \
		https://github.com/envoyproxy/protoc-gen-validate.git protos/validate
	git clone https://github.com/cncf/udpa.git protos/udpa && \
		cd protos/udpa && git checkout db4b343e48c1264bb4d9ff491b059300701dc7c7
	git clone -b v3.11.4 \
		https://github.com/protocolbuffers/protobuf.git protos/google
	git clone -b $(ENVOY_TAG) \
		https://github.com/envoyproxy/envoy.git protos/envoy

plugin:
	mkdir -p gen-bin
	stack install --local-bin-path=gen-bin proto-lens-protoc

gen:
	mkdir -p $(GEN)
	protoc  "--plugin=protoc-gen-haskell-protolens=./gen-bin/proto-lens-protoc" \
		-I./protos/google/src \
		-I./protos/envoy/api \
		-I./protos/udpa \
		-I./protos/validate \
		-I./protos/googleapis/ \
		--haskell-protolens_out=$(GEN) \
		\
		protos/envoy/api/envoy/api/v2/core/address.proto \
		protos/envoy/api/envoy/api/v2/core/base.proto \
		protos/envoy/api/envoy/api/v2/core/health_check.proto \
		protos/envoy/api/envoy/api/v2/core/config_source.proto \
		protos/envoy/api/envoy/api/v2/core/grpc_service.proto \
		protos/envoy/api/envoy/api/v2/core/http_uri.proto \
		protos/envoy/api/envoy/api/v2/core/protocol.proto \
		protos/envoy/api/envoy/api/v2/auth/cert.proto \
		protos/envoy/api/envoy/api/v2/cluster.proto \
		protos/envoy/api/envoy/api/v2/cluster/circuit_breaker.proto \
		protos/envoy/api/envoy/api/v2/cluster/filter.proto \
		protos/envoy/api/envoy/api/v2/cluster/outlier_detection.proto \
		protos/envoy/api/envoy/api/v2/discovery.proto \
		protos/envoy/api/envoy/api/v2/endpoint.proto \
		protos/envoy/api/envoy/api/v2/endpoint/endpoint_components.proto \
		protos/envoy/api/envoy/api/v2/route.proto \
		protos/envoy/api/envoy/api/v2/scoped_route.proto \
		protos/envoy/api/envoy/api/v2/listener.proto \
		protos/envoy/api/envoy/api/v2/listener/listener_components.proto \
		protos/envoy/api/envoy/api/v2/listener/udp_listener_config.proto \
		protos/envoy/api/envoy/api/v2/route/route_components.proto \
		protos/envoy/api/envoy/config/filter/accesslog/v2/accesslog.proto \
		protos/envoy/api/envoy/config/filter/network/http_connection_manager/v2/http_connection_manager.proto \
		protos/envoy/api/envoy/config/listener/v2/api_listener.proto \
		protos/envoy/api/envoy/service/discovery/v2/ads.proto \
		protos/envoy/api/envoy/type/http.proto \
		protos/envoy/api/envoy/type/percent.proto \
		protos/envoy/api/envoy/type/range.proto \
		protos/envoy/api/envoy/type/semantic_version.proto \
		protos/envoy/api/envoy/type/matcher/regex.proto \
		protos/envoy/api/envoy/type/matcher/string.proto \
		protos/envoy/api/envoy/type/metadata/v2/metadata.proto \
		protos/envoy/api/envoy/type/tracing/v2/custom_tag.proto \
		protos/envoy/api/envoy/annotations/deprecation.proto \
		protos/envoy/api/envoy/annotations/resource.proto \
		\
		protos/google/src/google/protobuf/any.proto \
		protos/google/src/google/protobuf/empty.proto \
		protos/google/src/google/protobuf/struct.proto \
		protos/google/src/google/protobuf/wrappers.proto \
		protos/google/src/google/protobuf/duration.proto \
		protos/google/src/google/protobuf/timestamp.proto \
		protos/google/src/google/protobuf/descriptor.proto \
		\
		protos/googleapis/google/rpc/status.proto \
		protos/googleapis/google/api/annotations.proto \
		protos/googleapis/google/api/http.proto \
		\
		protos/udpa/udpa/annotations/versioning.proto \
		protos/udpa/udpa/annotations/migrate.proto \
		protos/udpa/udpa/annotations/sensitive.proto \
		\
		protos/validate/validate/validate.proto
	find api/ -type f -name "*_Fields.hs" | xargs rm -v

certs:
	openssl genrsa -out key.pem 2048
	openssl req -new -key key.pem -out certificate.csr
	openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem

clean:
	rm -rf protos/ gen-bin/ *.pem *.csr

.PHONY: build download plugin gen certs clean
