GEN=./api
ENVOY_TAG=v1.14.1

build:
	stack build --fast #&& stack run

download:
	git clone -b common-protos-1_3_1 \
		https://github.com/googleapis/googleapis.git protos/googleapis
	git clone https://github.com/envoyproxy/protoc-gen-validate.git protos/validate && \
	  cd protos/validate && git checkout 0f2bc6c0fdac9113e3863ea6e30e5b2bd33e3b40
	git clone https://github.com/cncf/udpa.git protos/udpa && \
		cd protos/udpa && git checkout e8cd3a4bb307e2c810cffff99f93e96e6d7fee85
	git clone -b v3.11.4 \
		https://github.com/protocolbuffers/protobuf.git protos/google
	git clone -b v0.2.1 \
	  https://github.com/census-instrumentation/opencensus-proto.git protos/opencensus
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
		-I./protos/opencensus/src \
		--haskell-protolens_out=$(GEN) \
		\
		protos/envoy/api/envoy/annotations/deprecation.proto \
		\
		protos/envoy/api/envoy/extensions/filters/network/http_connection_manager/v3/http_connection_manager.proto \
		protos/envoy/api/envoy/extensions/transport_sockets/tls/v3/cert.proto \
		\
		protos/envoy/api/envoy/config/accesslog/v3/accesslog.proto \
		\
		protos/envoy/api/envoy/config/cluster/v3/circuit_breaker.proto \
		protos/envoy/api/envoy/config/cluster/v3/cluster.proto \
		protos/envoy/api/envoy/config/cluster/v3/filter.proto \
		protos/envoy/api/envoy/config/cluster/v3/outlier_detection.proto \
		\
		protos/envoy/api/envoy/config/core/v3/address.proto \
		protos/envoy/api/envoy/config/core/v3/backoff.proto \
		protos/envoy/api/envoy/config/core/v3/base.proto \
		protos/envoy/api/envoy/config/core/v3/config_source.proto \
		protos/envoy/api/envoy/config/core/v3/event_service_config.proto \
		protos/envoy/api/envoy/config/core/v3/grpc_service.proto \
		protos/envoy/api/envoy/config/core/v3/health_check.proto \
		protos/envoy/api/envoy/config/core/v3/http_uri.proto \
		protos/envoy/api/envoy/config/core/v3/protocol.proto \
		protos/envoy/api/envoy/config/core/v3/socket_option.proto \
		\
		protos/envoy/api/envoy/config/endpoint/v3/endpoint.proto \
		protos/envoy/api/envoy/config/endpoint/v3/endpoint_components.proto \
		\
		protos/envoy/api/envoy/config/listener/v3/api_listener.proto \
		protos/envoy/api/envoy/config/listener/v3/listener.proto \
		protos/envoy/api/envoy/config/listener/v3/listener_components.proto \
		protos/envoy/api/envoy/config/listener/v3/udp_listener_config.proto \
		\
		protos/envoy/api/envoy/config/route/v3/route.proto \
		protos/envoy/api/envoy/config/route/v3/route_components.proto \
		protos/envoy/api/envoy/config/route/v3/scoped_route.proto \
		\
		protos/envoy/api/envoy/config/trace/v3/trace.proto \
		\
		protos/envoy/api/envoy/service/discovery/v3/ads.proto \
		protos/envoy/api/envoy/service/discovery/v3/discovery.proto \
		\
		protos/envoy/api/envoy/type/metadata/v3/metadata.proto \
		protos/envoy/api/envoy/type/matcher/v3/regex.proto \
		protos/envoy/api/envoy/type/matcher/v3/string.proto \
		protos/envoy/api/envoy/type/tracing/v3/custom_tag.proto \
		protos/envoy/api/envoy/type/v3/http.proto \
		protos/envoy/api/envoy/type/v3/percent.proto \
		protos/envoy/api/envoy/type/v3/range.proto \
		protos/envoy/api/envoy/type/v3/semantic_version.proto \
		\
		protos/google/src/google/protobuf/any.proto \
		protos/google/src/google/protobuf/descriptor.proto \
		protos/google/src/google/protobuf/duration.proto \
		protos/google/src/google/protobuf/empty.proto \
		protos/google/src/google/protobuf/struct.proto \
		protos/google/src/google/protobuf/timestamp.proto \
		protos/google/src/google/protobuf/wrappers.proto \
		\
		protos/googleapis/google/api/annotations.proto \
		protos/googleapis/google/api/http.proto \
		protos/googleapis/google/rpc/status.proto \
		\
		protos/udpa/udpa/annotations/sensitive.proto \
		protos/udpa/udpa/annotations/status.proto \
		protos/udpa/udpa/annotations/versioning.proto \
		\
		protos/opencensus/src/opencensus/proto/trace/v1/trace_config.proto \
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
