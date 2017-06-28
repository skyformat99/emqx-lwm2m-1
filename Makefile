PROJECT = emq_coap
PROJECT_DESCRIPTION = CoAP Gateway
PROJECT_VERSION = 2.2

DEPS = lager gen_coap
dep_lager    = git https://github.com/basho/lager
dep_gen_coap = git https://github.com/grutabow/gen_coap  lwm2m

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd emq22
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_coap.conf -i priv/emq_coap.schema -d data
