PROJECT = emq_lwm2m
PROJECT_DESCRIPTION = LWM2M Gateway
PROJECT_VERSION = 0.1

DEPS = lager lwm2m_coap jsx
dep_lager      = git https://github.com/basho/lager
dep_lwm2m_coap = git https://github.com/grutabow/lwm2m-coap
dep_jsx        = git https://github.com/talentdeficit/jsx


BUILD_DEPS = emqttd cuttlefish
dep_emqttd     = git https://github.com/emqtt/emqttd emq22
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

NO_AUTOPATCH = lwm2m_coap

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_lwm2m.conf -i priv/emq_lwm2m.schema -d data
