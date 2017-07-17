
.PHONY: clean, clean_result, start_broker  stop_broker  case1 

ANJAY_ROOT ?= .

EMQ    = emq-relx/relx.config
ANJAY  = $(ANJAY_ROOT)/Anjay/output/bin/demo

all:  clean_result $(EMQ) $(ANJAY) start_broker clean_result case1 stop_broker
	@echo "  "
	@echo "  test complete"
	@echo "  "
	
clean_result:
	-rm -f case*.txt
	
start_broker:
	-rm -f emq-relx/_rel/emqttd/log/*
	-emq-relx/_rel/emqttd/bin/emqttd stop
	sleep 1
	emq-relx/_rel/emqttd/bin/emqttd start
	sleep 1
	emq-relx/_rel/emqttd/bin/emqttd_ctl plugins load emq_coap


stop_broker:
	-emq-relx/_rel/emqttd/bin/emqttd stop
	

case1:
	$(ANJAY_ROOT)/Anjay/output/bin/demo --server-uri coap://127.0.0.1:5783
	sleep 1
	python check_result.py  case1  case1_output.txt==w123G45
	


	
$(EMQ):
	git clone https://github.com/emqtt/emq-relx.git
	git clone https://github.com/emqtt/emq-lwm2m.git
	@echo "update emq-lwm2m with this development code"
	mv emq-lwm2m  emq_lwm2m
	-rm -rf emq_lwm2m/etc
	-rm -rf emq_lwm2m/include
	-rm -rf emq_lwm2m/priv
	-rm -rf emq_lwm2m/src
	-rm -rf emq_lwm2m/Makefile
	-rm -rf emq_lwm2m/erlang.mk
	cp -rf ../etc       emq_lwm2m/
	cp -rf ../include   emq_lwm2m/
	cp -rf ../priv      emq_lwm2m/
	cp -rf ../src       emq_lwm2m/
	cp -rf ../Makefile  emq_lwm2m/Makefile
	cp -rf ../erlang.mk emq_lwm2m/erlang.mk
	-mkdir emq-relx/deps
	mv emq_lwm2m  emq-relx/deps/
	@echo "start building ..."
	python insert_lwm2m_plugin.py
	make -C emq-relx -f Makefile
	

anjay: $(ANJAY)
	@echo "build anjay"
	
$(ANJAY):
	-mkdir $(ANJAY_ROOT)
	cd $(ANJAY_ROOT) && git clone https://github.com/AVSystem/Anjay.git
	cd $(ANJAY_ROOT)/Anjay && git submodule update --init && cmake . -DDTLS_BACKEND="" && make -j
	
r: rebuild_emq
	# r short for rebuild_emq
	@echo " rebuild complete "
	
	
rebuild_emq:
	-emq-relx/_rel/emqttd/bin/emqttd stop
	-rm -rf emq-relx/deps/emq_lwm2m/etc
	-rm -rf emq-relx/deps/emq_lwm2m/include
	-rm -rf emq-relx/deps/emq_lwm2m/priv
	-rm -rf emq-relx/deps/emq_lwm2m/src
	-rm -rf emq-relx/deps/emq_lwm2m/Makefile
	-rm -rf emq-relx/deps/emq_lwm2m/erlang.mk
	cp -rf ../etc       emq-relx/deps/emq_lwm2m/
	cp -rf ../include   emq-relx/deps/emq_lwm2m/
	cp -rf ../priv      emq-relx/deps/emq_lwm2m/
	cp -rf ../src       emq-relx/deps/emq_lwm2m/
	cp -rf ../Makefile  emq-relx/deps/emq_lwm2m/Makefile
	cp -rf ../erlang.mk emq-relx/deps/emq_lwm2m/erlang.mk
	make -C emq-relx -f Makefile
	
	
clean: clean_result
	-rm -f client/*.exe
	-rm -f client/*.o
	-rm -rf emq-relx
	-rm -rf $(ANJAY_ROOT)/Anjay
	
	
	
lazy: clean_result start_broker case1  stop_broker
	# custom your command here
	@echo "you are so lazy"
	
	
	