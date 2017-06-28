
emq_lwm2m
=========

LWM2M Gateway for the EMQ Broker

Configure Plugin
----------------

File: etc/emq_lwm2m.conf

```
lwmwm.port = 5683
lwmwm.keepalive = 120
lwmwm.certfile = etc/certs/cert.pem
lwmwm.keyfile = etc/certs/key.pem
```
- lwmwm.port
  + UDP port for coap.
- lwmwm.keepalive
  + Interval for keepalive, in seconds.
- lwmwm.certfile
  + server certificate for DTLS
- lwmwm.keyfile
  + private key for DTLS

Load Plugin
-----------

```
./bin/emqttd_ctl plugins load emq_lwm2m
```


# topics

Downlink command topic is "lwm2m/{?device_end_point_name}/command".
Uplink response topic is "lwm2m/{?device_end_point_name}/response".


DTLS
-----------
emq-coap support DTLS to secure UDP data.

Please config coap.certfile and coap.keyfile in emq_coap.conf. If certfile or keyfile are invalid, DTLS will be turned off and you could read a error message in system log.

## Client
libcoap is an excellent coap library which has a simple client tool.

To compile libcoap, do following steps:

```
git clone http://github.com/obgm/libcoap
cd libcoap
./autogen.sh
./configure --enable-documentation=no --enable-tests=no
make
```

### Publish example:
```
libcoap/examples/coap-client -m put -e 1234  "coap://127.0.0.1/mqtt/topic1?c=client1&u=tom&p=secret"
```
- topic name is topic1
- client id is client1
- username is tom
- password is secret
- payload is a text string "1234"

### Subscribe example:

```
libcoap/examples/coap-client -m get -s 10 "coap://127.0.0.1/mqtt/topic1?c=client1&u=tom&p=secret"
```
- topic name is topic1
- client id is client1
- username is tom
- password is secret
- subscribe time is 10 seconds

And you will get following result if anybody sent message with text "1234567" on topic1:

```
v:1 t:CON c:GET i:31ae {} [ ]
1234567v:1 t:CON c:GET i:31af {} [ Observe:1, Uri-Path:mqtt, Uri-Path:topic1, Uri-Query:c=client1, Uri-Query:u=tom, Uri-Query:p=secret ]
```

The output message is not well formatted which hide "1234567" at the head of the 2nd line.


### NOTES
emq_coap gateway does not accept POST and DELETE request.


## Known Issues
- Upon unloading emq-coap plugin, udp dtls port (5884 by default) could not be closed properly.


License
-------

Apache License Version 2.0

Author
------

Feng Lee <feng@emqtt.io>

