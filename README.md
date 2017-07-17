
emq_lwm2m
=========

LWM2M Gateway for the EMQ Broker

Configure Plugin
----------------

File: etc/emq_lwm2m.conf

```
lwm2m.port = 5783
lwm2m.keepalive = 120
lwm2m.certfile = etc/certs/cert.pem
lwm2m.keyfile = etc/certs/key.pem
```
- lwm2m.port
  + UDP port for coap. The secured port will be (lwm2m.port+1).
- lwm2m.keepalive
  + Interval for keepalive, in seconds.
- lwm2m.certfile
  + server certificate for DTLS
- lwm2m.keyfile
  + private key for DTLS

Load Plugin
-----------

```
./bin/emqttd_ctl plugins load emq_lwm2m
```


# topics

Downlink command topic is "lwm2m/{?device_end_point_name}/command".
Uplink response topic is "lwm2m/{?device_end_point_name}/response".




### READ command

#### Read resource
```
{
    "CmdID": {?CmdID},
    "Command": "Read",
    "BaseName": {?BaseName}
}
```

Response
```
{
    "CmdID": {?CmdID},
    "Command": "Read",
    "Result": {?JsonValue}
}
```
or
```
{
    "CmdID": {?CmdID},
    "Command": "Read",
    "Error": {?Error}
}
```
- {?JsonValue} is the json presentation of LWM2M data, please refer to OMA-TS-LightweightM2M-V1_0-20170208-A.pdf section 6.4.4 JSON.



### WRITE command
#### Write resource
```
{
    "CmdID": {?CmdID},
    "Command": "Write",
    "Value": {?JsonValue}
}
```
- {?CmdID}, an integer to identify a command response against its request.
- {?JsonValue} is the json presentation of LWM2M data, please refer to OMA-TS-LightweightM2M-V1_0-20170208-A.pdf section 6.4.4 JSON.


Response
```
{
    "CmdID": {?CmdID},
    "Command": "Write",
    "Result": {?Code}
}
```
- {?CmdID}, an integer to identify a command response against its request.
- {?Code} could be "Changed", "Bad Request", "Not Found", "Unauthorized" or "Method Not Allowed"




## NOTES
Fireware object is not supported now.


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



License
-------

Apache License Version 2.0

Author
------

Feng Lee <feng@emqtt.io>

