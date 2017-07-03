
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


## Value Type
Value may have following type:
- text - ascii text format of int, float, string, boolean
- binary - data is arbitrary binary which is base64-encoded
- json - json format of multi-resource


### READ command
```
{
    "CmdID": {?CmdID},
    "Command": "Read",
    "ObjectID":  {?ObjectName},
    "ObjectInstanceID": {?ObjectInstanceID},
    "ResourceID": {?ResourceID}
}
```

Response
```
{
    "CmdID": {?CmdID},
    "ObjectID":  {?ObjectName},
    "ObjectInstanceID": {?ObjectInstanceID},
    "Result":
    [
        {
            "ResourceID": {?ResourceID},
            "ValueType": {?ValueType},
            "Value": {?Value}
        },
        {
            "ResourceID": {?ResourceIDX},
            "ValueType": {?ValueTypeX},
            "Value": {?ValueX}
        },
    ]
}
```



### WRITE command
```
{
    "CmdID": {?CmdID},
    "Command": "Write",
    "ObjectID":  {?ObjectName},
    "ObjectInstanceID": {?ObjectInstanceID},
    "Changes":
    [
        {
            "ResourceID": {?ResourceID},
            "ValueType": {?ValueType},
            "Value": {?Value}
        },
        {
            "ResourceID": {?ResourceIDX},
            "ValueType": {?ValueTypeX},
            "Value": {?ValueX}
        },
    ]
}
```
- {?CmdID}, an integer to identify a command response against its request.
- {?ObjectName}, a string represents object name, mandatory.
- {?ObjectInstanceID}, an integer, optional.
- {?ResourceID}, a string represents resource name, mandatory.
- {?ValueType}, a string represents the value type of {?Value}, mandatory.
- {?Value}, this parameter could be variant type as {?ValueType} specified, mandatory.


Response
```
{
    "CmdID": {?CmdID},
    "Result": {?Code}
}
```
- {?CmdID}, an integer to identify a command response against its request.
- {?Code} could be "Changed", "Bad Request", "Not Found", "Unauthorized" or "Method Not Allowed"



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

