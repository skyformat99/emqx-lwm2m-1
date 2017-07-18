


import sys, time
import paho.mqtt.client as mqtt


quit_now = False
DeviceId = "jXtestlwm2m"
conclusion = False

def on_connect(mqttc, userdata, flags, rc):
    global DeviceId
    json = "{\"CmdID\":3,\"Command\":\"Discover\",\"BaseName\":\"/3\"}"
    mqttc.publish("lwm2m/"+DeviceId+"/command", json)

def on_message(mqttc, userdata, msg):
    global quit_now, conclusion
    print(["incoming message", msg])
    conclusion = True
    
    mqttc.disconnect()
    quit_now = True

def on_publish(mqttc, userdata, mid):
    pass



def main():
    global DeviceId, conclusion
    timeout = 7
    mqttc = mqtt.Client("test_coap_lwm2m_c02334")
    mqttc.on_message = on_message
    mqttc.on_publish = on_publish
    mqttc.on_connect = on_connect

    mqttc.connect("127.0.0.1", 1883, 120)
    mqttc.subscribe("lwm2m/"+DeviceId+"/respose")
    mqttc.loop_start()
    while quit_now == False and timeout > 0:
        time.sleep(1)
        timeout = timeout - 1
    mqttc.loop_stop()
    if conclusion:
        print("\n\n    CASE1 PASS\n\n")
    else:
        print("\n\n    CASE1 FAIL\n\n")


if __name__ == "__main__":
    main()

    

