

def change_makefile():
    f = open("emq-relx/Makefile", "rb")
    data = f.read()
    f.close()

    if data.find("emq_lwm2m") < 0:
        data = data.replace("emq_lua_hook emq_elixir_plugin", "emq_lua_hook emq_elixir_plugin emq_lwm2m\n\ndep_emq_lwm2m = git https://github.com/emqtt/emq-lwm2m\n\n")
        f = open("emq-relx/Makefile", "wb")
        f.write(data)
        f.close()
        

    f = open("emq-relx/relx.config", "rb")
    data = f.read()
    f.close()

    if data.find("emq_lwm2m") < 0:
        f = open("emq-relx/relx.config", "wb")
        data = data.replace("{emq_sn, load},", "{emq_sn, load},\n{emq_lwm2m, load},")
        f.write(data)
        f.close()
        
def change_lwm2m_config():
    f = open("emq-relx/deps/emq_lwm2m/etc/emq_lwm2m.conf", "rb")
    data = f.read()
    f.close()
    
    if data.find("lwm2m.port = 5783") > 0:
        data.replace("lwm2m.port = 5783", "lwm2m.port = 5683")
        f = open("emq-relx/deps/emq_lwm2m/etc/emq_lwm2m.conf", "wb")
        f.write(data)
        f.close()
    
    

def main():
    change_makefile()
    change_lwm2m_config
    
    
if __name__ == "__main__":
    main()
    
    