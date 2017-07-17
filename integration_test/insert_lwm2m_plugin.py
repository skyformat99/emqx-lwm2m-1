

f = open("emq-relx/Makefile", "rb")
data = read(f)
f.close()

if data.find("emq_lwm2m") < 0:
    data = data.replace("emq_lua_hook emq_elixir_plugin", "emq_lua_hook emq_elixir_plugin emq_lwm2m\n\ndep_emq_lwm2m = git https://github.com/emqtt/emq-lwm2m\n\n")
    f = open("emq-relx/Makefile", "wb")
    f.write(data)
    f.close()
    

f = open("emq-relx/relx.config", "rb")
data = read(f)
f.close()

if data.find("emq_lwm2m") < 0:
    f = open("emq-relx/relx.config", "wb")
    data = data.replace("{emq_sn, load},", "{emq_sn, load},\n{emq_lwm2m, load},")
    f.write(data)
    f.close()
    
    
