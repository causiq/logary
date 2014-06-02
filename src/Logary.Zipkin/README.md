# Logary ZipKin target

Running tests - like normal.

Starting RestKin:

```
# first of all, in your ~/.bash_profile or equiv.:
export JAVA_HOME=`/usr/libexec/java_home -v 1.7`

# be sure to add your hostname to /usr/local/etc/dnsmasq.conf:
# address=/parametised/127.0.0.1

# download restkin, first window
git clone https://github.com/racker/restkin.git
cd restkin
pip install -r requirements.txt
./bin/restkin-api --scribe=tcp:localhost:9410

# another window, starting zipkin (downloads may take about 30 minutes)
git clone https://github.com/twitter/zipkin.git
cd zipkin
bin/collector

# another window
bin/query

# another window
bin/web

# another window, testing restkin/zipkin integration
git clone https://github.com/racker/tryfer.git
cd tryfer
pip install -r requirements.txt
python examples/tracing-client-to-restkin.py 

# open your browser to view the traces
open http://localhost:8080/

# run sample logger
rake
mono src/Logary.Zipkin.Example/bin/Release/Logary.Zipkin.Example.exe
```

More information on installing a more production worthy setup:

http://twitter.github.io/zipkin/install.html
