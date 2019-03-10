var port = process.env.PORT || process.argv[2] || 8080;
var bind = process.env.PORT || process.argv[3] || '127.0.0.1';
var express = require('express');
var app = express();
var SSE = require('express-sse');
var sse = new SSE([
  { name: "Rutta", value: "Started on {ip}:{port}", fields: { ip:"127.0.0.1", port: 8080 }, context: { "app": "Rutta" }, level: "info", timestamp: new Date() },
  { name: "Logary", value: "Started Logary library", fields: {}, context: { "app": "Rutta" }, level: "debug", timestamp: new Date() },
  { name: "Rutta.udp", value: "Listening on {ip}:{port}", fields: { ip: "0.0.0.0", port: 8080 }, context: { "app": "Rutta" }, level: "info", timestamp: new Date() },
  { name: "Rutta.tcp", value: "Listening on {ip}:{port}", fields: { ip: "0.0.0.0", port: 8080 }, context: { "app": "Rutta" }, level: "info", timestamp: new Date() },
  { name: "Rutta.http", value: "Failed to bind to {ip}:{port}", fields: { ip: "[2001::2]", port: 8080 }, context: { "app": "Rutta", "protocol":"http" }, level:"error", timestamp: new Date() },
  { name: "Rutta", value: "Shut down prematurely", context: { "app": "Rutta" }, level:"warn", timestamp: new Date() },
  [ { name: "Rutta", value: "Started on {ip}:{port}", fields: { ip:"127.0.0.1", port: 8080 }, context: { "app": "Rutta" }, level: "info", timestamp: new Date() },
    { name: "Logary", value: "Started Logary library", fields: {}, context: { "app": "Rutta" }, level: "debug", timestamp: new Date() },
    { name: "Rutta.udp", value: "Listening on {ip}:{port}", fields: { ip: "0.0.0.0", port: 8080 }, context: { "app": "Rutta" }, level: "info", timestamp: new Date() },
    { name: "Rutta.tcp", value: "Listening on {ip}:{port}", fields: { ip: "0.0.0.0", port: 8080 }, context: { "app": "Rutta" }, level: "info", timestamp: new Date() },
    { name: "Rutta.http", value: "Failed to bind to {ip}:{port}", fields: { ip: "[2001::2]", port: 8080 }, context: { "app": "Rutta", "protocol":"http" }, level:"error", timestamp: new Date() },
    { name: "Rutta", value: "Shut down prematurely", context: { "app": "Rutta" }, level:"warn", timestamp: new Date() } ],
  { name: "Rutta", value: "Started on {ip}:{port}", fields: { ip:"127.0.0.1", port: 8080 }, context: { "app": "Rutta" }, level: "info", timestamp: new Date() },
  { name: "Logary", value: "Started Logary library", fields: {}, context: { "app": "Rutta" }, level: "debug", timestamp: new Date() },
  { name: "Rutta.udp", value: "Listening on {ip}:{port}", fields: { ip: "0.0.0.0", port: 8080 }, context: { "app": "Rutta" }, level: "info", timestamp: new Date() },
  { name: "Rutta.tcp", value: "Listening on {ip}:{port}", fields: { ip: "0.0.0.0", port: 8080 }, context: { "app": "Rutta" }, level: "info", timestamp: new Date() },
  { name: "Rutta.http", value: "Failed to bind to {ip}:{port}", fields: { ip: "[2001::2]", port: 8080 }, context: { "app": "Rutta", "protocol":"http" }, level:"error", timestamp: new Date() },
  { name: "Rutta", value: "Shut down prematurely", context: { "app": "Rutta" }, level:"warn", timestamp: new Date() },
  { name: "Rutta", value: "Started on {ip}:{port}", fields: { ip:"127.0.0.1", port: 8080 }, context: { "app": "Rutta" }, level: "info", timestamp: new Date() },
  { name: "Logary", value: "Started Logary library", fields: {}, context: { "app": "Rutta" }, level: "debug", timestamp: new Date() },
  { name: "Rutta.udp", value: "Listening on {ip}:{port}", fields: { ip: "0.0.0.0", port: 8080 }, context: { "app": "Rutta" }, level: "info", timestamp: new Date() },
  { name: "Rutta.tcp", value: "Listening on {ip}:{port}", fields: { ip: "0.0.0.0", port: 8080 }, context: { "app": "Rutta" }, level: "info", timestamp: new Date() },
  { name: "Rutta.http", value: "Failed to bind to {ip}:{port}", fields: { ip: "[2001::2]", port: 8080 }, context: { "app": "Rutta", "protocol":"http" }, level:"error", timestamp: new Date() },
  { name: "Rutta", value: "Shut down prematurely", context: { "app": "Rutta" }, level:"warn", timestamp: new Date() },
]);
//var Gun     = require('gun');
//require('gun/axe');
//app.use(Gun.serve);
app.use(express.static(__dirname));

app.all("*", (req, res, next) => {
  res.header("Access-Control-Allow-Origin", "*");
  res.header("Access-Control-Allow-Headers", "X-Requested-With");
  next();
})

app.get('/logs', sse.init);

app.listen(port, bind, () => console.log(`Listening on port ${port}`));

//var gun = Gun({	file: 'data', web: server });
//global.Gun = Gun; /// make global to `node --inspect` - debug only
//global.gun = gun; /// make global to `node --inspect` - debug only

console.log('Server started on port ' + port + ' with /logs');
