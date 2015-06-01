var LogaryClient = require('logary-js'),
    jQuery       = require('jquery');

(function() {
  // this by default both logs to server & console
  var logary = new LogaryClient();

  try {
    throw new Error('hello from logary-js');
  } catch (err) {
    logary.push(err);
  }
})();
