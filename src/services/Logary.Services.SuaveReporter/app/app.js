var LogaryClient = require('logary-js/lib/client');

(function() {
  var logary = new LogaryClient();

  try {
    console.debug('writing error');
    throw new Error('hello from logary-js');
  } catch (err) {
    logary.push(err);
  }
})();
