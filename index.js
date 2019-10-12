import { Elm } from './src/Main.elm'


Sentry.init({
  dsn: 'https://492e705e05e54021b4b22cb193874db6@sentry.io/1777628',
  environment: process.env.NODE_ENV
});

var app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: {
    env: process.env.NODE_ENV, // magic compile-time constant from Parcel; not a real variable!
    cache: localStorage.getItem('cache') || ''
  }
});

app.ports.cache.subscribe(function(data) {
  Sentry.configureScope(function(scope) {
    var string = JSON.stringify(data);
    scope.setLevel("info");
    scope.setFingerprint(['cache']);
    Sentry.captureMessage(string)
    localStorage.setItem('cache', string);
  });
});

app.ports.log.subscribe(function(data) {
  console[data.level || 'error']('log', data);
  Sentry.captureException(new Error(JSON.stringify(data)));
});

Sentry.captureMessage("load", "debug")
