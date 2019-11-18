import { Elm } from './src/Main.elm';
function blastoff () {
  Sentry && Sentry.init({
    dsn: 'https://492e705e05e54021b4b22cb193874db6@sentry.io/1777628',
    environment: process.env.NODE_ENV
  });

  var app = Elm.Main.init({
    // node: document.getElementById('main'),
    flags: {
      env: process.env.NODE_ENV, // magic compile-time constant from Parcel; not a real variable!
    }
  });

  app.ports.log && app.ports.log.subscribe(function(data) {
    console[data.level || 'error']('log', data);
    Sentry && Sentry.captureException(new Error(JSON.stringify(data)));
  });

  app.ports.setPath && app.ports.setPath.subscribe(function(p) {
    var path = '/' + p;
    console.log('setting path to', path)
    history.pushState({}, "", path);
    localStorage.setItem('key', path);
  });

  Sentry && Sentry.captureMessage("load", "debug");
}

window.addEventListener('DOMContentLoaded', blastoff);
