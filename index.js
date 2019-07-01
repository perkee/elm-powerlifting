import { Elm } from './src/Main.elm'

Elm.Main.init({
  node: document.getElementById('main'),
  flags: process.env.NODE_ENV // magic compile-time constant from Parcel; not a real variable!
});