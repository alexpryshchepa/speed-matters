// Styling
import './index.scss';
// Polyfills
import 'regenerator-runtime/runtime';
import '@webcomponents/webcomponentsjs';
// Material web components
import '@material/mwc-drawer';
import '@material/mwc-top-app-bar-fixed';
import '@material/mwc-icon-button';
import '@material/mwc-fab';
import '@material/mwc-radio';
import '@material/mwc-formfield';
// Extended mwc components
import './js/custom-elements/custom-mwc-button';
import './js/custom-elements/custom-mwc-snackbar';
import './js/custom-elements/custom-mwc-textfield';
// Service worker
import * as serviceWorker from './js/serviceWorker';

// Elm app init
const { Elm } = require('./Main');
Elm.Main.init({
  node: document.getElementById('root'),
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
