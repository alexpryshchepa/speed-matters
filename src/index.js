// Polyfills
import 'regenerator-runtime/runtime'
import '@webcomponents/webcomponentsjs'
// Material web components
import '@material/mwc-drawer'
import '@material/mwc-top-app-bar-fixed'
import '@material/mwc-icon-button'
import '@material/mwc-fab'
import '@material/mwc-radio'
import '@material/mwc-formfield'
// Extended mwc components
import './js/custom-elements/custom-mwc-button'
import './js/custom-elements/custom-mwc-snackbar'
import './js/custom-elements/custom-mwc-textfield'
// Local storage
import store from 'store'
// Service worker
import * as serviceWorker from './js/serviceWorker'
// Styling
import './index.scss'

// Elm app init
const { Elm } = require('./Main')
const app = Elm.Main.init({
  node: document.getElementById('root'),
})

app.ports.saveToLocalStorage.subscribe(([key, value]) =>
  store.set(key, JSON.stringify(value))
)
app.ports.getFromLocalStorage.subscribe(key => {
  const data = store.get(key)

  if (data) {
    app.ports.responseFromLocalStorage.send(JSON.parse(data))
  }
})

app.ports.pageInitialized.subscribe(data => {
  const pagePath = data[0]
  const pageDescription = data[1]

  if (window.ga) {
    window.ga('set', 'page', pagePath)
    window.ga('send', 'pageview')
  }

  if (pageDescription.length) {
    const descriptionMeta = document.head.querySelector(
      '[name=description][content]'
    )

    if (descriptionMeta) {
      descriptionMeta.content = pageDescription
    } else {
      const meta = document.createElement('meta')
      meta.name = 'description'
      meta.content = pageDescription

      document.head.appendChild(meta)
    }
  }

  if (pageDescription.length) {
    let url = `https://spdmttrs.com${pagePath}`
    const canonicalLink = document.head.querySelector('[rel=canonical][href]')

    if (canonicalLink) {
      canonicalLink.href = url
    } else {
      const link = document.createElement('link')
      link.rel = 'canonical'
      link.href = url

      document.head.appendChild(link)
    }
  }
})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register()
