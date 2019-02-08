import '../css/style.scss';

/** Add support for window.customElements */
import '@webcomponents/webcomponentsjs/custom-elements-es5-adapter.js'
import '@webcomponents/webcomponentsjs/webcomponents-bundle.js'

import CopyText from './copy-text.js'

const {
  Elm
} = require('../src/Main.elm');

const app = Elm.Main.init({
  node: document.getElementById('elm'),
  flags: {
    session: localStorage.session || null
  },
});

CopyText.start(app)

let hasUnsavedWork = false;
let previousLocation = window.location.pathname



app.ports.infoForOutside.subscribe(function(msg) {
  if (msg.tag == "LogError") {
    console.error(msg.data);
  } else if (msg.tag == "ClearSession") {
    localStorage.removeItem("session")
  } else if (msg.tag == "StoreSession") {
    localStorage.setItem("session", msg.data);
  } else if (msg.tag == "SetEditState") {
    console.log('SetEditState', hasUnsavedWork);
    hasUnsavedWork = msg.data;
  } else if (msg.tag == "ReplaceState") {

    /* To work around that the Navigation library force to call update function everytime calling `modifyUrl`,
     * the port is set up to just change the url of the history.
     */

    history.replaceState({}, '', msg.data);
  } else if (msg.tag == "FileSelected") {

  }
});


window.addEventListener("storage", function(event) {
  if (event.storageArea === localStorage && event.key === "session") {
    // app.ports.onSessionChange.send(event.newValue);
    console.log("storage", event.newValue);
    app.ports.infoForElm.send({
      tag: "SessionChange",
      data: event.newValue
    })
  }
}, false);

/*
 *
 */
window.addEventListener('popstate', e => {
  console.log('previousLocation', previousLocation);
  console.log('hasUnsavedWork', hasUnsavedWork);
  if (hasUnsavedWork) {
    const result = window.confirm('保存していない変更はが失われますが、移動しますか？')
    if (!result) {
      window.history.pushState({}, '', previousLocation)
      e.preventDefault()
    }
  }
})

window.addEventListener('beforeunload', function(e) {
  if (hasUnsavedWork) {
    e.returnValue = 'You have unsaved work. Are you sure you want to go?'
  }
})
