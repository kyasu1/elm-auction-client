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

app.ports.elmToJs.subscribe(function({
  tag,
  payload
}) {
  switch (tag) {
    case 'FileSelected':
      fileRead(payload)
  }
})

function fileRead(id) {
  var node = document.getElementById(id);

  if (node === null) {
    return;
  }

  let promises = []
  for (let i = 0, file; file = node.files[i]; i++) {
    promises.push(new Promise((resolve, reject) => {
      const reader = new FileReader();

      reader.onload = (function(event) {
        const base64encoded = event.target.result;
        node.value = '';
        resolve({
          contents: base64encoded,
          filenmae: file.filename,
        })
      })

      reader.readAsDataURL(file);
    }))
  }

  Promise.all(promises)
    .then(data => {
      app.ports.jsToElm.send(data);
    })
    .catch(err =>
      console.log('Error at Promise.all in fileRead: ' + err)
    )


  /*
    var reader = new FileReader();
    // FileReader API is event based. Once a file is selected
    // it fires events. We hook into the `onload` event for our reader.
    reader.onload = (function(event) {
      // The event carries the `target`. The `target` is the file
      // that was selected. The result is base64 encoded contents of the file.
      var base64encoded = event.target.result;
      // We build up the `ImagePortData` object here that will be passed to our Elm
      // runtime through the `fileContentRead` subscription.
      var portData = [{
        contents: base64encoded,
        filename: file.name
      }];
      // We call the `fileContentRead` port with the file data
      // which will be sent to our Elm runtime via Subscriptions.
      app.ports.jsToElm.send(portData);
    });
    // Connect our FileReader with the file that was selected in our `input` node.
    reader.readAsDataURL(file);
  */
}
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
  } else if (msg.tag == "CopyToClipBoard") {
    // const textArea = document.createElement('textarea')
    // textArea.innerHTML = msg.data
    // const parent = document.getElementById('clipboard')
    // parent.appendChild(textArea)
    // textArea.select()
    // document.execCommand('copy')
    // parent.removeChild(textArea)
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
