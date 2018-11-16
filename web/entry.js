const Terminal = require("xterm").Terminal;
require("go/misc/wasm/wasm_exec");

window.terminal = new Terminal();
window.terminal.open(document.getElementById("terminal"));

const go = new Go();
WebAssembly.instantiateStreaming(fetch("web.wasm"), go.importObject).then(
  result => {
    go.run(result.instance);
  }
);
