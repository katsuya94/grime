const xterm = require("xterm");
require("go/misc/wasm/wasm_exec");

class Terminal {
  constructor(xterminal) {
    this.xterminal = xterminal;
  }

  open(htmlElement) {
    this.xterminal.open(htmlElement);
  }

  write(bytes) {
    this.xterminal.write(bytes);
  }

  read(max) {
    return "";
  }
}

window.terminal = new Terminal(new xterm.Terminal());
window.terminal.open(document.getElementById("terminal"));

const go = new Go();
WebAssembly.instantiateStreaming(fetch("web.wasm"), go.importObject).then(
  result => {
    go.run(result.instance);
  }
);
