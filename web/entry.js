const buffer = require("buffer");
const xterm = require("xterm");
require("go/misc/wasm/wasm_exec");

class Terminal {
  constructor(xterm) {
    this.notify = null;
    this.buffer = buffer.Buffer.alloc(0);
    this.xterm = xterm;
    this.xterm.on("data", this.onData.bind(this));
  }

  open(htmlElement) {
    this.xterm.open(htmlElement);
  }

  write(data) {
    this.xterm.write(data);
  }

  onData(data) {
    for (let i = 0; i < data.length; i++) {
      this.onChar(data[i]);
    }
  }

  onChar(char) {
    switch (char) {
      case "\r":
        this.write("\n");
        this.send("\n");
      case "\x7f":
        this.write("\b \b");
        this.send("\b");
      default:
        this.write(char);
        this.send(char);
    }
  }

  send(data) {
    console.log(buffer.Buffer.from(data));
    this.buffer = buffer.Buffer.concat([this.buffer, buffer.Buffer.from(data)]);
  }

  read(max) {
    if (this.notify === null || this.buffer.length === 0) {
      requestAnimationFrame(this.read.bind(this));
      return;
    }
    const n = Math.min(this.buffer.length, max);
    this.notify(this.buffer.slice(0, n));
    this.buffer = this.buffer.slice(n, this.buffer.length);
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
