module.exports = {
  mode: "production",
  entry: "./entry.js",
  output: {
    path: __dirname,
    filename: "web.js"
  },
  resolve: {
    alias: {
      go: process.env.GOROOT
    }
  },
  module: {
    noParse: process.env.GOROOT
  }
};
