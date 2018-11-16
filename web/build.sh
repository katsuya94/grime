#!/usr/bin/env bash
set -o errexit
set -o nounset

cd "$(dirname "${BASH_SOURCE[0]}")"
GOOS=js GOARCH=wasm go build -o web.wasm
cp -f "$(go env GOROOT)/misc/wasm/wasm_exec.js" .
