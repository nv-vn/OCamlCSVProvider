#!/bin/bash
if [ -n `ocamlfind query ppx_csv_provider` ]; then
  echo 'Removing previously installed versions of `ppx_csv_provider`'
  ocamlfind remove ppx_csv_provider
fi
ocamlfind install ppx_csv_provider ./src/META
ocamlfind install -add ppx_csv_provider ./csv_provider.native
