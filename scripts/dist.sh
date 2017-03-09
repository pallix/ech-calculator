#!/bin/bash

rm -rf ./dist
mkdir -p ./dist
cp -R css fonts images vendor index.html rainwater.html rainwater_controls.js favicon.ico ./dist
lftp -e "mirror -R dist calculator-distrib" -u ftp 192.168.170.149 -p 2121
