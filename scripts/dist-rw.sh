#!/bin/bash

rm -rf ./dist
mkdir -p ./dist
cp -R css fonts images vendor rainwater.html rainwater.js rainwater_controls.js favicon.ico ./dist
mv ./dist/rainwater.html ./dist/index.html
lftp -e "mirror -R dist calculator-distrib" -u ftp 10.243.106.158 -p 2121
