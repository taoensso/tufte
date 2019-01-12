#!/bin/bash

clj -m cljs.main --target node --output-to main.js -c example.hello && node ./main.js

