#!/usr/bin/env node

const getBinary = require('./get-binary');
process.env['ESBUILD_PATH'] = __dirname + "/../node_modules/.bin/esbuild";
getBinary().run();
