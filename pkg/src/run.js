#!/usr/bin/env node

const getBinary = require('./get-binary');
process.env['ROLLUP_PATH'] = __dirname + "/../node_modules/.bin/rollup";
getBinary().run();
