'use strict';

const express = require('express');
const cors = require('cors');

const app = express();

app.use(express.urlencoded({ extended: true }));
app.use(express.json());
app.use(cors());

const commands = require('./routes/commands');
const target = require('./routes/target');

app.use('/commands', commands);
app.use('/target', target);
app.use('/clear', commands);
module.exports = app;
