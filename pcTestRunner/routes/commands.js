'use strict';

const express = require('express');

const router = express.Router();

const commandsService = require('../services/commands.service');

router.post('/', commandsService.sendCommand);
router.get('/', commandsService.clearLog);

module.exports = router;
