'use strict';

const express = require('express');

const router = express.Router();

const targetService = require('../services/target.service');

router.post('/', targetService.executeCommand);


module.exports = router;
