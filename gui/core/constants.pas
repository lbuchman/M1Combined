unit constants;

{$mode objfpc}{$H+}

interface

const
  { Process exit codes }
  PROCESS_TERMINATED = -9;
  NORMAL_EXIT = 0;
  PROCESS_EXEC_ERROR = 1;
  PRECHECK_HW_FAILED = 15;
  OTP_IS_NOT_BLANK = 10;
  EEPROM_IS_NOT_BLANK = 11;

  { Time intervals }
  INTERVAL_7_DAYS = 24 * 60 * 60 * 7;

  { File timestamps }
  UPDATE_FW_TIMESTAMP = 'UpdateFwTimeStamp.txt';
  UPDATE_SECRETS_TIMESTAMP = 'UpdateSycretsTimeStamp.txt';
  UPDATE_LOGS_TIMESTAMP = 'UpdateLogsTimeStamp.txt';

  { Default values }
  DEFAULT_DATETIME = '2023-01-24 21:20:08';
  DEFAULT_DEBUG_LEVEL = '0';
  DEBUG_LEVEL_OFF = '0';
  DEBUG_LEVEL_1 = '1';
  DEBUG_LEVEL_2 = '2';

  { Application paths }
  PID_FILE_PATH = '/m1mtf/m1tfd1app.pid';
  CONFIG_FILE_PATH = '/var/snap/m1tfd1/current/config.json';

  { M1 environment }
  M1_HOME_DIR = '/m1mtf/';
  M1_DEBUG_ENV = 'm1tfdebug';

implementation

end.
