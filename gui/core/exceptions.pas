unit exceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { Base exception for all test-related errors }
  TTestException = class(Exception);

  { Serial number validation failed }
  TSerialValidationError = class(TTestException);

  { External process execution failed }
  TProcessExecutionError = class(TTestException);

  { Configuration file issues }
  TConfigurationError = class(TTestException);

  { Hardware communication failed }
  THardwareError = class(TTestException);

  { LED/UI control error }
  TUIError = class(TTestException);

  { Timeout waiting for process }
  TTimeoutError = class(TTestException);

implementation

end.
