unit itesthandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IndLed;

type
  { ITestHandler - Interface for test execution handlers }
  ITestHandler = interface
    ['{12345678-1234-1234-1234-123456789012}']
    
    { Get friendly name of this test }
    function GetName: string;
    
    { Get progress bar increment value }
    function GetProgressValue: integer;
    
    { Get the LED control component associated with this test }
    function GetLedControl: TindLed;
    
    { Execute this test with given serial number and debug level }
    procedure Execute(Serial: string; DebugLevel: string);
    
    { Get the last error code from execution }
    function GetLastErrorCode: integer;
    
    { Get the last error message }
    function GetLastErrorMessage: string;
    
    { Is this test required in retest mode? }
    function IsRetestRequired: boolean;
    
    { Cancel execution if possible }
    procedure Cancel;
  end;

implementation

end.
