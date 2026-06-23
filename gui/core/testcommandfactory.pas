unit testcommandfactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTestCommand = (tcICT, tcMAC, tcFlash, tcFuncTest, tcEEPROM, tcDoLabel, tcAppsCheck);

  { TTestCommandFactory - Central registry for test command metadata }
  TTestCommandFactory = class
  public
    { Get the executable command name for a test type }
    function GetCommand(TestCmd: TTestCommand): string;
    
    { Get progress bar increment value }
    function GetProgressValue(TestCmd: TTestCommand): integer;
    
    { Whether this test is required during retest }
    function IsRetestRequired(TestCmd: TTestCommand): boolean;
    
    { Get display name }
    function GetDisplayName(TestCmd: TTestCommand): string;
    
    { Get all test commands in order }
    function GetAllCommands: array of TTestCommand;
  end;

implementation

function TTestCommandFactory.GetCommand(TestCmd: TTestCommand): string;
begin
  case TestCmd of
    tcICT: Result := 'ict';
    tcMAC: Result := 'progmac';
    tcFlash: Result := 'flash';
    tcFuncTest: Result := 'functest';
    tcEEPROM: Result := 'eeprom';
    tcDoLabel: Result := 'dolabel';
    tcAppsCheck: Result := 'pingM1apps';
  else
    Result := '';
  end;
end;

function TTestCommandFactory.GetProgressValue(TestCmd: TTestCommand): integer;
begin
  case TestCmd of
    tcICT: Result := 5;
    tcMAC: Result := 3;
    tcFlash: Result := 40;
    tcFuncTest: Result := 10;
    tcEEPROM: Result := 10;
    tcDoLabel: Result := 10;
    tcAppsCheck: Result := 5;
  else
    Result := 5;
  end;
end;

function TTestCommandFactory.IsRetestRequired(TestCmd: TTestCommand): boolean;
begin
  case TestCmd of
    tcICT, tcMAC, tcEEPROM, tcFuncTest, tcDoLabel: Result := True;
    tcFlash, tcAppsCheck: Result := False;
  else
    Result := True;
  end;
end;

function TTestCommandFactory.GetDisplayName(TestCmd: TTestCommand): string;
begin
  case TestCmd of
    tcICT: Result := 'In-Circuit Test';
    tcMAC: Result := 'MAC Programming';
    tcFlash: Result := 'Flash Programming';
    tcFuncTest: Result := 'Functional Test';
    tcEEPROM: Result := 'EEPROM Test';
    tcDoLabel: Result := 'Label Programming';
    tcAppsCheck: Result := 'Apps Verification';
  else
    Result := 'Unknown Test';
  end;
end;

function TTestCommandFactory.GetAllCommands: array of TTestCommand;
begin
  { Return in execution order }
  SetLength(Result, 7);
  Result[0] := tcDoLabel;
  Result[1] := tcEEPROM;
  Result[2] := tcFlash;
  Result[3] := tcFuncTest;
  Result[4] := tcICT;
  Result[5] := tcMAC;
  Result[6] := tcAppsCheck;
end;

end.
