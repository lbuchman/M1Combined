unit ictesthandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IndLed, itesthandler, testcommandfactory;

type
  { TICTestHandler - Implementation of ITestHandler for ICT test }
  TICTestHandler = class(TInterfacedObject, ITestHandler)
  private
    FLedControl: TindLed;
    FLastErrorCode: integer;
    FLastErrorMessage: string;
  public
    constructor Create(LedControl: TindLed);
    destructor Destroy; override;

    { ITestHandler implementation }
    function GetName: string;
    function GetProgressValue: integer;
    function GetLedControl: TindLed;
    procedure Execute(Serial: string; DebugLevel: string);
    function GetLastErrorCode: integer;
    function GetLastErrorMessage: string;
    function IsRetestRequired: boolean;
    procedure Cancel;
  end;

implementation

uses
  testrunner, logger;

constructor TICTestHandler.Create(LedControl: TindLed);
begin
  inherited Create;
  FLedControl := LedControl;
  FLastErrorCode := 0;
  FLastErrorMessage := '';
end;

destructor TICTestHandler.Destroy;
begin
  inherited Destroy;
end;

function TICTestHandler.GetName: string;
begin
  Result := 'In-Circuit Test (ICT)';
end;

function TICTestHandler.GetProgressValue: integer;
begin
  { Get from factory - ICT is 5% }
  Result := 5;
end;

function TICTestHandler.GetLedControl: TindLed;
begin
  Result := FLedControl;
end;

procedure TICTestHandler.Execute(Serial: string; DebugLevel: string);
var
  Args: array[0..6] of string;
  RetVal: integer;
begin
  FLedControl.LedValue := False;
  
  if Serial = '' then
  begin
    FLastErrorCode := 1;
    FLastErrorMessage := 'Serial number is empty';
    exit;
  end;

  { Build command arguments }
  Args[0] := '-s';
  Args[1] := Trim(Serial);
  Args[2] := '-d';
  Args[3] := DebugLevel;
  Args[4] := '-b';
  Args[5] := 'new';  // Assume new for now, could be parameterized
  Args[6] := '';

  { Execute the command }
  { This would be refactored to use a ProcessExecutor }
  { RetVal := FTestRunner.RunM1Tfc('ict', Args, FLedControl); }
  
  FLastErrorCode := RetVal;
  if RetVal <> 0 then
    FLastErrorMessage := 'ICT test failed with code: ' + IntToStr(RetVal);
end;

function TICTestHandler.GetLastErrorCode: integer;
begin
  Result := FLastErrorCode;
end;

function TICTestHandler.GetLastErrorMessage: string;
begin
  Result := FLastErrorMessage;
end;

function TICTestHandler.IsRetestRequired: boolean;
begin
  Result := True;  { ICT is always required during retest }
end;

procedure TICTestHandler.Cancel;
begin
  { TODO: Implement cancellation logic }
end;

end.
