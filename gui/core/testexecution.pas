unit testexecution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, IndLed, testrunner, logger;

type
  { TTestExecutor }
  TTestExecutor = class
  private
    FRunner: TTestRunner;
    FTargetVendorSerial: TEdit;
    FMemo: TMemo;
    FTestMode: TestingMode;

  public
    constructor Create(Runner: TTestRunner; TargetSerial: TEdit; Memo: TMemo);
    destructor Destroy; override;

    function DoLabelSwitchClick(DebugLevel: string; BusyFlag: boolean;
      PrintError: boolean; var DoLabelSwitch: TindLed; TestStatus: boolean): integer;
    function FlashSwitchClick(DebugLevel: string; BusyFlag: boolean;
      DebugLevelStr: string; var FlashSwitch: TindLed): integer;
    function FuncTestSwitchClick(DebugLevel: string; BusyFlag: boolean;
      DebugLevelStr: string; var FuncTestSwitch: TindLed): integer;
    function ICTTestSwitchClick(DebugLevel: string; BusyFlag: boolean;
      DebugLevelStr: string; var ICTTestSwitch: TindLed; TestMode: TestingMode): integer;
    function AppsCheckSwitchClick(DebugLevel: string; BusyFlag: boolean;
      DebugLevelStr: string; var AppsCheckSwitch: TindLed; var EEPROMSwitch: TindLed): integer;
    function EEPROMSwitchClick(DebugLevel: string; BusyFlag: boolean;
      DebugLevelStr: string; var EEPROMSwitch: TindLed): integer;
    function MacProgSwitchClick(DebugLevel: string; BusyFlag: boolean;
      DebugLevelStr: string; var MacProgSwitch: TindLed): integer;

    procedure ExecuteTestWrapper(Sender: TObject; callback: TTestHandler);
    procedure DoLabelError(PrintError: boolean; var DoLabelSwitch: TindLed);

    property Runner: TTestRunner read FRunner;
  end;

  TTestHandler = procedure(Sender: TObject) of object;

implementation

constructor TTestExecutor.Create(Runner: TTestRunner; TargetSerial: TEdit; Memo: TMemo);
begin
  inherited Create;
  FRunner := Runner;
  FTargetVendorSerial := TargetSerial;
  FMemo := Memo;
  FTestMode := TestingMode.none;
end;

destructor TTestExecutor.Destroy;
begin
  inherited Destroy;
end;

function TTestExecutor.DoLabelSwitchClick(DebugLevel: string; BusyFlag: boolean;
  PrintError: boolean; var DoLabelSwitch: TindLed; TestStatus: boolean): integer;
var
  arg: array[0..8] of string;
begin
  DoLabelSwitch.LedValue := False;
  if BusyFlag then
    exit(FRunner.TestRet);

  arg[0] := '-s';
  arg[1] := Trim(FTargetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  if not PrintError then
    arg[4] := ''
  else
  begin
    arg[4] := '-e';
    arg[5] := '';
  end;

  FRunner.TestRet := FRunner.RunM1Tfc('makelabel', arg, DoLabelSwitch);
  Result := FRunner.TestRet;
end;

function TTestExecutor.FlashSwitchClick(DebugLevel: string; BusyFlag: boolean;
  DebugLevelStr: string; var FlashSwitch: TindLed): integer;
var
  arg: array[0..8] of string;
begin
  FlashSwitch.LedValue := False;
  if FTargetVendorSerial.Text = '' then
    exit(FRunner.TestRet);

  if BusyFlag then
    exit(FRunner.TestRet);
  arg[0] := '-s';
  arg[1] := Trim(FTargetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevelStr;
  arg[4] := '';
  FRunner.TestRet := FRunner.RunM1Tfc('flash', arg, FlashSwitch);
  Result := FRunner.TestRet;
end;

function TTestExecutor.FuncTestSwitchClick(DebugLevel: string; BusyFlag: boolean;
  DebugLevelStr: string; var FuncTestSwitch: TindLed): integer;
var
  arg: array[0..8] of string;
begin
  FuncTestSwitch.LedValue := False;
  if FTargetVendorSerial.Text = '' then
    exit(FRunner.TestRet);

  if BusyFlag then
    exit(FRunner.TestRet);
  arg[0] := '-s';
  arg[1] := Trim(FTargetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevelStr;
  arg[4] := '';
  FRunner.TestRet := FRunner.RunM1Tfc('functest', arg, FuncTestSwitch);
  Result := FRunner.TestRet;
end;

function TTestExecutor.ICTTestSwitchClick(DebugLevel: string; BusyFlag: boolean;
  DebugLevelStr: string; var ICTTestSwitch: TindLed; TestMode: TestingMode): integer;
var
  arg: array[0..6] of string;
  ret: integer;
begin
  ICTTestSwitch.LedValue := False;

  if BusyFlag then
    exit(FRunner.TestRet);
  arg[0] := '-s';
  arg[1] := Trim(FTargetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevelStr;
  arg[4] := '-b';
  if TestMode = TestingMode.re_test then
    arg[5] := 'used'
  else
    arg[5] := 'new';

  arg[6] := '';
  ret := FRunner.RunM1Tfc('ict', arg, ICTTestSwitch);
  Result := ret;
end;

function TTestExecutor.AppsCheckSwitchClick(DebugLevel: string; BusyFlag: boolean;
  DebugLevelStr: string; var AppsCheckSwitch: TindLed; var EEPROMSwitch: TindLed): integer;
var
  arg: array[0..10] of string;
begin
  AppsCheckSwitch.LedValue := False;

  if BusyFlag then
    exit(FRunner.TestRet);
  arg[0] := '-s';
  arg[1] := Trim(FTargetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevelStr;
  arg[4] := '';
  FRunner.TestRet := FRunner.RunM1Tfc('pingM1apps', arg, AppsCheckSwitch);
  Result := FRunner.TestRet;
end;

function TTestExecutor.EEPROMSwitchClick(DebugLevel: string; BusyFlag: boolean;
  DebugLevelStr: string; var EEPROMSwitch: TindLed): integer;
var
  arg: array[0..10] of string;
begin
  EEPROMSwitch.LedValue := False;
  if FTargetVendorSerial.Text = '' then
    exit(FRunner.TestRet);

  if BusyFlag then
    exit(FRunner.TestRet);
  arg[0] := '-s';
  arg[1] := Trim(FTargetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevelStr;
  arg[4] := '';
  FRunner.TestRet := FRunner.RunM1Tfc('eeprom', arg, EEPROMSwitch);
  Result := FRunner.TestRet;
end;

function TTestExecutor.MacProgSwitchClick(DebugLevel: string; BusyFlag: boolean;
  DebugLevelStr: string; var MacProgSwitch: TindLed): integer;
var
  arg: array[0..8] of string;
  retValue: integer;
begin
  MacProgSwitch.LedValue := False;
  if FTargetVendorSerial.Text = '' then
    exit(FRunner.TestRet);

  if BusyFlag then
    exit(-1);

  arg[0] := '-s';
  arg[1] := Trim(FTargetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevelStr;
  arg[4] := '';
  retValue := FRunner.RunM1Tfc('progmac', arg, MacProgSwitch);
  if (retValue = OtpIsNotBlank) or (retValue = NormalExit) then
  begin
    MacProgSwitch.LedValue := False;
    MacProgSwitch.LedColorOff := clLime;
    FRunner.TestRet := NormalExit;
  end
  else
  begin
    FRunner.TestRet := retValue;
  end;
  Result := retValue;
end;

procedure TTestExecutor.ExecuteTestWrapper(Sender: TObject; callback: TTestHandler);
begin
  if FRunner.DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;
  TindLed(Sender).LedValue := False;
  if not FRunner.CheckSerialBarcodeScan(FTargetVendorSerial.Text) then
    exit;
  
  { Validate callback before calling }
  if Assigned(callback) then
    callback(Sender);
end;

procedure TTestExecutor.DoLabelError(PrintError: boolean; var DoLabelSwitch: TindLed);
begin
  DoLabelSwitchClick('', FRunner.BusyFlag, PrintError, DoLabelSwitch, FRunner.TestStatus);
end;

end.
