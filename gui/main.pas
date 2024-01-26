unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, Buttons, ComCtrls, ActnList, MaskEdit, SpinEx, IndLed, BCMDButton,
  strutils, BCListBox, Process, logger, about, DateUtils,
  configurationjson, jsonparser, ColorProgress, MSSQLConn;

const
  ProcessTerminated = -9;
  NormalExit = 0;
  ProcessExecError = 1;
  precheckHWFailed = 15;
  OtpIsNotBlank = 10;
  EepromIsntBlank = 11;
  FW_Dir = '/home/lenel/m1mtf/stm32mp15-lenels2-m1/VERSION';
  App_Dir = '/home/lenel/m1mtf/';
  Interval7Days = (24 * 60 * 60 * 7);
  UpdateFwTimeStamp = 'UpdateFwTimeStamp.txt';
  UpdateSycretsTimeStamp = 'UpdateSycretsTimeStamp.txt';
  UpdateLogsTimeStamp = 'UpdateLogsTimeStamp.txt';

type
  TMethodPtr = procedure(Sender: TObject) of object;

type
  TestingMode = (none = 0, commission = 1, re_test = 2);

type
  TestRecord = record
    Name: string;
    led: ^TindLed;
    progressValue: integer;
    methodPtr: TMethodPtr;
    doRetest: boolean;
  end;

  pTestLed = ^TindLed;


type

  { TmainForm }

  TmainForm = class(TForm)
    PushSecretsMenuItem: TMenuItem;
    PushLogsMenuItem: TMenuItem;
    SyncFailedLabel: TLabel;
    Re_Test: TAction;
    StopTestClick: TAction;
    QuitClick: TAction;
    AppsCheckSwitch: TindLed;
    AbountMenuItem: TMenuItem;
    Re_TestMenuItem1: TMenuItem;
    CommissionClick: TAction;
    FakeLed: TindLed;
    EnableSerialNumber: TAction;
    DebugLevel2: TAction;
    DevModeLabel: TLabel;
    DebugLevel0: TAction;
    DebugLevel1: TAction;
    ActionList1: TActionList;
    ColorProgress1: TColorProgress;
    DoLabelSwitch: TindLed;
    EEPROMSwitch: TindLed;
    FlashSwitch: TindLed;
    FuncTestSwitch: TindLed;
    ICTTestSwitch: TindLed;
    Label1: TLabel;
    MacProgSwitch: TindLed;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    CloudMenuItem: TMenuItem;
    StartTestMenuItem: TMenuItem;
    StaticText12: TStaticText;
    StopTestMenu: TMenuItem;
    QuitMenuItem: TMenuItem;
    Panel2: TPanel;
    Panel1: TPanel;
    StaticText10: TStaticText;
    StaticText11: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    StaticText9: TStaticText;
    CheckCloudUpdateTimer: TTimer;
    targetVendorSerial: TEdit;
    LogsOpenDialog1: TOpenDialog;
    TestTumer: TTimer;
    SyncLelLedTimer: TTimer;
    SyncLabelLedTimer: TTimer;
    procedure AbountMenuItemClick(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure CheckCloudUpdateTimerTimer(Sender: TObject);
    procedure CloudMenuItemClick(Sender: TObject);
    procedure LogsMenuItemClick(Sender: TObject);
    procedure PushLogsMenuItemClick(Sender: TObject);
    procedure PushSecretsMenuItemClick(Sender: TObject);
    procedure Re_TestMenuItem1Click(Sender: TObject);
    procedure OpenLogExecute(Sender: TObject);
    procedure PublishLogMenuItemClick(Sender: TObject);
    procedure Panel1DblClick(Sender: TObject);
    function DoLabelSwitchClick(Sender: TObject): integer;
    function FlashSwitchClick(Sender: TObject): integer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    function FuncTestSwitchClick(Sender: TObject): integer;
    function ICTTestSwitchClick(Sender: TObject): integer;
    function EEPROMSwitchClick(Sender: TObject): integer;
    procedure EEPROMSwitchClick_Wrapper(Sender: TObject);
    function AppsCheckSwitchClick(Sender: TObject): integer;
    procedure InterruptMenuItemClick(Sender: TObject);
    procedure LedsTimer(Sender: TObject);
    function MacProgSwitchClick(Sender: TObject): integer;
    procedure Memo1DblClick(Sender: TObject);
    procedure Commission(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure Debuglevel_2_Execute(Sender: TObject);
    procedure Debuglevel_1_Execute(Sender: TObject);
    procedure Debuglevel_0_Execute(Sender: TObject);
    procedure FuncTestSwitchClick_Wrapper(Sender: TObject);
    procedure ICTTestSwitchClick_Wrapper(Sender: TObject);
    procedure MacProgSwitchClick_Wrapper(Sender: TObject);
    procedure DoLabelSwitchClick_Wrapper(Sender: TObject);
    procedure FlashSwitchClick_Wrapper(Sender: TObject);
    procedure AppsCheckSwitchClick_Wrapper(Sender: TObject);
    procedure SyncLabelLedTimerTimer(Sender: TObject);
    procedure UpdateMeMenuItemClick(Sender: TObject);
  private
    TestMode: TestingMode;
    AProcess: TProcess;
    testStatus: boolean;
    testRet: integer;
    printError: boolean;
    blinkMe: boolean;
    doOnes: boolean;
    busyFlag: boolean;
    configuration: TConfigration;
    Tests: array[0..6] of TestRecord;
    DebugLevel: string;

    function GetDateTimeFromFile(filename: string): TDateTime;
    function RunM1Tfc(command: string; arg: array of string; var Led: TindLed): integer;
    function CheckSerialBarcodeScan(serial: ansistring): boolean;
    procedure RunTests(tMode: TestingMode; modeStr: ansistring);
  public
    newData: string;
    procedure DoCleanupCmd();
    procedure ResetLeds;
    procedure AddToProgressBar(Value: integer);
    procedure ClearBusyFlag;
    procedure SetBusyFlag;
    procedure DoLabelError;
    procedure SetTestStatusFailed;
    procedure SetTestStatusOk;
    function MakeTestRecord(testName: ansistring; progressValue: integer;
      methodPtr: TMethodPtr; testLed: pTestLed; doRetest: boolean): TestRecord;
  end;

type
  BarcodeException = class(Exception);

var
  mainForm: TmainForm;

implementation


{$R *.lfm}

{ TmainForm }

function TmainForm.GetDateTimeFromFile(filename: string): TDateTime;
var
  myDateTimeVariable: TDateTime;
  FS: TFormatSettings;
  dateTimeFromFile: string;
  filePath: string;
  stringList: TStringList;
begin
  dateTimeFromFile := '2023-01-24 21:20:08';
  stringList := TStringList.Create;
  try
    filePath := App_Dir + filename;
    stringList.LoadFromFile(filePath);
    dateTimeFromFile := stringList.Text;
  except
    on E: Exception do dateTimeFromFile := '2023-01-24 21:20:08';
  end;
  stringList.Free;
  FS := DefaultFormatSettings;
  FS.DateSeparator := '-';
  FS.ShortDateFormat := 'yyyy-mm-dd';
  FS.ShortTimeFormat := 'hh:mm:ss';
  myDateTimeVariable := strtodatetime(dateTimeFromFile, FS);
  Result := myDateTimeVariable;
end;


procedure TmainForm.FormCreate(Sender: TObject);
var
  pid: string;
begin
  printError := False;
  testStatus := True;
  ClearBusyFlag;
  doOnes := True;
  configuration := ConfigurationGet;
  SyncFailedLabel.Font.Color := clRed;
  Tests[0] := MakeTestRecord('ICT', 5, TMethodPtr(@ICTTestSwitchClick),
    @ICTTestSwitch, True);
  Tests[1] := MakeTestRecord('MAC', 3, TMethodPtr(@MacProgSwitchClick),
    @MacProgSwitch, True);
  Tests[2] := MakeTestRecord('Flash', 40, TMethodPtr(@FlashSwitchClick),
    @FlashSwitch, False);
  Tests[3] := MakeTestRecord('Func', 43, TMethodPtr(@FuncTestSwitchClick),
    @FuncTestSwitch, True);
  Tests[4] := MakeTestRecord('EEPROM', 5, TMethodPtr(@EEPROMSwitchClick),
    @EEPROMSwitch, True);
  Tests[5] := MakeTestRecord('Apps', 3, TMethodPtr(@AppsCheckSwitchClick),
    @AppsCheckSwitch, True);
  Tests[6] := MakeTestRecord('Label', 8, TMethodPtr(@DoLabelSwitchClick),
    @DoLabelSwitch, True);

  SyncLelLedTimer.Enabled := True;
  debugLevel := GetEnvironmentVariable('m1tfdebug');
  if debugLevel <> '1' then DebugLevel := '0';
  Memo1.Font.Size := 12;

  pid := IntToStr(system.GetProcessID);
  with TStringList.Create do
    try
      Add(pid);
      SaveToFile(GetEnvironmentVariable('HOME') + '/m1mtf/m1tfd1app.pid');
    finally
      Free;
    end;
end;

procedure TmainForm.SetTestStatusFailed;
begin
  testStatus := False;
end;

procedure TmainForm.SetTestStatusOk;
begin
  testStatus := True;
end;

procedure TmainForm.SetbusyFlag;
begin
  busyFlag := True;
end;

procedure TmainForm.ClearbusyFlag;
begin
  busyFlag := False;
end;

procedure TmainForm.AddToProgressBar(Value: integer);
var
  progress: integer;
begin
  progress := ColorProgress1.Progress;
  progress := progress + Value;
  if progress > 100 then progress := 100;
  ColorProgress1.Progress := progress;
end;

procedure TmainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  InterruptMenuItemClick(self);
  Application.ProcessMessages;
end;

function TmainForm.DoLabelSwitchClick(Sender: TObject): integer;
var
  arg: array[0..8] of string;
begin
  DoLabelSwitch.LedValue := False;
  // if targetVendorSerial.Text = '' then exit;
  if busyFlag then
  begin
    DoLabelSwitch.LedValue := False;
    exit(testRet);
    ;
  end;

  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  if not printError then arg[4] := ''
  else
  begin
    arg[4] := '-e';
    arg[5] := '';
  end;

  testRet := RunM1Tfc('makelabel', arg, DoLabelSwitch);
  Result := testRet;
end;

procedure TmainForm.DoLabelError();
begin
  printError := True;
  DoLabelSwitchClick(self);
  printError := False;
end;

procedure TmainForm.DoCleanupCmd();
var
  arg: array[0..8] of string;
begin
  DoLabelSwitch.LedValue := False;
  if targetVendorSerial.Text = '' then exit;
  if busyFlag then
  begin
    DoLabelSwitch.LedValue := False;
    exit;
  end;

  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  if (testStatus) then
  begin
    arg[2] := '';
  end
  else
  begin
    arg[2] := '-e';
    arg[3] := '';
  end;

  RunM1Tfc('cleanup', arg, fakeLed);
end;

procedure TmainForm.Debuglevel_0_Execute(Sender: TObject);
begin
  // targetVendorSerial.ReadOnly := True;
  DevModeLabel.Visible := False;
  DevModeLabel.Caption := 'D0';
  DevModeLabel.Font.color := clRed;
  DebugLevel := '0';
end;

procedure TmainForm.Debuglevel_1_Execute(Sender: TObject);
begin
  DevModeLabel.Visible := True;
  DevModeLabel.Caption := 'D1';
  DevModeLabel.Font.color := clRed;
  DebugLevel := '1';
end;

procedure TmainForm.Debuglevel_2_Execute(Sender: TObject);
begin
  DevModeLabel.Visible := True;
  DevModeLabel.Caption := 'D2';
  DevModeLabel.Font.color := clRed;
  DebugLevel := '2';
end;

function TmainForm.CheckSerialBarcodeScan(serial: ansistring): boolean;
var
  str: ansistring;
  intN: integer;
  ret: ansistring;
begin
  serial := targetVendorSerial.Text;
  ret := '?';
  try
    str := AnsiMidStr(serial, 1, 2);
    if str = '30' then
    begin
      ret := 'M1-3200';
    end
    else
      raise BarcodeException.Create('Invalid Barcode Scan');

    str := AnsiMidStr(serial, 3, 2);
    intN := StrToInt(str);
    if (intN > 22) and (intN < 47) then
    begin
      ret += ' Y-20' + str;
    end
    else
      raise BarcodeException.Create('Invalid Barcode Scan');

    str := AnsiMidStr(serial, 5, 2);
    intN := StrToInt(str);
    if (intN <= 53) and (intN >= 0) then
    begin
      ret += ' W-' + str;
    end
    else
      raise BarcodeException.Create('Invalid Barcode Scan');

    str := AnsiMidStr(serial, 7, 4);
    intN := StrToInt(str);
    if (intN <= 9999) and (Length(str) = 4) then
    begin
      ret += ' S-' + str;
    end
    else
      raise BarcodeException.Create('Invalid Barcode Scan');
  except
    on e: BarcodeException do
    begin
      ShowMessage('Invalid Barcode Scan');
      Result := False;
      exit(False);
    end;

  end;
  MainForm.Text := ret;
  Result := True;
end;

procedure TmainForm.AbountMenuItemClick(Sender: TObject);
begin
  aboutForm.ShowModal;
end;

procedure TmainForm.Action2Execute(Sender: TObject);
begin

end;

procedure TmainForm.CheckCloudUpdateTimerTimer(Sender: TObject);
var
  epochTime: int64;
  epochTimeNow: int64;
  dateTimeNow : TDateTime;
  dateTimeFromFile : TDateTime;
begin
  blinkMe := False;
  dateTimeNow := Now;
  epochTimeNow := DateTimeToUnix(dateTimeNow, False);
  // dateTimeFromFile := GetDateTimeFromFile(UpdateFwTimeStamp);
  // epochTime := DateTimeToUnix(dateTimeFromFile);
  // if (epochTimeNow - epochTime) > Interval7Days then blinkMe := True;
  epochTime := DateTimeToUnix(GetDateTimeFromFile(UpdateSycretsTimeStamp));
  if (epochTimeNow - epochTime) > Interval7Days then blinkMe := True;
  epochTime := DateTimeToUnix(GetDateTimeFromFile(UpdateLogsTimeStamp));
  if (epochTimeNow - epochTime) > Interval7Days then blinkMe := True;

end;

procedure TmainForm.CloudMenuItemClick(Sender: TObject);
begin

end;

procedure TmainForm.LogsMenuItemClick(Sender: TObject);
var
  homeEnv: string;
  envVarName: string;
begin
  envVarName := 'HOME';
  homeEnv := GetEnvironmentVariable(envVarName);
  LogsOpenDialog1.InitialDir := homeEnv + DirectorySeparator + 'm1mtf/logs';
  if LogsOpenDialog1.Execute then
  begin
    Memo1.Lines.LoadFromFile(LogsOpenDialog1.Filename);
  end;
end;

procedure TmainForm.PushLogsMenuItemClick(Sender: TObject);
const
  bufferSize = 1024 * 16;
var
  aLocalProcess: TProcess;
  Buffer: array[0..bufferSize] of byte;
  BytesRead: longint;
  textToSee: ansistring;
begin
  aLocalProcess := TProcess.Create(nil);
  aLocalProcess.Executable := 'sudo';
  aLocalProcess.Parameters.Add('/snap/bin/m1client');
  aLocalProcess.Parameters.Add('synclogs');
  aLocalProcess.Options := aLocalProcess.Options + [poUsePipes];
  aLocalProcess.Execute;

  while aLocalProcess.Running do
  begin
    Sleep(50);
    continue;
  end;

  Buffer[0] := 0;
  BytesRead := aLocalProcess.Output.NumBytesAvailable;
  if BytesRead > (bufferSize - 1) then
  begin
    exit;
  end;
  BytesRead := aLocalProcess.Output.Read(Buffer, BytesRead);
  if BytesRead >= bufferSize then Buffer[bufferSize - 1] := 0
  else
    Buffer[BytesRead] := 0;

  Sleep(50);
  textToSee := '';
  SetString(textToSee, pansichar(@Buffer[0]), BytesRead);
  Memo1.Lines.Add(logger.log('info', 'pushlogs', textToSee));
  aLocalProcess.Free;
  aLocalProcess := nil;
end;

procedure TmainForm.PushSecretsMenuItemClick(Sender: TObject);
const
  bufferSize = 1024 * 16;
var
  aLocalProcess: TProcess;
  Buffer: array[0..bufferSize] of byte;
  BytesRead: longint;
  textToSee: ansistring;
begin
  aLocalProcess := TProcess.Create(nil);
  aLocalProcess.Executable := 'sudo';
  aLocalProcess.Parameters.Add('/snap/bin/m1client');
  aLocalProcess.Parameters.Add('syncsecrets');
  aLocalProcess.Options := aLocalProcess.Options + [poUsePipes];
  aLocalProcess.Execute;

  while aLocalProcess.Running do
  begin
    Sleep(50);
    continue;
  end;

  Buffer[0] := 0;
  BytesRead := aLocalProcess.Output.NumBytesAvailable;
  if BytesRead > (bufferSize - 1) then
  begin
    exit;
  end;
  BytesRead := aLocalProcess.Output.Read(Buffer, BytesRead);
  if BytesRead >= bufferSize then Buffer[bufferSize - 1] := 0
  else
    Buffer[BytesRead] := 0;

  Sleep(50);
  textToSee := '';
  SetString(textToSee, pansichar(@Buffer[0]), BytesRead);
  Memo1.Lines.Add(logger.log('info', 'pushlogs', textToSee));
  aLocalProcess.Free;
  aLocalProcess := nil;
end;

procedure TmainForm.Re_TestMenuItem1Click(Sender: TObject);
begin
  if busyFlag then exit;
  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Memo1.Clear;
  Memo1.Lines.Add(log('info', targetVendorSerial.Text, 'Re-Test board'));
  ResetLeds;
  ColorProgress1.progress := 0;
  RunTests(TestingMode.re_test, 'Re-test');
end;

procedure TmainForm.OpenLogExecute(Sender: TObject);
begin
  LogsMenuItemClick(Sender);
end;

procedure TmainForm.PublishLogMenuItemClick(Sender: TObject);
const
  bufferSize = 1024 * 16;
var
  aLocalProcess: TProcess;
  Buffer: array[0..bufferSize] of byte;
  BytesRead: longint;
  textToSee: ansistring;
begin
  aLocalProcess := TProcess.Create(nil);
  aLocalProcess.Executable := '/snap/bin/m1client';
  aLocalProcess.Parameters.Add('synclogs');
  aLocalProcess.Options := aLocalProcess.Options + [poUsePipes];
  aLocalProcess.Execute;

  while aLocalProcess.Running do
  begin
    Sleep(50);
    continue;
  end;

  Buffer[0] := 0;
  BytesRead := aLocalProcess.Output.NumBytesAvailable;
  if BytesRead > (bufferSize - 1) then
  begin
    exit;
  end;
  BytesRead := aLocalProcess.Output.Read(Buffer, BytesRead);
  if BytesRead >= bufferSize then Buffer[bufferSize - 1] := 0
  else
    Buffer[BytesRead] := 0;

  Sleep(50);
  textToSee := '';
  SetString(textToSee, pansichar(@Buffer[0]), BytesRead);
  Memo1.Lines.Add(logger.log('info', 'pushlogs', textToSee));
  aLocalProcess.Free;
  aLocalProcess := nil;
end;

procedure TmainForm.Panel1DblClick(Sender: TObject);
begin
  ColorProgress1.Progress := 0;
  Memo1DblClick(Sender);
  ResetLeds();
end;

function TmainForm.FlashSwitchClick(Sender: TObject): integer;
var
  arg: array[0..8] of string;
begin
  FlashSwitch.LedValue := False;
  if targetVendorSerial.Text = '' then exit;

  if busyFlag then
  begin
    exit;
  end;
  with TStringList.Create do
    try
      LoadFromFile(FW_Dir);
      Memo1.Lines.Add(logger.log('info', 'eMMC', 'Programming FW Rev: ' + Text));
    finally
      Free;
    end;

  FlashSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  testRet := RunM1Tfc('flash', arg, FlashSwitch);
  Result := testRet;
end;

function TmainForm.MakeTestRecord(testName: ansistring; progressValue: integer;
  methodPtr: TMethodPtr; testLed: pTestLed; doRetest: boolean): TestRecord;
begin
  Result.Name := testName;
  Result.led := testLed;
  Result.progressValue := progressValue;
  Result.methodPtr := methodPtr;
  Result.doRetest := doRetest;
end;


function TmainForm.FuncTestSwitchClick(Sender: TObject): integer;
var
  arg: array[0..8] of string;
begin
  FuncTestSwitch.LedValue := False;
  if targetVendorSerial.Text = '' then exit(testRet);
  ;

  if busyFlag then
  begin
    exit(testRet);
    ;
  end;
  FuncTestSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  testRet := RunM1Tfc('functest', arg, FuncTestSwitch);
  Result := testRet;
end;

procedure TmainForm.ResetLeds;
var
  test: TestRecord;
begin
  for test in Tests do
  begin
    test.led^.LedColorOff := clGray;
  end;
end;

function TmainForm.ICTTestSwitchClick(Sender: TObject): integer;
var
  arg: array[0..6] of string;
var
  ret: integer;
begin
  ICTTestSwitch.LedValue := False;

  if busyFlag then
  begin
    exit(testRet);
    ;
  end;

  ICTTestSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '-b';
  if TestMode = TestingMode.re_test then arg[5] := 'used'
  else
    arg[5] := 'new';

  arg[6] := '';
  ret := RunM1Tfc('ict', arg, ICTTestSwitch);
  Result := ret;
end;

procedure TmainForm.InterruptMenuItemClick(Sender: TObject);
begin
  if (aProcess <> nil) and (aProcess.Running) then
  begin
    aProcess.Terminate(100);
    Memo1.Lines.Add(log('warn', targetVendorSerial.Text, 'Terminated'));
  end;
end;

function TmainForm.AppsCheckSwitchClick(Sender: TObject): integer;
var
  arg: array[0..10] of string;
begin
  AppsCheckSwitch.LedValue := False;

  if busyFlag then
  begin
    exit(testRet);
  end;

  EEPROMSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  testRet := RunM1Tfc('pingM1apps', arg, AppsCheckSwitch);
  Result := testRet;
end;

function TmainForm.EEPROMSwitchClick(Sender: TObject): integer;
var
  arg: array[0..10] of string;
begin
  EEPROMSwitch.LedValue := False;
  if targetVendorSerial.Text = '' then exit(testRet);

  if busyFlag then
  begin
    exit(testRet);
  end;

  EEPROMSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  testRet := RunM1Tfc('eeprom', arg, EEPROMSwitch);
  Result := testRet;
end;

procedure TmainForm.LedsTimer(Sender: TObject);
var
  test: TestRecord;
begin
  if SyncLelLedTimer.Tag = 1 then
  begin
    SyncLelLedTimer.Tag := 0;
    exit;
  end;
  for test in Tests do
  begin
    if test.led^.tag = 1 then
    begin
      if not test.led^.LedValue then
      begin
        if test.led^.LedColorOff = clYellow then test.led^.LedColorOff := clGray
        else
          test.led^.LedColorOff := clYellow;
      end;

    end;
  end;
end;

function TmainForm.RunM1Tfc(command: string; arg: array of string;
  var Led: TindLed): integer;
const
  bufferSize = 1024 * 64;
var
  BytesRead: longint;
  retValue: integer;
  Buffer: array[0..bufferSize] of byte;
  textToSee: ansistring;
  tmpInt: integer;
  MemoCopyTxt: string;
begin
  if command = 'cleanup' then
  begin

    Led.tag := 1;
  end;
  MemoCopyTxt := '';
  Led.tag := 1;
  retValue := -1;
  SetBusyFlag;
  AProcess := TProcess.Create(nil);
  AProcess.Executable := 'm1tfd1.cli';
  AProcess.Parameters.Add(command);
  tmpInt := 0;
  // Memo1.Lines.Add(logger.log('info', command, 'Executing <' + command + '>'));
  while arg[tmpInt] <> '' do
  begin
    AProcess.Parameters.Add(arg[tmpInt]);
    Inc(tmpInt);
  end;

  AProcess.Options := AProcess.Options + [poUsePipes];
  AProcess.Execute;
  while aProcess.Running do
  begin
    Buffer[0] := 0;
    BytesRead := AProcess.Output.Read(Buffer, AProcess.Output.NumBytesAvailable);
    if BytesRead >= bufferSize then Buffer[bufferSize - 1] := 0
    else
      Buffer[BytesRead] := 0;
    if BytesRead = 0 then
    begin
      Application.ProcessMessages;
      Sleep(50);
      continue;
    end;
    Sleep(50);
    textToSee := '';
    SetString(textToSee, pansichar(@Buffer[0]), BytesRead);
    Memo1.Lines.Text := Memo1.Lines.Text + textToSee;
    MemoCopyTxt := MemoCopyTxt + textToSee;
    Memo1.SelStart := Length(Memo1.Lines.Text);
    Application.ProcessMessages;
  end;
  BytesRead := AProcess.Stderr.Read(Buffer, AProcess.Stderr.NumBytesAvailable);
  if BytesRead > 0 then
  begin
    textToSee := '';
    SetString(textToSee, pansichar(@Buffer[0]), BytesRead);
    Memo1.Lines.Text := Memo1.Lines.Text + textToSee;
  end;

  if AProcess.ExitStatus <> 0 then retValue := AProcess.ExitStatus
  else
    retValue := AProcess.ExitCode;

  if (retValue = ProcessExecError) then
  begin
    Memo1.Lines.Add('Abnormal termination nodejs executable not found or crashed');
    Led.LedColorOff := clRed;
  end;
  if (retValue > ProcessExecError) then
  begin
    Led.LedColorOff := clRed;
    InterruptMenuItemClick(self);
    Led.Tag := 0;
  end;
  AProcess.Free;
  AProcess := nil;
  Led.tag := 0;
  Led.LedValue := False;
  if (retValue = 0) then
  begin
    Led.LedColorOff := clLime;
  end;
  if retValue = ProcessTerminated then
  begin
    Led.LedColorOff := clRed;
    Led.Tag := 0;
  end;

  Result := retValue;
  testRet := Result;
  ClearBusyFlag;
end;

function TmainForm.MacProgSwitchClick(Sender: TObject): integer;
var
  arg: array[0..8] of string;
  retValue: integer;
begin
  MacProgSwitch.LedValue := False;
  if targetVendorSerial.Text = '' then exit(testRet);
  ;
  if busyFlag then
  begin
    exit(-1);
  end;

  MacProgSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  retValue := RunM1Tfc('progmac', arg, MacProgSwitch);
  if (retValue = OtpIsNotBlank) or (retValue = normalExit) then
  begin
    MacProgSwitch.LedValue := False;
    MacProgSwitch.LedColorOff := clLime;
    testRet := normalExit;
  end
  else
  begin
    testRet := retValue;
  end;
  Result := retValue;
end;

procedure TmainForm.Memo1DblClick(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TmainForm.Commission(Sender: TObject);
begin
  if busyFlag then exit;
  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Memo1.Clear;
  Memo1.Lines.Add(log('info', targetVendorSerial.Text, 'Commission board'));
  ResetLeds;
  ColorProgress1.progress := 0;
  RunTests(TestingMode.commission, 'commission');
end;

procedure TmainForm.QuitMenuItemClick(Sender: TObject);
begin
  InterruptMenuItemClick(Sender);
  Application.ProcessMessages;
  Application.Terminate;
end;

procedure TmainForm.FuncTestSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;
  TindLed(Sender).LedValue := False;
  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Panel1DblClick(Sender);
  FuncTestSwitchClick(Sender);
end;

procedure TmainForm.ICTTestSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;
  TindLed(Sender).LedValue := False;
  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Panel1DblClick(Sender);
  ICTTestSwitchClick(Sender);
end;

procedure TmainForm.AppsCheckSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;
  TindLed(Sender).LedValue := False;
  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Panel1DblClick(Sender);
  AppsCheckSwitchClick(Sender);
end;

procedure TmainForm.SyncLabelLedTimerTimer(Sender: TObject);
begin
  if not blinkMe then
  begin
    SyncFailedLabel.Visible := False;
    exit;
  end;

  if SyncFailedLabel.Visible = False then SyncFailedLabel.Visible := True
  else
    SyncFailedLabel.Visible := False;

end;

procedure TmainForm.UpdateMeMenuItemClick(Sender: TObject);
const
  bufferSize = 1024 * 16;
var
  aLocalProcess: TProcess;
  Buffer: array[0..bufferSize] of byte;
  BytesRead: longint;
  textToSee: ansistring;
begin
  aLocalProcess := TProcess.Create(nil);
  aLocalProcess.Executable := '/snap/bin/m1client';
  aLocalProcess.Parameters.Add('update');
  aLocalProcess.Options := aLocalProcess.Options + [poUsePipes];
  aLocalProcess.Execute;

  while aLocalProcess.Running do
  begin
    Sleep(50);
    continue;
  end;

  Buffer[0] := 0;
  BytesRead := aLocalProcess.Output.NumBytesAvailable;
  if BytesRead > (bufferSize - 1) then
  begin
    exit;
  end;
  BytesRead := aLocalProcess.Output.Read(Buffer, BytesRead);
  if BytesRead >= bufferSize then Buffer[bufferSize - 1] := 0
  else
    Buffer[BytesRead] := 0;

  Sleep(50);
  textToSee := '';
  SetString(textToSee, pansichar(@Buffer[0]), BytesRead);
  Memo1.Lines.Add(logger.log('info', 'pushlogs', textToSee));
  aLocalProcess.Free;
  aLocalProcess := nil;
end;

procedure TmainForm.EEPROMSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;

  TindLed(Sender).LedValue := False;
  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Panel1DblClick(Sender);
  EEPROMSwitchClick(Sender);
end;

procedure TmainForm.MacProgSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;

  TindLed(Sender).LedValue := False;
  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Panel1DblClick(Sender);
  MacProgSwitchClick(Sender);
end;

procedure TmainForm.DoLabelSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;

  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;
  Panel1DblClick(Sender);
  DoLabelSwitchClick(Sender);
end;

procedure TmainForm.FlashSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;

  TindLed(Sender).LedValue := False;
  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  if TargetVendorSerial.Text = '' then
  begin
    ShowMessage('Barcode Scan is Missing');
  end;
  Panel1DblClick(Sender);
  FlashSwitchClick(Sender);
end;

procedure TmainForm.RunTests(tMode: TestingMode; modeStr: ansistring);
var
  test: TestRecord;
  testReturnStatus: integer;
begin
  TestMode := tMode;
  ResetLeds;
  for test in Tests do
  begin
    testRet := NormalExit;
    if TestMode <> TestingMode.commission then
    begin
      if not test.doRetest then
      begin
        AddToProgressBar(test.progressValue);
        continue;
      end;
    end;
    test.methodPtr(self);
    AddToProgressBar(test.progressValue);
    testReturnStatus := testRet;
    if testReturnStatus <> NormalExit then
    begin
      // Memo1.Lines.Add(logger.log('info', modeStr, 'Test return status - false'));
      break;
    end;
    // testReturnStatus is global since method is procedure

  end;

  { if TestMode = TestingMode.commission then } DoCleanupCmd;

  if (testReturnStatus <> NormalExit) and (testReturnStatus <> ProcessTerminated) then
  begin
    if  testReturnStatus <> precheckHWFailed then DoLabelError;
    resetLeds;
  end
  else
  if (testReturnStatus = NormalExit) then
    Memo1.Lines.Add(logger.log('info', modeStr, 'Success! All Done'));
  TestMode := TestingMode.none;
end;

end.
