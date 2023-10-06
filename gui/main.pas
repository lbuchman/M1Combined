unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, Buttons, ComCtrls, ActnList, MaskEdit, SpinEx, IndLed, BCMDButton,
  strutils, BCListBox, BCTrackbarUpdown, DTAnalogGauge, dtthemedgauge,
  DTAnalogClock, dtthemedclock, Process, logger, about,
  configurationjson, jsonparser, macUtils, provision, ColorProgress, MSSQLConn;

const
  NormalExit = 0;
  ConfigFileMissing = 1;
  MacMissing = 2;
  CommandFailed = 3;
  IctTestFailed = 4;
  TestPointTestaile = 5;
  ProgramEepromFailed = 6;
  ProgramMacFailed = 7;
  TamperSensorTestFailed = 8;
  ConfigVendorSiteMissing = 9;
  OtpIsNotBlank = 10;
  EepromIsntBlank = 11;

  LogHeight = 850;
  DefaultHeight = 411;

type

  { TmainForm }

  TmainForm = class(TForm)
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
    PublishLogMenuItem: TMenuItem;
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
    targetVendorSerial: TEdit;
    LogsOpenDialog1: TOpenDialog;
    TestTumer: TTimer;
    LedTimer: TTimer;
    procedure AbountMenuItemClick(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure BarcodeScanEditTimerTimer(Sender: TObject);
    procedure EnableSerialNumberExecute(Sender: TObject);
    procedure LogsMenuItemClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Re_TestMenuItem1Click(Sender: TObject);
    procedure Re_TestMenuItemClick(Sender: TObject);
    procedure OpenLogExecute(Sender: TObject);
    procedure PublishLogMenuItemClick(Sender: TObject);
    procedure Panel1DblClick(Sender: TObject);
    procedure DoLabelSwitchClick(Sender: TObject);
    procedure FlashSwitchClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FuncTestSwitchClick(Sender: TObject);
    procedure ICTTestSwitchClick(Sender: TObject);
    procedure EEPROMSwitchClick(Sender: TObject);
    procedure AppsCheckSwitchClick(Sender: TObject);
    procedure InterruptMenuItemClick(Sender: TObject);
    procedure LedTimerTimer(Sender: TObject);
    procedure MacProgSwitchClick(Sender: TObject);
    procedure Memo1DblClick(Sender: TObject);
    procedure Commission(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure Debuglevel_2_Execute(Sender: TObject);
    procedure Debuglevel_1_Execute(Sender: TObject);
    procedure Debuglevel_0_Execute(Sender: TObject);
    procedure FuncTestSwitchClick_Wrapper(Sender: TObject);
    procedure ICTTestSwitchClick_Wrapper(Sender: TObject);
    procedure EEPROMSwitchClick_Wrapper(Sender: TObject);
    procedure MacProgSwitchClick_Wrapper(Sender: TObject);
    procedure DoLabelSwitchClick_Wrapper(Sender: TObject);
    procedure FlashSwitchClick_Wrapper(Sender: TObject);
    procedure AppsCheckwitchClick_Wrapper(Sender: TObject);
  private
    AProcess: TProcess;
    Uid: string;
    testStatus: boolean;
    printError: boolean;
    doOnes: boolean;
    provisionThread: TProvisionThread;
    busyFlag: boolean;
    busyFlag1: boolean;
    previouseScanInput: string;
    configuration: TConfigration;
    Leds: array[0..6] of ^TindLed;
    MemoCopyTxt: string;
    DebugLevel: string;
    function RunM1Tfc(command: string; arg: array of string; var Led: TindLed): integer;
    function CheckSerialBarcodeScan(serial: ansistring): boolean;
  public
    newData: string;
    procedure DoCleanupCmd();
    procedure UpdateTargetSerial();
    procedure ICTTestSwitchClick_;
    procedure MacProgSwitchClick_;
    procedure FlashSwitchClick_;
    procedure FuncTestSwitchClick_;
    procedure EEPROMSwitchClick_;
    procedure AppsCheckSwitchClick_;
    procedure DoLabelSwitchClick_;
    procedure ResetLeds;
    procedure AddToProgressBar(Value: integer);
    procedure Add5ToProgressBar;
    procedure Add3ToProgressBar;
    procedure Add10ToProgressBar;
    procedure Add15ToProgressBar;
    procedure Add20ToProgressBar;
    procedure Add25ToProgressBar;
    procedure Add30ToProgressBar;
    procedure Add40ToProgressBar;
    procedure ClearbusyFlag1;
    procedure DoLabelError;
    procedure SetTestStatusFailed;
    procedure SetTestStatusOk;
  end;

type
  BarcodeException = class(Exception);

var
  mainForm: TmainForm;

implementation

{$R *.lfm}

{ TmainForm }

procedure TmainForm.SetTestStatusFailed;
begin
  testStatus := False;
end;

procedure TmainForm.SetTestStatusOk;
begin
  testStatus := True;
end;

procedure TmainForm.ClearbusyFlag1;
begin
  busyFlag1 := False;
end;

procedure TmainForm.Add5ToProgressBar;
begin
  AddToProgressBar(5);
end;

procedure TmainForm.Add10ToProgressBar;
begin
  AddToProgressBar(10);
end;

procedure TmainForm.Add15ToProgressBar;
begin
  AddToProgressBar(15);
end;

procedure TmainForm.Add3ToProgressBar;
begin
  AddToProgressBar(3);
end;

procedure TmainForm.Add20ToProgressBar;
begin
  AddToProgressBar(20);
end;

procedure TmainForm.Add25ToProgressBar;
begin
  AddToProgressBar(25);
end;

procedure TmainForm.Add30ToProgressBar;
begin
  AddToProgressBar(30);
end;

procedure TmainForm.Add40ToProgressBar;
begin
  AddToProgressBar(40);
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
  provisionThread.Terminate;
  provisionThread.ExecuteThread;
  provisionThread.WaitFor;
  provisionThread.Free;
end;

procedure TmainForm.DoLabelSwitchClick_();
begin
  DoLabelSwitchClick(self);
end;

procedure TmainForm.DoLabelSwitchClick(Sender: TObject);
var
  arg: array[0..8] of string;
begin
  DoLabelSwitch.LedValue := False;
  // if targetVendorSerial.Text = '' then exit;
  if busyFlag1 then
  begin
    DoLabelSwitch.LedValue := False;
    exit;
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

  RunM1Tfc('makelabel', arg, DoLabelSwitch);
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
  if busyFlag1 then
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

procedure TmainForm.EnableSerialNumberExecute(Sender: TObject);
begin
  // targetVendorSerial.ReadOnly := False;
  // BarcodeScanEditTimer.Enabled := True;
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
    if intN <= 1000 then
    begin
      ret += ' S-' + str;
    end
    else
      raise BarcodeException.Create('Invalid Barcode Scan');
  except
    on e: BarcodeException do
    begin
      ShowMessage('Invalid Barcode Scan');
      result := False;
      exit(false);
    end;

  end;
  MainForm.Text := ret;
  result := True;
end;

procedure TmainForm.BarcodeScanEditTimerTimer(Sender: TObject);
var
  serial: ansistring;
  str: ansistring;
  intN: integer;
  ret: ansistring;
begin
  serial := targetVendorSerial.Text;
  try
    str := AnsiMidStr(serial, 1, 2);
    case str of
      '30': ret := 'M1-3200';
    end;

    str := AnsiMidStr(serial, 3, 2);
    intN := StrToInt(str);
    if (intN > 22) and (intN < 47) then
    begin
      ret += ' Y-20' + str;
    end;

    str := AnsiMidStr(serial, 5, 2);
    intN := StrToInt(str);
    if (intN <= 53) and (intN >= 0) then
    begin
      ret += ' W-' + str;
    end;

    str := AnsiMidStr(serial, 7, 4);
    intN := StrToInt(str);
    if intN <= 1000 then
    begin
      ret += ' S-' + str;
    end;
  except
    On E: EConvertError do
    begin

    end;

  end;
  MainForm.Text := ret;

end;

procedure TmainForm.AbountMenuItemClick(Sender: TObject);
begin
  aboutForm.ShowModal;
end;

procedure TmainForm.Action2Execute(Sender: TObject);
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

procedure TmainForm.Memo1Change(Sender: TObject);
begin

end;

procedure TmainForm.Re_TestMenuItem1Click(Sender: TObject);
begin
  if busyFlag1 then exit;

  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Memo1.Clear;
  ResetLeds;
  Memo1.Lines.Add(log('info', targetVendorSerial.Text, 'Re-test board'));
  ColorProgress1.progress := 0;
  provisionThread.reTestMode := True;
  provisionThread.ExecuteThread;
end;

procedure TmainForm.Re_TestMenuItemClick(Sender: TObject);
begin

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
  tmpStr: string;
  splitOutput: TStringArray;
  splitLine: TStringArray;
  Count: integer;
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

procedure TmainForm.FlashSwitchClick(Sender: TObject);
var
  arg: array[0..8] of string;
begin
  FlashSwitch.LedValue := False;
  if targetVendorSerial.Text = '' then exit;

  if busyFlag1 then
  begin
    exit;
  end;

  FlashSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  RunM1Tfc('flash', arg, FlashSwitch);

end;

procedure TmainForm.FormCreate(Sender: TObject);
var
  pid: string;
  ipaddresses: string;
begin
  printError := False;
  testStatus := True;
  busyFlag := False;
  busyFlag1 := False;
  doOnes := True;
  configuration := ConfigurationGet;
  provisionThread := TProvisionThread.Create(False);
  // Leds[0] := @ProvisionSwitch;
  Leds[0] := @ICTTestSwitch;
  Leds[1] := @MacProgSwitch;
  Leds[2] := @FlashSwitch;
  Leds[3] := @FuncTestSwitch;
  Leds[4] := @EEPROMSwitch;
  Leds[5] := @DoLabelSwitch;
  Leds[6] := @AppsCheckSwitch;
  LedTimer.Enabled := True;
  DebugLevel := '1';
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

procedure TmainForm.FormShow(Sender: TObject);
begin
  DebugLevel := '1';
end;

procedure TmainForm.FuncTestSwitchClick(Sender: TObject);
var
  arg: array[0..8] of string;
begin
  FuncTestSwitch.LedValue := False;
  if targetVendorSerial.Text = '' then exit;

  if busyFlag1 then
  begin
    exit;
  end;
  FuncTestSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  RunM1Tfc('functest', arg, FuncTestSwitch);

end;

procedure TmainForm.UpdateTargetSerial();
var
  enterStr: string;
  input: string;

begin
end;

procedure TmainForm.ResetLeds;
var
  led: ^TindLed;
begin
  if provisionThread.GetTermnateTestStatus then exit;
  for led in Leds do
  begin
    led^.LedColorOff := clGray;
  end;
end;

procedure TmainForm.ICTTestSwitchClick(Sender: TObject);
var
  arg: array[0..6] of string;
var
  ret: integer;
begin
  ICTTestSwitch.LedValue := False;
  if targetVendorSerial.Text = '' then exit;

  if busyFlag1 then
  begin
    exit;
  end;

  ICTTestSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '-b';
  if (provisionThread.reTestMode) then arg[5] := 'used'
  else
    arg[5] := 'new';
  arg[6] := '';
  ret := RunM1Tfc('ict', arg, ICTTestSwitch);
  if (ret <> NormalExit) then InterruptMenuItemClick(self);
end;

procedure TmainForm.InterruptMenuItemClick(Sender: TObject);
begin
  if (aProcess <> nil) and (aProcess.Running) then
  begin
    aProcess.Terminate(-1);
    Memo1.Lines.Add(log('warn', targetVendorSerial.Text, 'Terminated'));
  end;
  provisionThread.UserInterruptTest();
end;

procedure TmainForm.AppsCheckSwitchClick(Sender: TObject);
var
  arg: array[0..10] of string;
begin
  AppsCheckSwitch.LedValue := False;

  if busyFlag1 then
  begin
    exit;
  end;

  EEPROMSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  RunM1Tfc('pingM1apps', arg, AppsCheckSwitch);
end;

procedure TmainForm.EEPROMSwitchClick(Sender: TObject);
var
  arg: array[0..10] of string;
begin
  EEPROMSwitch.LedValue := False;
  if targetVendorSerial.Text = '' then exit;

  if busyFlag1 then
  begin
    exit;
  end;

  EEPROMSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  RunM1Tfc('eeprom', arg, EEPROMSwitch);
end;

procedure TmainForm.LedTimerTimer(Sender: TObject);
var
  led: ^TindLed;
begin
  if LedTimer.Tag = 1 then
  begin
    LedTimer.Tag := 0;
    exit;
  end;
  for led in Leds do
  begin
    if led^.tag = 1 then
    begin
      if not led^.LedValue then
      begin
        if led^.LedColorOff = clYellow then led^.LedColorOff := clGray
        else
          led^.LedColorOff := clYellow;
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
  startTime: integer;
  stdout: string;
begin
  if command = 'cleanup' then
  begin

    Led.tag := 1;
  end;
  provisionThread.ResetTest();
  startTime := logger.getEpochTime();
  MemoCopyTxt := '';
  Led.tag := 1;
  retValue := -1;
  Uid := '';
  busyFlag1 := True;
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

  retValue := AProcess.ExitCode;
  if (retValue = 1) then
  begin
    Memo1.Lines.Add('Abnormal termination nodejs executable not found or crashed');
    Led.LedColorOff := clRed;
  end;
  if (retValue > 1) then
  begin
    Led.LedColorOff := clRed;
    InterruptMenuItemClick(self);
    Led.Tag := 0;
  end;
  AProcess.Free;
  AProcess := nil;
  busyFlag1 := False;
  Led.tag := 0;
  Led.LedValue := False;
  if (retValue = 0) and not provisionThread.GetTermnateTestStatus then
  begin
    Led.LedColorOff := clLime;
  end;
  if provisionThread.GetTermnateTestStatus then
  begin
    Led.LedColorOff := clRed;
    Led.Tag := 0;
    retValue := 1;
  end;
  // memo1.Lines.Add(logger.log('info', command, 'Run Time - ' + IntToStr(logger.getEpochTime() - startTime)));
  Result := retValue;
end;

procedure TmainForm.MacProgSwitchClick(Sender: TObject);
var
  arg: array[0..8] of string;
  retValue: integer;
  tmpInt: integer;
  tmpString: string;
begin
  MacProgSwitch.LedValue := False;
  if targetVendorSerial.Text = '' then exit;

  if busyFlag1 then
  begin
    exit;
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
    tmpInt := 0;
    tmpString := Memo1.Text;
    tmpInt := PosEx('MAC Address is: ', MemoCopyTxt);
    tmpString := MemoCopyTxt;
    Inc(tmpInt, 16);
    Uid := copy(tmpString, tmpInt, 17);
  end
  else
    Uid := '';
end;

procedure TmainForm.Memo1DblClick(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TmainForm.Commission(Sender: TObject);
begin
  if busyFlag1 then exit;

  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Memo1.Clear;
  Memo1.Lines.Add(log('info', targetVendorSerial.Text, 'Commission new board'));
  ResetLeds;
  ColorProgress1.progress := 0;
  provisionThread.reTestMode := False;
  provisionThread.ExecuteThread;
end;

procedure TmainForm.QuitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TmainForm.ICTTestSwitchClick_;
begin
  ICTTestSwitchClick(self);
end;

procedure TmainForm.MacProgSwitchClick_;
begin
  MacProgSwitchClick(self);
end;

procedure TmainForm.FlashSwitchClick_;
begin
  FlashSwitchClick(self);
end;

procedure TmainForm.FuncTestSwitchClick_;
begin
  FuncTestSwitchClick(self);
end;

procedure TmainForm.AppsCheckSwitchClick_;
begin
  AppsCheckSwitchClick(self);
end;

procedure TmainForm.EEPROMSwitchClick_;
begin
  EEPROMSwitchClick(self);
end;

procedure TmainForm.FuncTestSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;
  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Panel1DblClick(Sender);
  FuncTestSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;

procedure TmainForm.ICTTestSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;
  if TargetVendorSerial.Text = '' then
  begin
    ShowMessage('Barcode Scan is Missing');
    exit;
  end;
  provisionThread.reTestMode := True;
  Panel1DblClick(Sender);
  ICTTestSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;


procedure TmainForm.AppsCheckwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;
  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Panel1DblClick(Sender);
  AppsCheckSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;

procedure TmainForm.EEPROMSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;
  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Panel1DblClick(Sender);
  EEPROMSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;

procedure TmainForm.MacProgSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;
  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  if TargetVendorSerial.Text = '' then
  begin
    ShowMessage('Barcode Scan is Missing');
  end;
  Panel1DblClick(Sender);
  MacProgSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;

procedure TmainForm.DoLabelSwitchClick_Wrapper(Sender: TObject);
begin

  if not CheckSerialBarcodeScan(targetVendorSerial.Text) then
  begin
    exit;
  end;
  Panel1DblClick(Sender);
  DoLabelSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;

procedure TmainForm.FlashSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then
  begin
    TindLed(Sender).LedValue := False;
    exit;
  end;

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
  ColorProgress1.Progress := 100;
end;

end.
