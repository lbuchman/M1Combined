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
    Timer1: TTimer;
    TestTumer: TTimer;
    LedTimer: TTimer;
    BarcodeScanEditTimer: TTimer;
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
    procedure Timer1Timer(Sender: TObject);
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
    doOnes: boolean;
    provisionThread: TProvisionThread;
    busyFlag: boolean;
    busyFlag1: boolean;
    previouseScanInput: string;
    newSerialNumberIsAvailable: boolean;
    newSerialNumber: string;
    configuration: TConfigration;
    SaveWidth: integer;
    Leds: array[0..6] of ^TindLed;
    MemoCopyTxt: string;
    DebugLevel: string;
    lastCommand: string;
    function RunM1Tfc(command: string; arg: array of string; var Led: TindLed): integer;
    function CheckSerial(): boolean;
    function myIPAddress(): string;
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
  end;

var
  mainForm: TmainForm;

implementation

{$R *.lfm}

{ TmainForm }
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
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '-e';
  arg[5] := '';
  RunM1Tfc('makelabel', arg, DoLabelSwitch);
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

  if (Length(targetVendorSerial.Text) > 0) then begin
    arg[0] := '-s';
    arg[1] := Trim(targetVendorSerial.Text);
    arg[2] := '-d';
    arg[3] := DebugLevel;
    arg[4] := '';
  end
  else begin
    arg[0] := '-d';
    arg[1] := DebugLevel;
    arg[2] := '';
  end;
  RunM1Tfc('makelabel', arg, DoLabelSwitch);
end;

procedure TmainForm.DoLabelError();
var
  arg: array[0..8] of string;
begin
  arg[0] := '-l';
  arg[1] := lastCommand + ',error';
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  RunM1Tfc('makelabel', arg, FakeLed);
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
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  RunM1Tfc('cleanup', arg, fakeLed);
end;

function TmainForm.myIPAddress(): string;
var
  theProcess: TProcess;
  AddressString: ansistring;
begin
  try
    theProcess := TProcess.Create(nil);
    theProcess.Executable := 'hostname';
    theProcess.Parameters.Add('-I');
    theProcess.Options := [poUsePipes, poWaitOnExit];
    theProcess.Execute;
    if theProcess.Output.NumBytesAvailable > 0 then
    begin
      SetLength(AddressString{%H-}, theProcess.Output.NumBytesAvailable);
      theProcess.Output.ReadBuffer(AddressString[1],
        theProcess.Output.NumBytesAvailable);
    end;
    Result := AddressString;
  finally
    theProcess.Free;
  end;
end;

procedure TmainForm.Debuglevel_0_Execute(Sender: TObject);
begin
  // targetVendorSerial.ReadOnly := True;
  Height := DefaultHeight;
  DevModeLabel.Visible := False;
  DevModeLabel.Caption := 'D0';
  DevModeLabel.Font.color := clRed;
  DebugLevel := '0';
end;

procedure TmainForm.Debuglevel_1_Execute(Sender: TObject);
begin
  Height := LogHeight;
  DevModeLabel.Visible := True;
  DevModeLabel.Caption := 'D1';
  DevModeLabel.Font.color := clRed;
  DebugLevel := '1';
end;

procedure TmainForm.Debuglevel_2_Execute(Sender: TObject);
begin
  Height := LogHeight;
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

procedure TmainForm.BarcodeScanEditTimerTimer(Sender: TObject);
begin
  // targetVendorSerial.ReadOnly := True;
  // BarcodeScanEditTimer.Enabled := False;
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
    Height := LogHeight;
  end;
end;

procedure TmainForm.Memo1Change(Sender: TObject);
begin

end;

procedure TmainForm.Re_TestMenuItem1Click(Sender: TObject);
begin
  if busyFlag1 then exit;

  if not checkSerial() then
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
  //
end;

procedure TmainForm.OpenLogExecute(Sender: TObject);
begin
  LogsMenuItemClick(Sender);
end;

procedure TmainForm.PublishLogMenuItemClick(Sender: TObject);
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
  RunM1Tfc('pushtocloud', arg, fakeLed);

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
  ipaddresses := myIPAddress();
 // mainForm.Caption := mainForm.Caption + ' My IP: ' + ipaddresses;
end;

procedure TmainForm.FormShow(Sender: TObject);
begin
  // Debuglevel_0_Execute(Sender);
  DebugLevel := '1';
  SaveWidth := Width;
  Constraints.MaxWidth := Width;
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

procedure TmainForm.Timer1Timer(Sender: TObject);
begin
  if busyFlag then
  begin
    exit;
  end;
  if newSerialNumberIsAvailable then
  begin
    if busyFlag then
    begin
      exit;
    end;
    Commission(Sender);
    busyFlag := False;
    newSerialNumberIsAvailable := False;
  end;
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
  else arg[5] := 'new';
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
 {
  result := 0;
  Led.Tag := 1;
  for  BytesRead:=0 to 20 do begin
      Application.ProcessMessages;
      Sleep(300);

  end;
  Led.Tag := 0;
  exit(0);
  }
  lastCommand := command;
  provisionThread.ResetTest();
  startTime := logger.getEpochTime();
  MemoCopyTxt := '';
  Led.tag := 1;
  retValue := -1;
  Uid := '';
  busyFlag1 := True;
  AProcess := TProcess.Create(nil);
  AProcess.Executable := 'm1tfd1.cli'{'m1tfc'};
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
  newSerialNumberIsAvailable := False;
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

  if not checkSerial() then
  begin
    exit;
  end;
  Memo1.Clear;
  Memo1.Lines.Add(log('info', targetVendorSerial.Text, 'Commission new board'));
  ResetLeds;
  ColorProgress1.progress := 0;
  provisionThread.reTestMode := false;
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

function TmainForm.checkSerial(): boolean;
var
  serial: integer;
begin
  try
    serial := StrToInt(TargetvendorSerial.Text);
    Result := True;
  except
    On E: EConvertError do
    begin
      ShowMessage('Scan the bar code from M1-3200');
      Result := False;
    end;
  end;
end;

procedure TmainForm.FuncTestSwitchClick_Wrapper(Sender: TObject);
begin
    if DebugLevel <> '2' then begin
      TindLed(Sender).LedValue := false;
      exit;
  end;
  if TargetVendorSerial.Text = ''  then begin
      ShowMessage('Barcode Scan is Missing');
      exit;
  end;
  Panel1DblClick(Sender);
  FuncTestSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;

procedure TmainForm.ICTTestSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then begin
       TindLed(Sender).LedValue := false;
       exit;
  end;
  if TargetVendorSerial.Text = ''  then begin
      ShowMessage('Barcode Scan is Missing');
      exit;
  end;
  provisionThread.reTestMode := true;
  Panel1DblClick(Sender);
  ICTTestSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;


procedure TmainForm.AppsCheckwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then begin
      TindLed(Sender).LedValue := false;
      exit;
  end;

  Panel1DblClick(Sender);
  AppsCheckSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;

procedure TmainForm.EEPROMSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then begin
      TindLed(Sender).LedValue := false;
      exit;
  end;
  if TargetVendorSerial.Text = '' then begin
      ShowMessage('Barcode Scan is Missing');
  end;
  Panel1DblClick(Sender);
  EEPROMSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;

procedure TmainForm.MacProgSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then begin
      TindLed(Sender).LedValue := false;
      exit;
  end;
  if TargetVendorSerial.Text = ''  then begin
      ShowMessage('Barcode Scan is Missing');
  end;
  Panel1DblClick(Sender);
  MacProgSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;

procedure TmainForm.DoLabelSwitchClick_Wrapper(Sender: TObject);
begin

  if TargetVendorSerial.Text = ''  then begin
    //  ShowMessage('Barcode Scan is Missing');
  end;
  Panel1DblClick(Sender);
  DoLabelSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;

procedure TmainForm.FlashSwitchClick_Wrapper(Sender: TObject);
begin
  if DebugLevel <> '2' then begin
      TindLed(Sender).LedValue := false;
      exit;
  end;
  if TargetVendorSerial.Text = ''  then begin
      ShowMessage('Barcode Scan is Missing');
  end;
  Panel1DblClick(Sender);
  FlashSwitchClick(Sender);
  ColorProgress1.Progress := 100;
end;

end.
