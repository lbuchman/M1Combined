unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, Buttons, ComCtrls, ActnList, MaskEdit, SpinEx, IndLed, BCMDButton,
  strutils, BCListBox, BCTrackbarUpdown, DTAnalogGauge, dtthemedgauge,
  DTAnalogClock, dtthemedclock, scannerClient, Process, logger,
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
  DefaultHeight = 320;

type

  { TmainForm }

  TmainForm = class(TForm)
    OpenLog: TAction;
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
    procedure BarcodeScanEditTimerTimer(Sender: TObject);
    procedure EnableSerialNumberExecute(Sender: TObject);
    procedure LogsMenuItemClick(Sender: TObject);
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
    procedure InterruptMenuItemClick(Sender: TObject);
    procedure LedTimerTimer(Sender: TObject);
    procedure MacProgSwitchClick(Sender: TObject);
    procedure Memo1DblClick(Sender: TObject);
    procedure StartTestClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Debuglevel_2_Execute(Sender: TObject);
    procedure Debuglevel_1_Execute(Sender: TObject);
    procedure Debuglevel_0_Execute(Sender: TObject);
  private
    AProcess: TProcess;
    Uid: string;
    doOnes : Boolean;
    scannerThread: TUDPScannerServerThread;
    provisionThread: TProvisionThread;
    busyFlag: boolean;
    busyFlag1: boolean;
    previouseScanInput : String;
    newSerialNumberIsAvailable: boolean;
    newSerialNumber: string;
    configuration: TConfigration;
    SaveWidth: integer;
    ClearProgressBar: boolean;
    Leds: array[0..5] of ^TindLed;
    MemoCopyTxt : String;
    DebugLevel : String;
    function RunM1Tfc(command: string; arg: array of string; var Led: TindLed): integer;
    function CheckSerial(): boolean;

  public
    newData: string;
    procedure DoCleanupCmd();
    procedure UpdateTargetSerial();
    procedure ICTTestSwitchClick_;
    procedure MacProgSwitchClick_;
    procedure FlashSwitchClick_;
    procedure FuncTestSwitchClick_;
    procedure EEPROMSwitchClick_;
    procedure DoLabelSwitchClick_;
    procedure ResetLeds;
    procedure AddToProgressBar(value : Integer);
    procedure Add5ToProgressBar;
    procedure Add10ToProgressBar;
    procedure Add15ToProgressBar;
    procedure Add20ToProgressBar;
    procedure Add25ToProgressBar;
    procedure Add30ToProgressBar;
    procedure ClearbusyFlag1;
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
procedure TmainForm.AddToProgressBar(value : Integer);
var
  progress: integer;
begin
  progress := ColorProgress1.Progress;
  ColorProgress1.Progress := progress + value;
  if (ColorProgress1.Progress > 99) then ColorProgress1.ForeColor := clLime;
end;

procedure TmainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  InterruptMenuItemClick(self);
  Application.ProcessMessages;
  scannerThread.Terminate;
  scannerThread.WaitFor;
  scannerThread.Free;
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
  if targetVendorSerial.text = '' then exit;
  if busyFlag1 then
  begin
    DoLabelSwitch.LedValue := False;
    exit;
  end;

  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '-e';
  arg[5] := '';
  if ClearProgressBar then colorProgress1.Progress := 0;
  RunM1Tfc('makelabel', arg, DoLabelSwitch);
end;

procedure TmainForm.DoLabelSwitchClick(Sender: TObject);
var
  arg: array[0..8] of string;
begin
  DoLabelSwitch.LedValue := False;
  if targetVendorSerial.text = '' then exit;
  if busyFlag1 then
  begin
    DoLabelSwitch.LedValue := False;
    exit;
  end;

  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  if ClearProgressBar then colorProgress1.Progress := 0;
  RunM1Tfc('makelabel', arg, DoLabelSwitch);
end;

procedure TmainForm.DoCleanupCmd();
var
  arg: array[0..8] of string;
begin
  DoLabelSwitch.LedValue := False;
  if targetVendorSerial.text = '' then exit;
  if busyFlag1 then
  begin
    DoLabelSwitch.LedValue := False;
    exit;
  end;

  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  if ClearProgressBar then colorProgress1.Progress := 0;
  RunM1Tfc('cleanup', arg, fakeLed);
end;

procedure TmainForm.Debuglevel_0_Execute(Sender: TObject);
begin
  targetVendorSerial.ReadOnly := true;
  Height := DefaultHeight;
  DevModeLabel.Visible := false;
  DevModeLabel.Caption := 'D0';
  DevModeLabel.Font.color := clRed;
  DebugLevel := '0';
end;

procedure TmainForm.Debuglevel_1_Execute(Sender: TObject);
begin
  Height := LogHeight;
  DevModeLabel.Visible := true;
  DevModeLabel.Caption := 'D1';
  DevModeLabel.Font.color := clRed;
  DebugLevel := '1';
  end;

procedure TmainForm.Debuglevel_2_Execute(Sender: TObject);
begin
  Height := LogHeight;
  DevModeLabel.Visible := true;
  DevModeLabel.Caption := 'D2';
  DevModeLabel.Font.color := clRed;
  DebugLevel := '2';
end;

procedure TmainForm.EnableSerialNumberExecute(Sender: TObject);
begin
  targetVendorSerial.Readonly := false;
  BarcodeScanEditTimer.Enabled := true;
end;

procedure TmainForm.BarcodeScanEditTimerTimer(Sender: TObject);
begin
  targetVendorSerial.Readonly := true;
  BarcodeScanEditTimer.Enabled := false;
end;

procedure TmainForm.LogsMenuItemClick(Sender: TObject);
var
  homeEnv: string;
  envVarName: string;
begin
  envVarName := 'HOME';
  homeEnv := GetEnvironmentVariable(envVarName);
  LogsOpenDialog1.InitialDir := homeEnv + DirectorySeparator + 'm1mtf/logs';
  if LogsOpenDialog1.execute then
  begin
     Memo1.Lines.LoadFromFile(LogsOpenDialog1.Filename);
     Height := LogHeight;
  end;
end;

procedure TmainForm.OpenLogExecute(Sender: TObject);
begin
  LogsMenuItemClick(Sender);
end;

procedure TmainForm.PublishLogMenuItemClick(Sender: TObject);
var
  arg: array[0..8] of string;
begin
  FlashSwitch.LedValue := false;
  if targetVendorSerial.text = '' then exit;

  if busyFlag1 then
  begin
    exit;
  end;

  FlashSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  RunM1Tfc('pushtocloud', arg, fakeLed);

end;

procedure TmainForm.Panel1DblClick(Sender: TObject);
begin
  ColorProgress1.Progress := 0;
  ResetLeds();
end;

procedure TmainForm.FlashSwitchClick(Sender: TObject);
var
  arg: array[0..8] of string;
begin
  FlashSwitch.LedValue := false;
  if targetVendorSerial.text = '' then exit;

  if busyFlag1 then
  begin
    exit;
  end;

  FlashSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  if ClearProgressBar then ColorProgress1.Progress := 0;
  RunM1Tfc('flash', arg, FlashSwitch);

end;

procedure TmainForm.FormCreate(Sender: TObject);
var pid : String;
begin
  busyFlag := False;
  busyFlag1 := False;
  doOnes := true;
  try
  configuration := ConfigurationGet;
  Except
    begin
      ShowMessage('must have config file $HOME/configM1.json');
      Application.Terminate;
    end;
  end;
  scannerThread := TUDPScannerServerThread.Create(False, configuration.ScannerUdpPort);
  provisionThread := TProvisionThread.Create(False);
  // Leds[0] := @ProvisionSwitch;
  Leds[0] := @ICTTestSwitch;
  Leds[1] := @MacProgSwitch;
  Leds[2] := @FlashSwitch;
  Leds[3] := @FuncTestSwitch;
  Leds[4] := @EEPROMSwitch;
  Leds[5] := @DoLabelSwitch;
  LedTimer.Enabled := True;
  DebugLevel := '0';
  Memo1.Font.Size  := 12;

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
 // Debuglevel_0_Execute(Sender);
  DebugLevel := '0';
  SaveWidth := Width;
  Constraints.MaxWidth := Width;
  ClearProgressBar := True;
end;

procedure TmainForm.FuncTestSwitchClick(Sender: TObject);
var
  arg: array[0..8] of string;
begin
  FuncTestSwitch.LedValue := false;
  if targetVendorSerial.text = '' then exit;

  if busyFlag1 then
  begin
    exit;
  end;
  FuncTestSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  if ClearProgressBar then ColorProgress1.Progress := 0;
  RunM1Tfc('functest', arg, FuncTestSwitch);

end;

procedure TmainForm.UpdateTargetSerial();
var
  enterStr: string;
  input: string;

begin
  input := scannerThread.GetSerialNumber;

  if CompareStr('ENTER', input) <> 0 then begin
     newSerialNumber := input;
     targetVendorSerial.Text := input;
     previouseScanInput := input;
     exit;
  end;

  if (CompareStr('ENTER', input) = 0 ) and (targetVendorSerial.Text <> '') then begin
    if CompareStr('ENTER', previouseScanInput) <> 0 then begin
          newSerialNumberIsAvailable := True;
    end;
  end;

  previouseScanInput := input;

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
    StartTestClick(Sender);
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
  var ret : integer;
begin
  ICTTestSwitch.LedValue := false;
  if targetVendorSerial.text = '' then exit;

  if busyFlag1 then
  begin
    exit;
  end;

  ICTTestSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  if ClearProgressBar then ColorProgress1.Progress := 0;
  ret := RunM1Tfc('ict', arg, ICTTestSwitch);
  if (ret <> NormalExit) then InterruptMenuItemClick(self);
end;

procedure TmainForm.InterruptMenuItemClick(Sender: TObject);
begin
  if (aProcess <> nil) and (aProcess.Running) then
  begin
    aProcess.Terminate(-1);
    Memo1.Lines.Add(log('warn', targetVendorSerial.text, 'Terminated'));
  end;
  provisionThread.InterruptTest();
end;

procedure TmainForm.EEPROMSwitchClick(Sender: TObject);
var
  arg: array[0..10] of string;
begin
  EEPROMSwitch.LedValue := false;
  if targetVendorSerial.text = '' then exit;

  if busyFlag1 then
  begin
    exit;
  end;

  EEPROMSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  if ClearProgressBar then ColorProgress1.Progress := 0;
  RunM1Tfc('eeprom', arg, EEPROMSwitch);
end;

procedure TmainForm.LedTimerTimer(Sender: TObject);
var
  led: ^TindLed;
begin
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
  startTime : Integer;
  stdout : String;
begin
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
  if BytesRead > 0 then begin
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
  end;
  AProcess.Free;
  AProcess := nil;
  busyFlag1 := False;
  Led.tag := 0;
  Led.LedValue := False;
  if (retValue = 0) and not provisionThread.GetTermnateTestStatus then begin
    Led.LedColorOff := clLime;
  end;
  if provisionThread.GetTermnateTestStatus then begin
   Led.LedColorOff := clRed;
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
  MacProgSwitch.LedValue := false;
  if targetVendorSerial.text = '' then exit;

  if busyFlag1 then
  begin
    exit;
  end;

  MacProgSwitch.LedValue := False;
  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.text);
  arg[2] := '-d';
  arg[3] := DebugLevel;
  arg[4] := '';
  if ClearProgressBar then ColorProgress1.Progress := 0;
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

procedure TmainForm.StartTestClick(Sender: TObject);
begin
  if busyFlag1 then exit;
  if not checkSerial() then
  begin
    exit;
  end;
 // targetVendorSerial.Text := 'S' + targetVendorSerial.Text;
  Memo1.Clear;
  ResetLeds;
  ColorProgress1.progress := 0;
  provisionThread.ExecuteThread;
end;

procedure TmainForm.QuitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TmainForm.ICTTestSwitchClick_;
begin
  ClearProgressBar := False;
  ICTTestSwitchClick(self);
  ClearProgressBar := True;
end;

procedure TmainForm.MacProgSwitchClick_;
begin
  ClearProgressBar := False;
  MacProgSwitchClick(self);
  ClearProgressBar := True;
end;

procedure TmainForm.FlashSwitchClick_;
begin
  ClearProgressBar := False;
  FlashSwitchClick(self);
  ClearProgressBar := True;
end;

procedure TmainForm.FuncTestSwitchClick_;
begin
  ClearProgressBar := False;
  FuncTestSwitchClick(self);
  ClearProgressBar := True;
end;

procedure TmainForm.EEPROMSwitchClick_;
begin
  ClearProgressBar := False;
  EEPROMSwitchClick(self);
  ClearProgressBar := True;
end;

function TmainForm.checkSerial(): boolean;
var
  serial: integer;
begin
  try
    serial := StrToInt(TargetvendorSerial.text);
    Result := True;
  except
    On E: EConvertError do
    begin
      ShowMessage('Scan the bar code from M1-3200');
      Result := False;
    end;
  end;
end;


end.
