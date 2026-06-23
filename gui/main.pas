
Unit main;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
Menus, Buttons, ComCtrls, ActnList, MaskEdit, SpinEx, IndLed, BCMDButton,
strutils, BCListBox, Process, logger, about, DateUtils, errorReportForm,
configurationjson, jsonparser, ColorProgress, MSSQLConn,
testrunner, testexecution, ledmanager, cloudops;

Const 
  Interval7Days = (24 * 60 * 60 * 7);
  UpdateFwTimeStamp = 'UpdateFwTimeStamp.txt';
  UpdateSycretsTimeStamp = 'UpdateSycretsTimeStamp.txt';
  UpdateLogsTimeStamp = 'UpdateLogsTimeStamp.txt';
  DEFAULT_DATETIME = '2023-01-24 21:20:08';

Type 

  { TmainForm }

  TmainForm = Class(TForm)
    Revs: TMenuItem;
    PushSecretsMenuItem: TMenuItem;
    PushLogsMenuItem: TMenuItem;
    SyncFailedLabel: TLabel;
    Re_Test: TAction;
    StopTestClick: TAction;
    QuitClick: TAction;
    AppsCheckSwitch: TindLed;
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
    Procedure AboutMenuItemClick(Sender: TObject);
    Procedure CheckCloudUpdateTimerTimer(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure PushLogsMenuItemClick(Sender: TObject);
    Procedure PushSecretsMenuItemClick(Sender: TObject);
    Procedure RevsClick(Sender: TObject);
    Procedure Re_TestMenuItem1Click(Sender: TObject);
    Procedure PublishLogMenuItemClick(Sender: TObject);
    Procedure Panel1DblClick(Sender: TObject);
    Function DoLabelSwitchClick(Sender: TObject): integer;
    Function FlashSwitchClick(Sender: TObject): integer;
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Function FuncTestSwitchClick(Sender: TObject): integer;
    Function ICTTestSwitchClick(Sender: TObject): integer;
    Function EEPROMSwitchClick(Sender: TObject): integer;
    Procedure EEPROMSwitchClick_Wrapper(Sender: TObject);
    Function AppsCheckSwitchClick(Sender: TObject): integer;
    Procedure InterruptMenuItemClick(Sender: TObject);
    Procedure LedsTimer(Sender: TObject);
    Function MacProgSwitchClick(Sender: TObject): integer;
    Procedure Memo1DblClick(Sender: TObject);
    Procedure Commission(Sender: TObject);
    Procedure QuitMenuItemClick(Sender: TObject);
    Procedure Debuglevel_2_Execute(Sender: TObject);
    Procedure Debuglevel_1_Execute(Sender: TObject);
    Procedure Debuglevel_0_Execute(Sender: TObject);
    Procedure FuncTestSwitchClick_Wrapper(Sender: TObject);
    Procedure ICTTestSwitchClick_Wrapper(Sender: TObject);
    Procedure MacProgSwitchClick_Wrapper(Sender: TObject);
    Procedure DoLabelSwitchClick_Wrapper(Sender: TObject);
    Procedure FlashSwitchClick_Wrapper(Sender: TObject);
    Procedure AppsCheckSwitchClick_Wrapper(Sender: TObject);
    Procedure SyncLabelLedTimerTimer(Sender: TObject);
    Procedure UpdateMeMenuItemClick(Sender: TObject);
    Private 
      FTestRunner: TTestRunner;
      FTestExecutor: TTestExecutor;
      FLedManager: TLedManager;
      FCloudOps: TCloudOperations;

      TestMode: TestingMode;
      testStatus: boolean;
      testRet: integer;
      printError: boolean;
      blinkMe: boolean;
      doOnes: boolean;
      busyFlag: boolean;
      configuration: TConfiguration;
      Tests: array[0..6] Of TestRecord;
      DebugLevel: string;
      fwVersionFile: string;

      Function GetDateTimeFromFile(filename: String): TDateTime;
      Function ReadThisFile(FileName: String): string;
      Procedure SetFlag(Var Flag: boolean; Value: boolean);
    Public 
      newData: string;
      Procedure DoCleanupCmd();
      Procedure ResetLeds;
      Procedure AddToProgressBar(Value: integer);
      Procedure ClearBusyFlag;
      Procedure SetBusyFlag;
      Procedure DoLabelError;
      Procedure SetTestStatusFailed;
      Procedure SetTestStatusOk;
  End;

Var 
  mainForm: TmainForm;

Implementation

{$R *.lfm}

{ TmainForm }

Function TmainForm.ReadThisFile(FileName: String): string;

Var 
  f: TextFile;
  OneLine: string;
Begin
  Result := '';
  assignFile(f, FileName);
  reset(f);
  While Not EOF(f) Do
    Begin
      readln(f, OneLine);
      Result := OneLine;
      closefile(f);
      exit;
    End;
End;

Function TmainForm.GetDateTimeFromFile(filename: String): TDateTime;

Var 
  myDateTimeVariable: TDateTime;
  FS: TFormatSettings;
  dateTimeFromFile: string;
  filePath: string;
Begin
  dateTimeFromFile := DEFAULT_DATETIME;
  filePath := GetEnvironmentVariable('HOME') + '/m1mtf/' + filename;
  Try
    dateTimeFromFile := ReadThisFile(filePath);
  Except
    on E: Exception Do dateTimeFromFile := DEFAULT_DATETIME;
End;
If dateTimeFromFile = '' Then dateTimeFromFile := DEFAULT_DATETIME;

FS := DefaultFormatSettings;
FS.DateSeparator := '-';
FS.ShortDateFormat := 'yyyy-mm-dd';
FS.ShortTimeFormat := 'hh:mm:ss';
myDateTimeVariable := strtodatetime(dateTimeFromFile, FS);
Result := myDateTimeVariable;
End;

Procedure TmainForm.SetFlag(Var Flag: boolean; Value: boolean);
Begin
  Flag := Value;
End;

Procedure TmainForm.SetTestStatusFailed;
Begin
  SetFlag(testStatus, False);
End;

Procedure TmainForm.SetTestStatusOk;
Begin
  SetFlag(testStatus, True);
End;

Procedure TmainForm.SetbusyFlag;
Begin
  SetFlag(busyFlag, True);
End;

Procedure TmainForm.ClearbusyFlag;
Begin
  SetFlag(busyFlag, False);
End;

Procedure TmainForm.AddToProgressBar(Value: integer);
Begin
  FLedManager.AddToProgressBar(Value);
End;

Procedure TmainForm.ResetLeds;
Begin
  DoLabelSwitch.LedColorOff := clGray;
  EEPROMSwitch.LedColorOff := clGray;
  FlashSwitch.LedColorOff := clGray;
  FuncTestSwitch.LedColorOff := clGray;
  ICTTestSwitch.LedColorOff := clGray;
  MacProgSwitch.LedColorOff := clGray;
  AppsCheckSwitch.LedColorOff := clGray;
End;

Procedure TmainForm.FormCreate(Sender: TObject);

Var 
  pid: string;
Begin
  // Initialize helper classes
  FTestRunner := TTestRunner.Create(Memo1, DebugLevel);
  FLedManager := TLedManager.Create(ColorProgress1);
  FCloudOps := TCloudOperations.Create(Memo1);

  printError := False;
  testStatus := True;
  ClearBusyFlag;
  doOnes := True;
  SyncFailedLabel.Font.Color := clRed;

  // Note: Test handlers will be set up when needed
  DebugLevel := GetEnvironmentVariable('m1tfdebug');
  If DebugLevel <> '1' Then DebugLevel := '0';

  Memo1.Font.Size := 12;

  pid := IntToStr(system.GetProcessID);
  With TStringList.Create Do
    Try
      Add(pid);
      SaveToFile(GetEnvironmentVariable('HOME') + '/m1mtf/m1tfd1app.pid');
    Finally
      Free;
End;

SyncLelLedTimer.Enabled := True;
End;

Procedure TmainForm.FormShow(Sender: TObject);
Begin
  If ReadConfigFile = False Then
    Begin
      ShowMessage('Mising Configfile ' + config.error);
      Application.Terminate;
    End;
  configuration := ConfigurationGet;
  fwVersionFile := configuration.fwDir + '/VERSION';
  aboutForm.SetFwDirectory(fwVersionFile);

  FTestRunner.DebugLevel := DebugLevel;
End;

Procedure TmainForm.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  InterruptMenuItemClick(self);
  Application.ProcessMessages;

  // Clean up helper instances
  If FTestRunner <> Nil Then
    FTestRunner.Free;
  If FTestExecutor <> Nil Then
    FTestExecutor.Free;
  If FLedManager <> Nil Then
    FLedManager.Free;
  If FCloudOps <> Nil Then
    FCloudOps.Free;
End;

Procedure TmainForm.SetbusyFlag;
Begin
  SetFlag(busyFlag, True);
End;

Procedure TmainForm.ClearbusyFlag;
Begin
  SetFlag(busyFlag, False);
End;

Procedure TmainForm.DoCleanupCmd();

Var 
  arg: array[0..8] Of string;
  fakeLed: TindLed;
Begin
  If targetVendorSerial.Text = '' Then exit;
  If busyFlag Then
    exit;

  arg[0] := '-s';
  arg[1] := Trim(targetVendorSerial.Text);
  If (testStatus) Then
    arg[2] := ''
  Else
    Begin
      arg[2] := '-e';
      arg[3] := '';
    End;

  FTestRunner.RunM1Tfc('cleanup', arg, fakeLed);
End;

Procedure TmainForm.DoLabelError();
Begin
  printError := True;
  DoLabelSwitchClick(self);
  printError := False;
End;

Function TmainForm.DoLabelSwitchClick(Sender: TObject): integer;
Begin
  If FTestExecutor = Nil Then
    FTestExecutor := TTestExecutor.Create(FTestRunner, targetVendorSerial, Memo1
                     );
  Result := FTestExecutor.DoLabelSwitchClick(DebugLevel, busyFlag, printError,
            DoLabelSwitch, testStatus);
End;

Function TmainForm.FlashSwitchClick(Sender: TObject): integer;
Begin
  If FTestExecutor = Nil Then
    FTestExecutor := TTestExecutor.Create(FTestRunner, targetVendorSerial, Memo1
                     );
  Result := FTestExecutor.FlashSwitchClick(DebugLevel, busyFlag, DebugLevel,
            FlashSwitch);
End;

Function TmainForm.FuncTestSwitchClick(Sender: TObject): integer;
Begin
  If FTestExecutor = Nil Then
    FTestExecutor := TTestExecutor.Create(FTestRunner, targetVendorSerial, Memo1
                     );
  Result := FTestExecutor.FuncTestSwitchClick(DebugLevel, busyFlag, DebugLevel,
            FuncTestSwitch);
End;

Function TmainForm.ICTTestSwitchClick(Sender: TObject): integer;
Begin
  If FTestExecutor = Nil Then
    FTestExecutor := TTestExecutor.Create(FTestRunner, targetVendorSerial, Memo1
                     );
  Result := FTestExecutor.ICTTestSwitchClick(DebugLevel, busyFlag, DebugLevel,
            ICTTestSwitch, TestMode);
End;

Function TmainForm.AppsCheckSwitchClick(Sender: TObject): integer;
Begin
  If FTestExecutor = Nil Then
    FTestExecutor := TTestExecutor.Create(FTestRunner, targetVendorSerial, Memo1
                     );
  Result := FTestExecutor.AppsCheckSwitchClick(DebugLevel, busyFlag, DebugLevel,
            AppsCheckSwitch, EEPROMSwitch);
End;

Function TmainForm.EEPROMSwitchClick(Sender: TObject): integer;
Begin
  If FTestExecutor = Nil Then
    FTestExecutor := TTestExecutor.Create(FTestRunner, targetVendorSerial, Memo1
                     );
  Result := FTestExecutor.EEPROMSwitchClick(DebugLevel, busyFlag, DebugLevel,
            EEPROMSwitch);
End;

Function TmainForm.MacProgSwitchClick(Sender: TObject): integer;
Begin
  If FTestExecutor = Nil Then
    FTestExecutor := TTestExecutor.Create(FTestRunner, targetVendorSerial, Memo1
                     );
  Result := FTestExecutor.MacProgSwitchClick(DebugLevel, busyFlag, DebugLevel,
            MacProgSwitch);
End;

Procedure TmainForm.LedsTimer(Sender: TObject);
Begin
  If FLedManager.SyncTimerTag = 1 Then
    Begin
      FLedManager.SyncTimerTag := 0;
      exit;
    End;

  If DoLabelSwitch.tag = 1 Then
    Begin
      If Not DoLabelSwitch.LedValue Then
        Begin
          If DoLabelSwitch.LedColorOff = clYellow Then
            DoLabelSwitch.LedColorOff := clGray
          Else
            DoLabelSwitch.LedColorOff := clYellow;
        End;
    End;
  If EEPROMSwitch.tag = 1 Then
    Begin
      If Not EEPROMSwitch.LedValue Then
        Begin
          If EEPROMSwitch.LedColorOff = clYellow Then
            EEPROMSwitch.LedColorOff := clGray
          Else
            EEPROMSwitch.LedColorOff := clYellow;
        End;
    End;
  If FlashSwitch.tag = 1 Then
    Begin
      If Not FlashSwitch.LedValue Then
        Begin
          If FlashSwitch.LedColorOff = clYellow Then
            FlashSwitch.LedColorOff := clGray
          Else
            FlashSwitch.LedColorOff := clYellow;
        End;
    End;
  If FuncTestSwitch.tag = 1 Then
    Begin
      If Not FuncTestSwitch.LedValue Then
        Begin
          If FuncTestSwitch.LedColorOff = clYellow Then
            FuncTestSwitch.LedColorOff := clGray
          Else
            FuncTestSwitch.LedColorOff := clYellow;
        End;
    End;
  If ICTTestSwitch.tag = 1 Then
    Begin
      If Not ICTTestSwitch.LedValue Then
        Begin
          If ICTTestSwitch.LedColorOff = clYellow Then
            ICTTestSwitch.LedColorOff := clGray
          Else
            ICTTestSwitch.LedColorOff := clYellow;
        End;
    End;
  If MacProgSwitch.tag = 1 Then
    Begin
      If Not MacProgSwitch.LedValue Then
        Begin
          If MacProgSwitch.LedColorOff = clYellow Then
            MacProgSwitch.LedColorOff := clGray
          Else
            MacProgSwitch.LedColorOff := clYellow;
        End;
    End;
  If AppsCheckSwitch.tag = 1 Then
    Begin
      If Not AppsCheckSwitch.LedValue Then
        Begin
          If AppsCheckSwitch.LedColorOff = clYellow Then
            AppsCheckSwitch.LedColorOff := clGray
          Else
            AppsCheckSwitch.LedColorOff := clYellow;
        End;
    End;
End;

Procedure TmainForm.Memo1DblClick(Sender: TObject);
Begin
  Memo1.Clear;
End;

Procedure TmainForm.Panel1DblClick(Sender: TObject);
Begin
  ColorProgress1.Progress := 0;
  Memo1DblClick(Sender);
  ResetLeds();
End;

Procedure TmainForm.Commission(Sender: TObject);
Begin
  If busyFlag Then exit;
  If Not FTestRunner.CheckSerialBarcodeScan(targetVendorSerial.Text) Then
    exit;

  Memo1.Clear;
  Memo1.Lines.Add(Log('info', targetVendorSerial.Text, 'Commission board'));
  ResetLeds;
  ColorProgress1.progress := 0;

  TestMode := TestingMode.commission;
  targetVendorSerial.Text := '';
End;

Procedure TmainForm.Re_TestMenuItem1Click(Sender: TObject);
Begin
  If busyFlag Then exit;
  If Not FTestRunner.CheckSerialBarcodeScan(targetVendorSerial.Text) Then
    exit;

  Memo1.Clear;
  Memo1.Lines.Add(Log('info', targetVendorSerial.Text, 'Re-Test board'));
  ResetLeds;
  ColorProgress1.progress := 0;
  TestMode := TestingMode.re_test;
  targetVendorSerial.Text := '';
End;

Procedure TmainForm.Debuglevel_0_Execute(Sender: TObject);
Begin
  DevModeLabel.Visible := False;
  DevModeLabel.Caption := 'D0';
  DevModeLabel.Font.color := clRed;
  DebugLevel := '0';
  FTestRunner.FDebugLevel := '0';
End;

Procedure TmainForm.Debuglevel_1_Execute(Sender: TObject);
Begin
  DevModeLabel.Visible := True;
  DevModeLabel.Caption := 'D1';
  DevModeLabel.Font.color := clRed;
  DebugLevel := '1';
  FTestRunner.FDebugLevel := '1';
End;

Procedure TmainForm.Debuglevel_2_Execute(Sender: TObject);
Begin
  DevModeLabel.Visible := True;
  DevModeLabel.Caption := 'D2';
  DevModeLabel.Font.color := clRed;
  DebugLevel := '2';
  FTestRunner.FDebugLevel := '2';
End;

Procedure TmainForm.InterruptMenuItemClick(Sender: TObject);
Begin
  If (FTestRunner.Process <> Nil) And (FTestRunner.Process.Running) Then
    Begin
      FTestRunner.Process.Terminate(100);
      Memo1.Lines.Add(Log('warn', targetVendorSerial.Text, 'Terminated'));
    End;
End;

Procedure TmainForm.AboutMenuItemClick(Sender: TObject);
Begin
  aboutForm.ShowModal;
End;

Procedure TmainForm.CheckCloudUpdateTimerTimer(Sender: TObject);

Var 
  epochTime: int64;
  epochTimeNow: int64;
  dateTimeNow: TDateTime;
Begin
  blinkMe := False;
  dateTimeNow := Now;
  epochTimeNow := DateTimeToUnix(dateTimeNow, False);

  epochTime := DateTimeToUnix(GetDateTimeFromFile(UpdateSycretsTimeStamp));
  If (epochTimeNow - epochTime) > Interval7Days Then blinkMe := True;
  epochTime := DateTimeToUnix(GetDateTimeFromFile(UpdateLogsTimeStamp));
  If (epochTimeNow - epochTime) > Interval7Days Then blinkMe := True;

  GetDateTimeFromFile(UpdateSycretsTimeStamp);
End;

Procedure TmainForm.RevsClick(Sender: TObject);
Begin
  aboutForm.ShowModal;
End;

Procedure TmainForm.MenuItem1Click(Sender: TObject);
Begin

End;

Procedure TmainForm.PushLogsMenuItemClick(Sender: TObject);
Begin
  FCloudOps.PushLogs;
End;

Procedure TmainForm.PushSecretsMenuItemClick(Sender: TObject);
Begin
  FCloudOps.PushSecrets;
End;

Procedure TmainForm.PublishLogMenuItemClick(Sender: TObject);
Begin
  FCloudOps.PublishLog;
End;

Procedure TmainForm.UpdateMeMenuItemClick(Sender: TObject);
Begin
  FCloudOps.UpdateMe;
End;

Procedure TmainForm.EEPROMSwitchClick_Wrapper(Sender: TObject);
Begin
  If DebugLevel <> '2' Then
    Begin
      TindLed(Sender).LedValue := False;
      exit;
    End;

  TindLed(Sender).LedValue := False;
  If Not FTestRunner.CheckSerialBarcodeScan(targetVendorSerial.Text) Then
    exit;

  Panel1DblClick(Sender);
  EEPROMSwitchClick(Sender);
End;

Procedure TmainForm.FuncTestSwitchClick_Wrapper(Sender: TObject);
Begin
  If DebugLevel <> '2' Then
    Begin
      TindLed(Sender).LedValue := False;
      exit;
    End;
  TindLed(Sender).LedValue := False;
  If Not FTestRunner.CheckSerialBarcodeScan(targetVendorSerial.Text) Then
    exit;
  Panel1DblClick(Sender);
  FuncTestSwitchClick(Sender);
End;

Procedure TmainForm.ICTTestSwitchClick_Wrapper(Sender: TObject);
Begin
  If DebugLevel <> '2' Then
    Begin
      TindLed(Sender).LedValue := False;
      exit;
    End;
  TindLed(Sender).LedValue := False;
  If Not FTestRunner.CheckSerialBarcodeScan(targetVendorSerial.Text) Then
    exit;
  Panel1DblClick(Sender);
  ICTTestSwitchClick(Sender);
End;

Procedure TmainForm.AppsCheckSwitchClick_Wrapper(Sender: TObject);
Begin
  If DebugLevel <> '2' Then
    Begin
      TindLed(Sender).LedValue := False;
      exit;
    End;
  TindLed(Sender).LedValue := False;
  If Not FTestRunner.CheckSerialBarcodeScan(targetVendorSerial.Text) Then
    exit;
  Panel1DblClick(Sender);
  AppsCheckSwitchClick(Sender);
End;

Procedure TmainForm.MacProgSwitchClick_Wrapper(Sender: TObject);
Begin
  If DebugLevel <> '2' Then
    Begin
      TindLed(Sender).LedValue := False;
      exit;
    End;

  TindLed(Sender).LedValue := False;
  If Not FTestRunner.CheckSerialBarcodeScan(targetVendorSerial.Text) Then
    exit;

  Panel1DblClick(Sender);
  MacProgSwitchClick(Sender);
End;

Procedure TmainForm.DoLabelSwitchClick_Wrapper(Sender: TObject);
Begin
  If DebugLevel <> '2' Then
    Begin
      TindLed(Sender).LedValue := False;
      exit;
    End;

  If Not FTestRunner.CheckSerialBarcodeScan(targetVendorSerial.Text) Then
    Begin
      TindLed(Sender).LedValue := False;
      exit;
    End;
  Panel1DblClick(Sender);
  DoLabelSwitchClick(Sender);
End;

Procedure TmainForm.FlashSwitchClick_Wrapper(Sender: TObject);
Begin
  If DebugLevel <> '2' Then
    Begin
      TindLed(Sender).LedValue := False;
      exit;
    End;

  TindLed(Sender).LedValue := False;
  If Not FTestRunner.CheckSerialBarcodeScan(targetVendorSerial.Text) Then
    exit;
  If TargetVendorSerial.Text = '' Then
    ShowMessage('Barcode Scan is Missing');
  Panel1DblClick(Sender);
  FlashSwitchClick(Sender);
End;

Procedure TmainForm.SyncLabelLedTimerTimer(Sender: TObject);
Begin
  If Not blinkMe Then
    Begin
      SyncFailedLabel.Visible := False;
      exit;
    End;

  If SyncFailedLabel.Visible = False Then
    SyncFailedLabel.Visible := True
  Else
    SyncFailedLabel.Visible := False;
End;

Procedure TmainForm.QuitMenuItemClick(Sender: TObject);
Begin
  InterruptMenuItemClick(Sender);
  Application.ProcessMessages;
  Application.Terminate;
End;

End.
