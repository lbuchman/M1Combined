unit testrunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Process, StdCtrls, IndLed, logger;

const
  ProcessTerminated = -9;
  NormalExit = 0;
  ProcessExecError = 1;
  precheckHWFailed = 15;
  OtpIsNotBlank = 10;
  EepromIsntBlank = 11;

type
  TMethodPtr = procedure(Sender: TObject) of object;

  TestingMode = (none = 0, commission = 1, re_test = 2);

  TestRecord = record
    Name: string;
    led: ^TindLed;
    progressValue: integer;
    methodPtr: TMethodPtr;
    doRetest: boolean;
  end;

  { TTestRunner }
  TTestRunner = class
  private
    FMemo: TMemo;
    FAProcess: TProcess;
    FBusyFlag: boolean;
    FTestStatus: boolean;
    FDebugLevel: string;
    FTestRet: integer;
    FTargetSerial: string;

  public
    constructor Create(Memo: TMemo; DebugLevel: string);
    destructor Destroy; override;

    function RunM1Tfc(command: string; arg: array of string; var Led: TindLed): integer;
    function RunTests(Tests: array of TestRecord; tMode: TestingMode; modeStr: ansistring;
      ResetLedsProc, AddProgressProc, DoCleanupProc, DoLabelErrorProc, InterruptProc: TMethodPtr): integer;
    function CheckSerialBarcodeScan(serial: ansistring): boolean;
    procedure DoCleanupCmd(targetSerial: string);
    function MakeTestRecord(testName: ansistring; progressValue: integer;
      methodPtr: TMethodPtr; testLed: ^TindLed; doRetest: boolean): TestRecord;

    property BusyFlag: boolean read FBusyFlag write FBusyFlag;
    property TestStatus: boolean read FTestStatus write FTestStatus;
    property TestRet: integer read FTestRet write FTestRet;
    property TargetSerial: string read FTargetSerial write FTargetSerial;
    property Process: TProcess read FAProcess write FAProcess;
  end;

implementation

constructor TTestRunner.Create(Memo: TMemo; DebugLevel: string);
begin
  inherited Create;
  FMemo := Memo;
  FDebugLevel := DebugLevel;
  FBusyFlag := False;
  FTestStatus := True;
  FTestRet := NormalExit;
  FAProcess := nil;
  FTargetSerial := '';
end;

destructor TTestRunner.Destroy;
begin
  if FAProcess <> nil then
    FAProcess.Free;
  inherited Destroy;
end;

function TTestRunner.MakeTestRecord(testName: ansistring; progressValue: integer;
  methodPtr: TMethodPtr; testLed: ^TindLed; doRetest: boolean): TestRecord;
begin
  Result.Name := testName;
  Result.led := testLed;
  Result.progressValue := progressValue;
  Result.methodPtr := methodPtr;
  Result.doRetest := doRetest;
end;

function TTestRunner.CheckSerialBarcodeScan(serial: ansistring): boolean;
var
  str: ansistring;
  intN: integer;
  ret: ansistring;
begin
  FTargetSerial := serial;  { Store parameter, don't overwrite it }
  ret := '?';
  try
    if serial.Length <> 10 then
      raise Exception.Create('Invalid Barcode Scan');
    str := AnsiMidStr(serial, 1, 2);
    case str of
      '30': ret := 'M1';
      '32': ret := 'MNP'
      else
        raise Exception.Create('Invalid Barcode Scan');
    end;

    str := AnsiMidStr(serial, 3, 2);
    intN := StrToInt(str);
    if (intN > 22) and (intN < 47) then
      ret += ' Y-20' + str
    else
      raise Exception.Create('Invalid Barcode Scan');

    str := AnsiMidStr(serial, 5, 2);
    intN := StrToInt(str);
    if (intN <= 52) and (intN >= 0) then
      ret += ' W-' + str
    else
      raise Exception.Create('Invalid Barcode Scan');

    str := AnsiMidStr(serial, 7, 4);
    intN := StrToInt(str);
    if (intN <= 9999) and (Length(str) = 4) then
      ret += ' S-' + str
    else
      raise Exception.Create('Invalid Barcode Scan');
  except
    on e: Exception do
    begin
      ShowMessage('Invalid Barcode Scan');
      Result := False;
      exit(False);
    end;
  end;
  Result := True;
end;

function TTestRunner.RunM1Tfc(command: string; arg: array of string;
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
  ExitStatus: integer;
begin
  Result := -1;
  retValue := -1;
  
  if command = 'cleanup' then
    Led.tag := 1;

  MemoCopyTxt := '';
  Led.tag := 1;
  
  { Wrap in try/finally to ensure busy flag is reset }
  try
    FBusyFlag := True;

    { Create process }
    FAProcess := TProcess.Create(nil);
    try
      FAProcess.Executable := 'm1tfd1.cli';
      
      { Check if executable exists }
      if not FileExists(FAProcess.Executable) then
        raise Exception.Create('Executable not found: ' + FAProcess.Executable);
      
      FAProcess.Parameters.Add(command);
      tmpInt := 0;

      { Add arguments with bounds checking }
      while (tmpInt < Length(arg)) and (arg[tmpInt] <> '') do
      begin
        FAProcess.Parameters.Add(arg[tmpInt]);
        Inc(tmpInt);
      end;

      FAProcess.Options := [poUsePipes];
      FAProcess.Execute;

      { Read output while process runs }
      while FAProcess.Running do
      begin
        Buffer[0] := 0;
        { Cap read size to buffer }
        BytesRead := FAProcess.Output.NumBytesAvailable;
        if BytesRead > (bufferSize - 1) then
          BytesRead := bufferSize - 1;
        
        if BytesRead > 0 then
          BytesRead := FAProcess.Output.Read(Buffer, BytesRead);
        
        if BytesRead >= bufferSize then
          Buffer[bufferSize - 1] := 0
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
        FMemo.Lines.Text := FMemo.Lines.Text + textToSee;
        MemoCopyTxt := MemoCopyTxt + textToSee;
        FMemo.SelStart := Length(FMemo.Lines.Text);
        Application.ProcessMessages;
      end;

      { Read stderr before freeing process }
      BytesRead := FAProcess.Stderr.NumBytesAvailable;
      if BytesRead > (bufferSize - 1) then
        BytesRead := bufferSize - 1;
      
      if BytesRead > 0 then
      begin
        BytesRead := FAProcess.Stderr.Read(Buffer, BytesRead);
        textToSee := '';
        SetString(textToSee, pansichar(@Buffer[0]), BytesRead);
        FMemo.Lines.Text := FMemo.Lines.Text + textToSee;
      end;

      { Get exit code before freeing }
      ExitStatus := FAProcess.ExitCode;
      retValue := ExitStatus;

      if (retValue = ProcessExecError) then
      begin
        FMemo.Lines.Add('Abnormal termination: m1tfd1.cli not found or crashed');
        Led.LedColorOff := clRed;
      end;

      if (retValue > ProcessExecError) then
      begin
        Led.LedColorOff := clRed;
        Led.Tag := 0;
      end;

      if (retValue = 0) then
        Led.LedColorOff := clLime;

      if retValue = ProcessTerminated then
      begin
        Led.LedColorOff := clRed;
        Led.Tag := 0;
      end;
      
    finally
      { Ensure process is freed }
      if FAProcess <> nil then
        FAProcess.Free;
      FAProcess := nil;
    end;
    
    Result := retValue;
    FTestRet := Result;
    
  finally
    { Always reset busy flag }
    FBusyFlag := False;
    Led.tag := 0;
    Led.LedValue := False;
  end;
end;

function TTestRunner.RunTests(Tests: array of TestRecord; tMode: TestingMode;
  modeStr: ansistring; ResetLedsProc, AddProgressProc, DoCleanupProc,
  DoLabelErrorProc, InterruptProc: TMethodPtr): integer;
var
  test: TestRecord;
  testReturnStatus: integer;
begin
  testReturnStatus := NormalExit;

  if Assigned(ResetLedsProc) then
    ResetLedsProc(nil);

  for test in Tests do
  begin
    FTestRet := NormalExit;
    if tMode <> TestingMode.commission then
    begin
      if not test.doRetest then
      begin
        if Assigned(AddProgressProc) then
          AddProgressProc(nil);
        continue;
      end;
    end;

    test.methodPtr(nil);
    if Assigned(AddProgressProc) then
      AddProgressProc(nil);

    testReturnStatus := FTestRet;
    if testReturnStatus <> NormalExit then
      break;
  end;

  if testReturnStatus <> precheckHWFailed then
  begin
    if Assigned(DoCleanupProc) then
      DoCleanupProc(nil);
  end;

  if (testReturnStatus <> NormalExit) and (testReturnStatus <> ProcessTerminated) then
  begin
    if testReturnStatus <> precheckHWFailed then
    begin
      if Assigned(DoLabelErrorProc) then
        DoLabelErrorProc(nil);
    end;
    if Assigned(ResetLedsProc) then
      ResetLedsProc(nil);
  end
  else if (testReturnStatus = NormalExit) then
  begin
    FMemo.Lines.Add(Log('info', modeStr, 'Success! All Done'));
    ShowMessage('Test Success!');
  end;

  Result := testReturnStatus;
end;

procedure TTestRunner.DoCleanupCmd(targetSerial: string);
var
  arg: array[0..8] of string;
  fakeLed: TindLed;
begin
  if targetSerial = '' then
    exit;
  if FBusyFlag then
    exit;

  arg[0] := '-s';
  arg[1] := Trim(targetSerial);
  if FTestStatus then
    arg[2] := ''
  else
  begin
    arg[2] := '-e';
    arg[3] := '';
  end;

  RunM1Tfc('cleanup', arg, fakeLed);
end;

end.
