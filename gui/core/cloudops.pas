unit cloudops;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Process, StdCtrls, DateUtils, logger;

type
  { TCloudOperations }
  TCloudOperations = class
  private
    FMemo: TMemo;
    FBlinkMe: boolean;

  public
    constructor Create(Memo: TMemo);
    destructor Destroy; override;

    procedure PushLogs;
    procedure PushSecrets;
    procedure PublishLog;
    procedure UpdateMe;
    procedure CheckCloudUpdateTimer(UpdateSycretsTimeStamp, UpdateLogsTimeStamp, UpdateFwTimeStamp: string; Interval7Days: integer);

    property BlinkMe: boolean read FBlinkMe write FBlinkMe;
  end;

implementation

constructor TCloudOperations.Create(Memo: TMemo);
begin
  inherited Create;
  FMemo := Memo;
  FBlinkMe := False;
end;

destructor TCloudOperations.Destroy;
begin
  inherited Destroy;
end;

procedure ExecuteSnapCommand(Memo: TMemo; Command: string; Parameters: array of string);
const
  bufferSize = 1024 * 16;
var
  aLocalProcess: TProcess;
  Buffer: array[0..bufferSize] of byte;
  BytesRead: longint;
  textToSee: ansistring;
  i: integer;
begin
  aLocalProcess := TProcess.Create(nil);
  aLocalProcess.Executable := '/snap/bin/m1client';
  aLocalProcess.Parameters.Add(Command);

  for i := Low(Parameters) to High(Parameters) do
    if Parameters[i] <> '' then
      aLocalProcess.Parameters.Add(Parameters[i]);

  aLocalProcess.Options := aLocalProcess.Options + [poUsePipes];
  aLocalProcess.Execute;

  while aLocalProcess.Running do
  begin
    Sleep(50);
  end;

  Buffer[0] := 0;
  BytesRead := aLocalProcess.Output.NumBytesAvailable;
  if BytesRead > (bufferSize - 1) then
  begin
    aLocalProcess.Free;
    exit;
  end;

  BytesRead := aLocalProcess.Output.Read(Buffer, BytesRead);
  if BytesRead >= bufferSize then
    Buffer[bufferSize - 1] := 0
  else
    Buffer[BytesRead] := 0;

  Sleep(50);
  textToSee := '';
  SetString(textToSee, pansichar(@Buffer[0]), BytesRead);
  Memo.Lines.Add(Log('info', Command, textToSee));
  aLocalProcess.Free;
  aLocalProcess := nil;
end;

procedure TCloudOperations.PushLogs;
var
  params: array[0..0] of string;
begin
  params[0] := '';
  ExecuteSnapCommand(FMemo, 'synclogs', params);
end;

procedure TCloudOperations.PushSecrets;
var
  params: array[0..0] of string;
begin
  params[0] := '';
  ExecuteSnapCommand(FMemo, 'syncsecrets', params);
end;

procedure TCloudOperations.PublishLog;
var
  params: array[0..0] of string;
begin
  params[0] := '';
  ExecuteSnapCommand(FMemo, 'synclogs', params);
end;

procedure TCloudOperations.UpdateMe;
var
  params: array[0..0] of string;
begin
  params[0] := '';
  ExecuteSnapCommand(FMemo, 'update', params);
end;

procedure TCloudOperations.CheckCloudUpdateTimer(UpdateSycretsTimeStamp, UpdateLogsTimeStamp, UpdateFwTimeStamp: string; Interval7Days: integer);
var
  epochTime: int64;
  epochTimeNow: int64;
  dateTimeNow: TDateTime;
  dateTimeFromFile: TDateTime;
begin
  FBlinkMe := False;
  dateTimeNow := Now;
  epochTimeNow := DateTimeToUnix(dateTimeNow, False);

  // Check secrets update
  if UpdateSycretsTimeStamp <> '' then
  begin
    // epochTime := DateTimeToUnix(GetDateTimeFromFile(UpdateSycretsTimeStamp));
    // if (epochTimeNow - epochTime) > Interval7Days then FBlinkMe := True;
  end;

  // Check logs update
  if UpdateLogsTimeStamp <> '' then
  begin
    // epochTime := DateTimeToUnix(GetDateTimeFromFile(UpdateLogsTimeStamp));
    // if (epochTimeNow - epochTime) > Interval7Days then FBlinkMe := True;
  end;
end;

end.
