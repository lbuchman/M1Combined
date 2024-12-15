unit about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Process;

type

  { TaboutForm }

  TaboutForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    m1FirmwareVersion: TLabel;
    m1tfd1Version: TLabel;
    m1ClientVersion: TLabel;
    procedure FormShow(Sender: TObject);
  private
    fwVersionFile: string;
    procedure GetRevision(Sender: TObject; snapName: string);
  public
    procedure SetFwDirectory(fw_VersionFile: string);
  end;

var
  aboutForm: TaboutForm;

implementation

{$R *.lfm}


procedure TaboutForm.SetFwDirectory(fw_VersionFile: string);
begin
  fwVersionFile := fw_VersionFile;
end;

procedure TaboutForm.GetRevision(Sender: TObject; snapName: string);
const
  bufferSize = 1024 * 16;
var
  AProcess: TProcess;
  Buffer: array[0..bufferSize] of byte;
  BytesRead: longint;
  textToSee: ansistring;
  tmpStr: string;
  splitOutput: TStringArray;
  splitLine: TStringArray;
  Count: integer;
begin
  AProcess := TProcess.Create(nil);
  AProcess.Executable := 'snap';
  AProcess.Parameters.Add('list');
  AProcess.Parameters.Add('|');
  AProcess.Parameters.Add('grep');
  AProcess.Parameters.Add(snapName);
  AProcess.Options := AProcess.Options + [poUsePipes];
  AProcess.Execute;

  while aProcess.Running do
  begin
    Sleep(50);
    continue;
  end;

  Buffer[0] := 0;
  BytesRead := AProcess.Output.Read(Buffer, AProcess.Output.NumBytesAvailable);
  if BytesRead >= bufferSize then Buffer[bufferSize - 1] := 0
  else
    Buffer[BytesRead] := 0;
  if BytesRead = 0 then
  begin
    AProcess.Free;
    exit;
  end;

  Sleep(50);
  textToSee := '';
  SetString(textToSee, pansichar(@Buffer[0]), BytesRead);
  splitOutput := textToSee.Split(#10);
  BytesRead := length(splitOutput);

  for Count := 0 to BytesRead do
  begin
    tmpStr := splitOutput[Count];
    splitLine := tmpStr.Split(' ');
    if splitLine[0] = snapName then
    begin
      tmpStr := splitLine[2];
      TLABEL(Sender).Caption := tmpStr;
      break;
    end;
  end;

  AProcess.Free;
  AProcess := nil;
end;


{ TaboutForm }

procedure TaboutForm.FormShow(Sender: TObject);
var
  tempStringList: TStringList;
begin
  GetRevision(m1tfd1Version, 'm1tfd1');
  GetRevision(m1ClientVersion, 'm1client');
  tempStringList := TStringList.Create;
  tempStringList.LoadFromFIle(fwVersionFile);
  m1FirmwareVersion.Caption := tempStringList[0];
  tempStringList.Free;

end;

end.
