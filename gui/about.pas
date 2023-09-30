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
    Label6: TLabel;
    m1FirmwareVersion: TLabel;
    m1tfd1Version: TLabel;
    m1ClientVersion: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  aboutForm: TaboutForm;

implementation

{$R *.lfm}

{ TaboutForm }

procedure TaboutForm.FormShow(Sender: TObject);
const
bufferSize = 512;
var
  AProcess: TProcess;
  Buffer: array[0..bufferSize] of byte;
  BytesRead: longint;
  textToSee: ansistring;
  splitOutput: TStringArray;
begin
    // SaveToFile(GetEnvironmentVariable('HOME') + '/m1mtf/m1tfd1app.pid');
  m1tfd1Version.Caption := '';
  AProcess := TProcess.Create(nil);
  AProcess.Executable := 'snap';
  AProcess.Parameters.Add('list');

  AProcess.Options := AProcess.Options + [poUsePipes];
  AProcess.Execute;

  while aProcess.Running do
  begin
    Application.ProcessMessages;
    Sleep(50);
    continue;
 end;

  Buffer[0] := 0;
  BytesRead := AProcess.Output.Read(Buffer, AProcess.Output.NumBytesAvailable);
  if BytesRead >= bufferSize then Buffer[bufferSize - 1] := 0
  else
    Buffer[BytesRead] := 0;

  Sleep(50);
  textToSee := '';
  SetString(textToSee, pansichar(@Buffer[0]), BytesRead);
  splitOutput := textToSee.Split(#10);

  m1tfd1Version.Caption := textToSee;


  AProcess.Free;
  AProcess := nil;
end;

end.

