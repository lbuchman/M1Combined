unit outputs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  jsonparser, fpJSON, relay, configurationJson, iovalues;

type

  { TRelays }

  TRelays = class(TForm)
    pullTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pullTimerTimer(Sender: TObject);
  private
    relays: array [0..9] of TRelay;
  public
    procedure SaveGeometry();
  end;

var
  Relays: TRelays;

implementation

uses mainForm;

{$R *.lfm}

{ TRelays }

procedure TRelays.SaveGeometry();
begin
  setFormGeometryJson('relayForm', top, left, Width, Height);
end;

procedure TRelays.pullTimerTimer(Sender: TObject);
var
  i: integer;
  Value: integer;
  ioValues: TIOValues;
  retStatus: boolean;
begin
  Initialize(@ioValues, SizeOf(ioValues));
  retStatus := MainAppForm.getIoValues(ioValues, relaysFormDest);
  if not retStatus then exit;
  for i := 0 to 9 do
  begin
    Value := ioValues.outputs[i];
    if Value = 1 then relays[i].setOn()
    else
      relays[i].setOff();
  end;

end;

procedure TRelays.FormCreate(Sender: TObject);
var
  baseTop, baseLeft, deltaX, deltaY: integer;
begin
  baseLeft := 40;
  baseTop := 40;
  deltaX := 230;
  deltaY := 60;

  relays[0] := TRelay.Create(self, baseTop, baseLeft, 'Relay 1');
  relays[1] := TRelay.Create(self, baseTop + deltaY, baseLeft, 'Relay 2');
  relays[2] := TRelay.Create(self, baseTop + deltaY * 2, baseLeft, 'Relay 3');
  relays[3] := TRelay.Create(self, baseTop + deltaY * 3, baseLeft, 'Relay 4');

  relays[4] := TRelay.Create(self, baseTop, baseLeft + deltaX, 'Relay 5');
  relays[5] := TRelay.Create(self, baseTop + deltaY, baseLeft + deltaX, 'Relay 6');
  relays[6] := TRelay.Create(self, baseTop + deltaY * 2, baseLeft + deltaX, 'Relay 7');
  relays[7] := TRelay.Create(self, baseTop + deltaY * 3, baseLeft + deltaX, 'Relay 8');

  relays[8] := TRelay.Create(self, baseTop, baseLeft + (deltaX shr 1), 'Relay 9');
  relays[9] := TRelay.Create(self, baseTop + deltaY, baseLeft +
    (deltaX shr 1), 'Relay 10');

end;

procedure TRelays.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to 9 do relays[i].Free;
end;

procedure TRelays.FormShow(Sender: TObject);
var
  _top, _left, _width, _height: integer;
begin
  _top := 0;
  _left := 0;
  _width := 0;
  _height := 0;
  Font.Size := 7;
  getFormGeometryJson('relayForm', _top, _left, _width, _height);
  top := _top;
  left := _left;
  Width := _width;
  Height := _height;

end;


end.
