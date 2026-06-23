unit ledmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IndLed, ColorProgress;

type
  { TLedManager }
  TLedManager = class
  private
    FTests: array of record
      Name: string;
      led: ^TindLed;
      progressValue: integer;
    end;
    FColorProgress: TColorProgress;
    FSyncLelLedTimerTag: integer;

  public
    constructor Create(ColorProgress: TColorProgress);
    destructor Destroy; override;

    procedure ResetLeds(Tests: array of TindLed);
    procedure AddToProgressBar(Value: integer);
    procedure LedsTimer(Tests: array of TindLed);
    procedure SyncLabelLedTimerTimer(Label1: TLabel; var blinkMe: boolean);

    property SyncTimerTag: integer read FSyncLelLedTimerTag write FSyncLelLedTimerTag;
  end;

implementation

constructor TLedManager.Create(ColorProgress: TColorProgress);
begin
  inherited Create;
  FColorProgress := ColorProgress;
  FSyncLelLedTimerTag := 0;
end;

destructor TLedManager.Destroy;
begin
  inherited Destroy;
end;

procedure TLedManager.ResetLeds(Tests: array of TindLed);
var
  i: integer;
begin
  for i := Low(Tests) to High(Tests) do
  begin
    Tests[i].LedColorOff := clGray;
  end;
end;

procedure TLedManager.AddToProgressBar(Value: integer);
var
  progress: integer;
begin
  if FColorProgress = nil then
    exit;

  progress := FColorProgress.Progress;
  progress := progress + Value;
  if progress > 100 then
    progress := 100;
  FColorProgress.Progress := progress;
end;

procedure TLedManager.LedsTimer(Tests: array of TindLed);
var
  i: integer;
begin
  if FSyncLelLedTimerTag = 1 then
  begin
    FSyncLelLedTimerTag := 0;
    exit;
  end;

  for i := Low(Tests) to High(Tests) do
  begin
    if Tests[i].tag = 1 then
    begin
      if not Tests[i].LedValue then
      begin
        if Tests[i].LedColorOff = clYellow then
          Tests[i].LedColorOff := clGray
        else
          Tests[i].LedColorOff := clYellow;
      end;
    end;
  end;
end;

procedure TLedManager.SyncLabelLedTimerTimer(Label1: TLabel; var blinkMe: boolean);
begin
  if not blinkMe then
  begin
    Label1.Visible := False;
    exit;
  end;

  if Label1.Visible = False then
    Label1.Visible := True
  else
    Label1.Visible := False;
end;

end.
