unit blinkLed;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls,
  ComCtrls, Graphics, AdvLed;

type
  TBlinkLed = class(TAdvLed)
  private

  public
    constructor Create(AOwner: TComponent; _top, _left: integer;
      _kind: TLedKind); overload;
    destructor Destroy; override;
    procedure BlinkNow(interval: integer);
  private
    UpdateTimer1: TTimer;
    blinkInprogress: boolean;
    procedure UpdateTimer1Timer(Sender: TObject);
  end;

implementation

destructor TBlinkLed.Destroy;
begin
  UpdateTimer1.Free;
  inherited;
end;

constructor TBlinkLed.Create(AOwner: TComponent; _top, _left: integer; _kind: TLedKind);
begin
  inherited Create(AOwner);
  blinkInprogress := False;
  top := _top;
  left := _left;
  kind := _kind;
  Parent := TWinControl(AOwner);
  UpdateTimer1 := TTimer.Create(nil);
  UpdateTimer1.Enabled := False;
  UpdateTimer1.OnTimer := @UpdateTimer1Timer;
  Visible := True;
end;

procedure TBlinkLed.UpdateTimer1Timer(Sender: TObject);
begin
  state := lsDisabled;
  blinkInprogress := False;
end;


procedure TBlinkLed.BlinkNow(interval: integer);
begin
  if (blinkInprogress) then exit;
  blinkInprogress := True;
  UpdateTimer1.Enabled := False;
  UpdateTimer1.Enabled := True;
  UpdateTimer1.Interval := interval;
  state := lsOn;
end;

end.
