unit relay;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls, AdvLed;

type
  TRelay = class(TAdvLed)
  private
    relayName: TLabel;

  public
    constructor Create(AOwner: TComponent; _top: integer; _left: integer;
      labelCaption: string); overload;
    destructor Destroy; override;
    procedure setOn();
    procedure setOff();
  end;

implementation

destructor TRelay.Destroy;
begin
  relayName.Free;
  inherited;
end;

constructor TRelay.Create(AOwner: TComponent; _top: integer; _left: integer;
  labelCaption: string);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  top := _top;
  left := _left;
  kind := lkRedLight;
  relayName := TLabel.Create(AOwner);
  relayName.Parent := TWinControl(AOwner);
  relayName.ParentFont := true;
  relayName.Top := top + 30;
  relayName.Left := left - 15;
  relayName.Visible := True;
  relayName.Caption := labelCaption;
  relayName.Font.Style := [fsBold];
  relayName.Font.Size := 9;

end;

procedure TRelay.setOn();
begin
  state := lsDisabled;
end;

procedure TRelay.setOff();
begin
  state := lsOn;
end;


end.
