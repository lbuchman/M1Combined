unit logForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TLoggerForm }

  TLoggerForm = class(TForm)
    Memo1: TMemo;
    procedure FormClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure Memo1DblClick(Sender: TObject);
  private

  public

  end;

var
  LoggerForm: TLoggerForm;

implementation

{$R *.lfm}

{ TLoggerForm }


procedure TLoggerForm.Memo1DblClick(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TLoggerForm.FormClick(Sender: TObject);
begin

end;

procedure TLoggerForm.Memo1Change(Sender: TObject);
begin

end;

procedure TLoggerForm.Memo1Click(Sender: TObject);
begin
  Memo1.Lines.LoadFromFile(Caption);
end;

end.
