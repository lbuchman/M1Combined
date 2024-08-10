unit errorReportForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TErrorForm }

  TErrorForm = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  ErrorForm: TErrorForm;

implementation

{$R *.lfm}

{ TErrorForm }

procedure TErrorForm.FormCreate(Sender: TObject);
begin
  Label1.Font.Color := clRed;
end;

end.

