unit LoggerForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, configuration;

type

  { TLoggerForm }

  TLoggerForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private

  public

  end;

var
  Logger: TLoggerForm;

implementation

{$R *.lfm}

{ TLoggerForm }

procedure TLoggerForm.MenuItem1Click(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TLoggerForm.FormCreate(Sender: TObject);
var
  _top, _left, _width, _height: integer;
begin
  getFormGeometry('LOGFORM-INFO', _top, _left, _width, _height);
  top := _top;
  left := _left;
  Width := _width;
  Height := _height;
end;

procedure TLoggerForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  setFormGeometry('LOGFORM-INFO', top, left, Width, Height);
end;

procedure TLoggerForm.FormDblClick(Sender: TObject);
begin

end;

end.
