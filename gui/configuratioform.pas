unit configuratioForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  configurationjson;

type

  { TConfigForm }

  TConfigForm = class(TForm)
    M1TfcExecutable: TEdit;
    ictFWFilePathEdit: TEdit;
    FlashLayoutFilePath: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    UidCountLabel: TLabel;
    UidCountEdit: TEdit;
    RunInFullScreenEdit: TEdit;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    VendorSiteEdit1: TEdit;
    SaveConfigMenuItem: TMenuItem;
    ProgrammingCommandEdit: TEdit;
    ScannerUdpPortEdit: TEdit;
    M1SerialDevEdit: TEdit;
    LogDirrectoryEdit: TEdit;
    M1TerminalDevEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    UidRangeStartEdit: TEdit;
  private

  public
    procedure load(configuration: TConfigration);

  end;

var
  ConfigForm: TConfigForm;

implementation

{$R *.lfm}

procedure TConfigForm.load(configuration: TConfigration);
begin
  M1TfcExecutable.Text := configuration.M1TfcExecutable;
  ictFWFilePathEdit.Text := configuration.IctFWFilePath;
  FlashLayoutFilePath.Text := configuration.LayoutFilePath;
  ProgrammingCommandEdit.Text := configuration.STM32_Programmer_CLI;
  ScannerUdpPortEdit.Text := configuration.ScannerUdpPort;
  M1SerialDevEdit.Text := configuration.M1SerialDev;
  LogDirrectoryEdit.Text := configuration.Logdir;
  M1TerminalDevEdit.Text := configuration.TestBoardTerminalDev;
  VendorSiteEdit1.Text := configuration.VendorSite;
  UidRangeStartEdit.Text := configuration.UidStartRange;
end;

end.
