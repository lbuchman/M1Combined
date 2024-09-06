unit configurationForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, MaskEdit, Menus, Buttons, ComCtrls, IndLed, dateutils,
  CheckBoxThemed, SpinEx, configurationJson, fpJSON, jsonparser;

type

  { TConfigForm }

  TConfigForm = class(TForm)
    CalibrateCheckBoxThemed: TCheckBoxThemed;
    DaTablelLabel: TLabel;
    SerialcEdit: TEdit;
    FwRevLabel: TLabel;
    FwRevEdit: TEdit;
    BoardDescEdit: TEdit;
    CalibInputSelect: TSpinEditEx;
    SerialLabel: TLabel;
    PwmValueSelectGroupBox: TPanel;
    pwm1KRadioButton: TRadioButton;
    pwm2KRadioButton: TRadioButton;
    InputSelectLabel: TLabel;
    SpinEditExSpin: TSpinEditEx;
    AutoCalibtButton: TButton;
    UpdateDataLabel: TLabel;
    PWMSetDataLabel: TLabel;
    UpdateEnableLed: TindLed;
    NTPStatusLabel: TLabel;
    TimeServerEdit: TEdit;
    NTPStatusEdit: TEdit;
    TimeServerLabel: TLabel;
    OpenDialog1: TOpenDialog;
    RebootButton: TButton;
    SaveDialog1: TSaveDialog;
    TimeSetButton: TButton;
    BoardDescSaveButton: TButton;
    UpTimeEdit: TEdit;
    ControllerMaclLabel2: TLabel;
    GroupBox3: TGroupBox;
    ADTableStringGrid: TStringGrid;
    MacEdit: TEdit;
    ControllerMaclLabel: TLabel;
    ControllerModelCombo: TComboBox;
    ControllerModelLabel: TLabel;
    ControllerTimeEdit: TEdit;
    ControllerTimelLabel: TLabel;
    GroupBox2: TGroupBox;
    PersistButton: TButton;
    SaveToFileButton: TButton;
    LoadFromFileButton: TButton;
    Timer1: TTimer;
    UpdateNetworkButton: TButton;
    IplLabel: TLabel;
    GwlLabel: TLabel;
    MacklLabel: TLabel;
    DnslLabel: TLabel;
    MaskAddressEdit: TEdit;
    IpAddressEdit: TEdit;
    GroupBox1: TGroupBox;
    GwAddressEdit: TEdit;
    DnsAddressEdit: TEdit;
    NetworkDHCPCkeckBox: TCheckBoxThemed;
    NTPStateCheckBox: TCheckBoxThemed;
    SaveDA2220Button: TButton;
    procedure AutoCalibtButtonClick(Sender: TObject);
    procedure BoardDescSaveButtonClick(Sender: TObject);
    procedure ControllerModelComboChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadFromFileButtonClick(Sender: TObject);
    procedure ModelChangeButtonClick(Sender: TObject);
    procedure NTPStateCheckBoxChange(Sender: TObject);
    procedure PersistButtonClick(Sender: TObject);
    procedure RebootButtonClick(Sender: TObject);
    procedure SaveDAButtonClick(Sender: TObject);
    procedure SaveToFileButtonClick(Sender: TObject);
    procedure SpinEditExSpinChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TimeSetButtonClick(Sender: TObject);
    procedure UpdateDataButtonClick(Sender: TObject);
    procedure UpdateEnableLedClick(Sender: TObject);
    procedure UpdateNetworkButtonClick(Sender: TObject);
  private
    updateFormdata: boolean;
    EEPROMFileName: string;
    saveEEPROM: boolean;
    procedure saveEEToFle(var jData: TJSONData);
  public
    procedure SaveGeometry();
    procedure UpdateData(var jData: TJSONData; cmd: string);
  end;

var
  ConfigForm: TConfigForm;

const
  settingStructRev = 1;

implementation

{$R *.lfm}
uses mainForm;

{ TConfigForm }

procedure TConfigForm.SaveGeometry();
begin
  setFormGeometryJson('configForm', top, left, Width, Height);
end;

procedure TConfigForm.FormCreate(Sender: TObject);
var
  _top, _left, _width, _height: integer;
  Count: integer;
begin
  updateFormdata := True;
  _top := 0;
  _left := 0;
  _width := 0;
  _height := 0;
  getFormGeometryJson('configForm', _top, _left, _width, _height);
  top := _top;
  left := _left;
  Width := _width;
  Height := _height;
  ADTableStringGrid.Options := ADTableStringGrid.Options + [goEditing];
  for Count := 1 to 8 do
  begin
    ADTableStringGrid.Cells[0, Count] := 'Input' + IntToStr(Count);
  end;
  ADTableStringGrid.Cells[1, 0] := 'pwm1K';
  ADTableStringGrid.Cells[2, 0] := 'pwm2K';
  ADTableStringGrid.Cells[3, 0] := 'pwmCut';
  ADTableStringGrid.Cells[4, 0] := 'pwmShort';
  SaveDA2220Button.Top := GroupBox3.Height - 60;
  UpdateNetworkButton.Top := GroupBox1.Height - 60;
end;

procedure TConfigForm.ControllerModelComboChange(Sender: TObject);
begin
  MainAppForm.SendCommand('setcontrollermodel', ControllerModelCombo.Text);
end;

procedure TConfigForm.BoardDescSaveButtonClick(Sender: TObject);
begin
  MainAppForm.SendCommand('setboarddesc', QuotedStr(BoardDescEdit.Text));
  MainAppForm.setDeviceDesc(BoardDescEdit.Text);
end;

procedure TConfigForm.AutoCalibtButtonClick(Sender: TObject);
begin
     MainAppForm.SendCommand('calibrateinputs', 'null');
end;

procedure TConfigForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := CloseAction;
  Timer1.Enabled := False;
end;

procedure TConfigForm.FormShow(Sender: TObject);
var
  _top, _left, _width, _height: integer;
begin
  Timer1.Enabled := True;
  _top := 0;
  _left := 0;
  _width := 0;
  _height := 0;
   MainAppForm.SendCommand('printconfiguration', 'null');
   getFormGeometryJson('configForm', _top, _left, _width, _height);
  top := _top;
  left := _left;
  Width := _width;
  Height := _height;
end;

procedure TConfigForm.LoadFromFileButtonClick(Sender: TObject);
var
  base64: string;
  eepromFile: TextFile;
begin
  OpenDialog1.FileName := 'eeprom_' + SerialcEdit.Text;
  if OpenDialog1.Execute then
  begin
    AssignFile(eepromFile, OpenDialog1.FileName);
    try
      // Open the file for reading
      reset(eepromFile);
      readln(eepromFile, base64);
    except
      on E: EInOutError do

    end;
    CloseFile(eepromFile);
    MainAppForm.SendCommand('saveeeprombinary', base64);
  end;
end;

procedure TConfigForm.ModelChangeButtonClick(Sender: TObject);
begin
  MainAppForm.SendCommand('setcontrollermodel', ControllerModelCombo.Text);
end;

procedure TConfigForm.NTPStateCheckBoxChange(Sender: TObject);
begin
  if NTPStateCheckBox.Checked then  MainAppForm.SendCommand('setntpswitch', '1')
  else
    MainAppForm.SendCommand('setntpswitch', '0');
end;

procedure TConfigForm.PersistButtonClick(Sender: TObject);
begin
  MainAppForm.SendCommand('saveconfig', 'null');
end;

procedure TConfigForm.RebootButtonClick(Sender: TObject);
begin
  MainAppForm.SendCommand('reboot', 'null');
end;

procedure TConfigForm.SaveDAButtonClick(Sender: TObject);
var
  Count: integer;
  Controller : Integer;
begin
  Controller := 3;
  if ControllerModelCombo.Text = 'MER-Ser2' then Controller := 0;
  if ControllerModelCombo.Text = 'MER-Ser3' then Controller := 1;
  if ControllerModelCombo.Text = 'S2-Contr' then Controller := 2;
  for Count := 1 to 8 do
  begin
    MainAppForm.SendCommand('setpwm', IntToStr(Controller) + ' ' + IntToStr(Count) + ' ' +
      ADTableStringGrid.Cells[1, Count] + ' ' + ADTableStringGrid.Cells[2, Count] + ' ' + ADTableStringGrid.Cells[3, Count] + ' ' + ADTableStringGrid.Cells[4, Count]);
  end;
end;

procedure TConfigForm.SaveToFileButtonClick(Sender: TObject);
begin
  SaveDialog1.FileName := 'eeprom_' + SerialcEdit.Text;
  if SaveDialog1.Execute then
  begin
    MainAppForm.SendCommand('geteeeprombinary', 'null');
    EEPROMFileName := SaveDialog1.FileName;
    saveEEPROM := True;
  end;
end;

procedure TConfigForm.SpinEditExSpinChange(Sender: TObject);
var
  sellIndex: integer;
begin
  if not CalibrateCheckBoxThemed.Checked then exit;
  if pwm1KRadioButton.Checked then sellIndex := 1
  else
    sellIndex := 2;
  MainAppForm.SendCommand('trypwm', ' ' + IntToStr(CalibInputSelect.Value) +
    ' ' + IntToStr(SpinEditExSpin.Value));
  ADTableStringGrid.Cells[sellIndex, CalibInputSelect.Value] :=
    IntToStr(SpinEditExSpin.Value);
end;

procedure TConfigForm.Timer1Timer(Sender: TObject);
begin
  if NetworkDHCPCkeckBox.Checked then
  begin
    IpAddressEdit.Enabled := False;
    GwAddressEdit.Enabled := False;
    MaskAddressEdit.Enabled := False;
    DnsAddressEdit.Enabled := False;
  end
  else
  begin
    IpAddressEdit.Enabled := True;
    GwAddressEdit.Enabled := True;
    MaskAddressEdit.Enabled := True;
    DnsAddressEdit.Enabled := True;
  end;

  if updateFormdata then MainAppForm.SendCommand('printconfiguration', 'null');
end;

procedure TConfigForm.TimeSetButtonClick(Sender: TObject);
var
  epoch: int64;
begin
  epoch := DateTimeToUnix(Now, True);
  MainAppForm.SendCommand('settime', IntToStr(epoch));
end;

procedure TConfigForm.UpdateDataButtonClick(Sender: TObject);
begin
  MainAppForm.SendCommand('printconfiguration', 'null');
end;

procedure TConfigForm.UpdateEnableLedClick(Sender: TObject);
begin
  if UpdateEnableLed.LedValue then updateFormdata := True
  else
    updateFormdata := False;
end;

procedure TConfigForm.UpdateNetworkButtonClick(Sender: TObject);
var
  arg: string;
begin
  if NetworkDHCPCkeckBox.Checked then arg := '1'
  else
    arg := '0';
  arg := arg + ' ' + IpAddressEdit.Text;
  arg := arg + ' ' + MaskAddressEdit.Text;
  arg := arg + ' ' + GwAddressEdit.Text;
  arg := arg + ' ' + DnsAddressEdit.Text;
  if NTPStateCheckBox.Checked then arg := arg + ' 1'
  else
    arg := arg + ' 0';
  arg := arg + ' ' + TimeServerEdit.Text;
  arg := arg + ' 0';
  MainAppForm.SendCommand('setnetwork', arg);
end;

procedure TConfigForm.saveEEToFle(var jData: TJSONData);
var
  jss: string;
  eepromFile: TextFile;
begin
  jss := jData.FindPath('eeprom').AsString;
  AssignFile(eepromFile, EEPROMFileName);
  try
    rewrite(eepromFile);
  except
    on E: EInOutError do
      exit;
  end;

  writeln(eepromFile, jss);
  CloseFile(eepromFile);
end;

procedure TConfigForm.UpdateData(var jData: TJSONData; cmd: string);
var
  jArray: TJSonArray;
  jArray1: TJSonData;
  jNetwork: TJSonData;
  i: integer;
  controllerPwmname: string;
begin
  try
    if cmd = 'geteeeprombinary' then
    begin
      if saveEEPROM then
      begin
        saveEEToFle(jData);
        saveEEPROM := False;
      end;
    end;

    if (cmd <> 'printconfiguration') then exit;

    ControllerModelCombo.Text := jData.FindPath('controller').AsString;
    MacEdit.Text := jData.FindPath('mac').AsString;

    NTPStateCheckBox.Checked := jData.FindPath('ntp').AsBoolean;
    SerialcEdit.Text := jData.FindPath('serialnumber').AsString;
    UpTimeEdit.Text := jData.FindPath('uptime').AsString;
    ControllerTimeEdit.Text := jData.FindPath('timenow').AsString;
    FwRevEdit.Text := jData.FindPath('fwrev').AsString;
    NTPStatusEdit.Text := jData.FindPath('ntpStatus').AsString;

    TimeServerEdit.Text := jData.FindPath('timeServer').AsString;

    BoardDescEdit.Text := jData.FindPath('boardServiceId').AsString;

    jNetwork := jData.FindPath('network');
    IpAddressEdit.Text := jNetwork.FindPath('ip').AsString;
    NetworkDHCPCkeckBox.Checked := jNetwork.FindPath('dhcp').AsBoolean;
    GwAddressEdit.Text := jNetwork.FindPath('gateway').AsString;
    MaskAddressEdit.Text := jNetwork.FindPath('subnet').AsString;
    DnsAddressEdit.Text := jNetwork.FindPath('dns').AsString;

    case ControllerModelCombo.Text of
      'MER-Ser3': controllerPwmname := 'MER-Ser3';
      'MER-Ser2': controllerPwmname := 'MER-Ser2';
      'S2-Contr': controllerPwmname := 'S2-Contr';
    end;
    jArray := TJSONArray(jData.FindPath(controllerPwmname));
    if (jArray = nil) then exit;
    for i := 0 to Pred(jArray.Count) do
    begin
      jArray1 := TJSONArray(jArray.Items[i]);
      controllerPwmname := jArray1.Items[0].AsString;
      controllerPwmname := jArray1.Items[1].AsString;
      ADTableStringGrid.Cells[1, i + 1] := jArray1.Items[0].AsString;
      ADTableStringGrid.Cells[2, i + 1] := jArray1.Items[1].AsString;
      ADTableStringGrid.Cells[3, i + 1] := jArray1.Items[2].AsString;
      ADTableStringGrid.Cells[4, i + 1] := jArray1.Items[3].AsString;
    end;

  except
    on E: Exception do ;
  end;
end;

end.
