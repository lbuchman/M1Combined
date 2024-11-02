unit Door2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, MaskEdit, SpinEx, Sensors, AdvLed, configurationJson, fpJSON, iovalues,
  jsonparser;

type

  { TDoor2Form }

  TDoor2Form = class(TForm)
    AccessCardPicture: TImage;
    Key0: TButton;
    CardFormatCombo: TComboBox;
    ClosedDoor: TImage;
    CutImageDoor: TImage;
    CutImageRex: TImage;
    DoorSensorDefaultStateCombo: TComboBox;
    FormatLabel: TLabel;
    Key1: TButton;
    Key10: TButton;
    Key11: TButton;
    Key2: TButton;
    Key3: TButton;
    Key4: TButton;
    Key5: TButton;
    Key6: TButton;
    Key7: TButton;
    Key8: TButton;
    Key9: TButton;
    KeyPadLabel: TLabel;
    KeyPadModeCombo: TComboBox;
    DiDefaultLabel: TLabel;
    OpenDoor: TImage;
    Panel1: TPanel;
    Panel2: TPanel;
    Rd1SwipeLabel14: TLabel;
    Reader: TImage;
    ReaderBlueLed: TAdvLed;
    ReaderpropReadButton: TButton;
    ReaderpropSaveButton: TButton;
    ReaderRedLed: TAdvLed;
    RexLed: TAdvLed;
    RexSensor: TImage;
    RexSensorDefaultStateCombo: TComboBox;
    ShortImageDoor: TImage;
    ShortImageRex: TImage;
    StopLightSensor1: TStopLightSensor;
    SwipeDataBinaryData: TEdit;
    ReaderpropWriteButton: TButton;
    Rd1SwipeLabel5: TLabel;
    Rd1SwipeLabel6: TLabel;
    ReaderTypeCombo: TComboBox;
    DoorSensorModeCombo: TComboBox;
    RexSensorModeCombo: TComboBox;
    StrikeDefaultStateCombo: TComboBox;
    LinkDStoStrikeCheckbox: TCheckBox;
    ReaderPropGroupBox: TGroupBox;
    Rd1Badge: TEdit;
    RdDsHold: TEdit;
    RdDsWait: TEdit;
    Rd1DsWait1: TEdit;
    Rd1FC: TEdit;
    DiTypeLabel: TLabel;
    Rd1SwipeLabel11: TLabel;
    Rd1SwipeLabel12: TLabel;
    Rd1SwipeLabel2: TLabel;
    Rd1SwipeLabel9: TLabel;
    Rd1WaitDsActivate3: TLabel;
    Rd1WaitDsActivate6: TLabel;
    AccsessCardPanel: TPanel;
    Timer1: TTimer;
    procedure ClosedDoorClick(Sender: TObject);
    procedure CutImageRexClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure KeypadKeyClick(Sender: TObject);
    procedure ReaderpropReadButtonClick(Sender: TObject);
    procedure ReaderpropSaveButtonClick(Sender: TObject);
    procedure ReaderpropWriteButtonClick(Sender: TObject);
    procedure RexSensorModeComboChange(Sender: TObject);
    procedure ShortImageDoorClick(Sender: TObject);
    procedure CutImageDoorClick(Sender: TObject);
    procedure OpenDoorClick(Sender: TObject);
    procedure Reader1DblClick(Sender: TObject);
    procedure ShortImageRexClick(Sender: TObject);
    procedure StrikeDefaultStateComboChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RexSensorDblClick(Sender: TObject);
  private
    ioValues: TIOValues;
  public
    procedure SaveGeometry();
    procedure updateData(var jData: TJSONData; cmd: string);
  end;

var
  Door2Form: TDoor2Form;

implementation

uses mainForm;

const

{$DEFINE DOOR2}

{$IFDEF DOOR1}
  DoorNumber = 1;
  FormDest = door1FormDest;
  DS_Number = 1;
  DS_Index = 0;
  Rex_Number = 2;
  Rex_Index = 1;
  ReaderGreenLedIndex = 0;
  ReaderRedLedIndex = 1;
  Strike_Index = 0;

{$ELSE}
    DoorNumber = 2;
    FormDest = door2FormDest;
    DS_Number =  5;
    DS_Index  =  4;
    Rex_Number = 6;
    Rex_Index  = 5;
    ReaderGreenLedIndex  = 2;
    ReaderRedLedIndex = 3;
    Strike_Index = 2;
{$ENDIF}

{$R *.lfm}

{ TDoor2Form }

procedure TDoor2Form.SaveGeometry();
begin
  setFormGeometryJson('reader2Form', top, left, Width, Height);
end;

procedure TDoor2Form.FormShow(Sender: TObject);
var
  _top, _left, _width, _height: integer;
begin
  _top := 0;
  _left := 0;
  _width := 0;
  _height := 0;
  getFormGeometryJson('reader2Form', _top,
   _left, _width, _height);
  top := _top;
  left := _left;
  Width := _width;
  Height := _height;
  ReaderBlueLed.State := lson;
  ReaderRedLed.State := lsoff;
  ReaderpropReadButtonClick(Sender);
end;


procedure TDoor2Form.KeypadKeyClick(Sender: TObject);
begin
  MainAppForm.SendCommand('keypadpress', IntToStr(DoorNumber) +
    ' ' + IntToStr(TButton(Sender).Tag));
end;

procedure TDoor2Form.ReaderpropReadButtonClick(Sender: TObject);
begin
  ReaderTypeCombo.Text := '';
  DoorSensorModeCombo.Text := '';
  DoorSensorDefaultStateCombo.Text := '';
  RexSensorDefaultStateCombo.Text := '';
  StrikeDefaultStateCombo.Text := '';
  RexSensorModeCombo.Text := '';
  KeyPadModeCombo.Text := '';
  RdDsWait.Text := '';
  RdDsWait.Text := '';
  RdDsHold.Text := '';
  LinkDStoStrikeCheckbox.Checked := False;
  MainAppForm.SendCommand('getreaderconfig', IntToStr(DoorNumber));
end;

procedure TDoor2Form.ReaderpropSaveButtonClick(Sender: TObject);
begin
  MainAppForm.SendCommand('saveconfig', 'null');
end;


procedure TDoor2Form.ReaderpropWriteButtonClick(Sender: TObject);
var
  cmd, arg: string;
begin
  cmd := 'setreaderconfig';
  arg := IntToStr(DoorNumber) + ' ';

  arg := arg + IntToStr(ReaderTypeCombo.Items.IndexOf(ReaderTypeCombo.Text)) + ' ';
  arg := arg + IntToStr(DoorSensorModeCombo.Items.IndexOf(
    DoorSensorModeCombo.Text)) + ' ';
  arg := arg + IntToStr(DoorSensorDefaultStateCombo.Items.IndexOf(
    DoorSensorDefaultStateCombo.Text)) + ' ';
  arg := arg + IntToStr(RexSensorModeCombo.Items.IndexOf(RexSensorModeCombo.Text)) + ' ';
  arg := arg + IntToStr(RexSensorDefaultStateCombo.Items.IndexOf(
    RexSensorDefaultStateCombo.Text)) + ' ';
  arg := arg + IntToStr(StrikeDefaultStateCombo.Items.IndexOf(
    StrikeDefaultStateCombo.Text)) + ' ';
  if KeyPadModeCombo.Items.IndexOf(KeyPadModeCombo.Text) = 0 then
    arg := arg + IntToStr(4) + ' '
  else
    arg := arg + IntToStr(8) + ' ';

  arg := arg + RdDsWait.Text + ' ';
  arg := arg + RdDsHold.Text + ' ';
  if LinkDStoStrikeCheckbox.Checked then arg := arg + '1 '
  else
    arg := arg + '0 ';

  MainAppForm.SendCommand(cmd, arg);
end;

procedure TDoor2Form.RexSensorModeComboChange(Sender: TObject);
begin

end;

procedure TDoor2Form.ShortImageDoorClick(Sender: TObject);
begin
  MainAppForm.SendCommand('setsupervisedinput', IntToStr(DS_Number) + ' 2');
end;

procedure TDoor2Form.CutImageDoorClick(Sender: TObject);
begin
  MainAppForm.SendCommand('setsupervisedinput', IntToStr(DS_Number) + ' 3');
end;

procedure TDoor2Form.OpenDoorClick(Sender: TObject);
begin
  MainAppForm.SendCommand('setsupervisedinput', IntToStr(DS_Number) + ' 1');
end;

procedure TDoor2Form.Reader1DblClick(Sender: TObject);
begin
  if CardFormatCombo.Text = 'Raw' then
      MainAppForm.SendCommand('swipe', ' ' + IntToStr(DoorNumber) +
        ' ' + SwipeDataBinaryData.Text + ' ' + RdDsWait.Text +
        ' ' + RdDsHold.Text)
  else
      MainAppForm.SendCommand('swipe26', ' ' + IntToStr(DoorNumber) +
        ' ' + Rd1FC.Text + ' ' + Rd1Badge.Text + ' ' + RdDsWait.Text +
        ' ' + RdDsHold.Text);
end;

procedure TDoor2Form.ShortImageRexClick(Sender: TObject);
begin
  MainAppForm.SendCommand('setsupervisedinput', IntToStr(Rex_Number) + ' 2');
end;

procedure TDoor2Form.StrikeDefaultStateComboChange(Sender: TObject);
begin

end;

procedure TDoor2Form.Timer1Timer(Sender: TObject);
var
  retStatus: boolean;
begin
  Initialize(@ioValues, SizeOf(ioValues));
  retStatus := MainAppForm.getIoValues(ioValues, FormDest);
  if not retStatus then exit;
  if ioValues.leds[ReaderGreenLedIndex] = 1 then
  begin
    ReaderBlueLed.State := lson;
  end
  else
  begin
    ReaderBlueLed.State := lsoff;
  end;

  if ioValues.leds[ReaderRedLedIndex] = 1 then
  begin
    ReaderRedLed.State := lson;
  end
  else
  begin
    ReaderRedLed.State := lsoff;
  end;

  case ioValues.inputs[DS_Index] of
    1: begin
      ClosedDoor.Visible := True;
      OpenDoor.Visible := False;
      OpenDoor.Transparent := False;
      ShortImageDoor.Transparent := False;
    end;
    0: begin
      ClosedDoor.Visible := False;
      OpenDoor.Visible := True;
      OpenDoor.Transparent := False;
      ShortImageDoor.Transparent := False;
    end;
    2: begin
      ClosedDoor.Visible := True;
      OpenDoor.Visible := True;
      if OpenDoor.Transparent then
      begin
        OpenDoor.Transparent := False;
        ShortImageDoor.Transparent := False;
        CutImageDoor.Transparent := False;
      end
      else
      begin
        OpenDoor.Transparent := True;
        ShortImageDoor.Transparent := True;
        CutImageDoor.Transparent := False;
      end;
    end;
    3: begin
      ClosedDoor.Visible := True;
      OpenDoor.Visible := True;
      if OpenDoor.Transparent then
      begin
        OpenDoor.Transparent := False;
        CutImageDoor.Transparent := False;
      end
      else
      begin
        OpenDoor.Transparent := True;
        CutImageDoor.Transparent := True;
      end;
    end;
  end;

  case ioValues.inputs[Rex_Index] of
    1: begin
      RexLed.state := lsOn;
      ShortImageRex.Transparent := False;
      CutImageRex.Transparent := False;
    end;
    0: begin
      RexLed.state := lsOff;
      ShortImageRex.Transparent := False;
      CutImageRex.Transparent := False;
    end;
    2: begin
      if RexLed.state = lsOff then
      begin
        RexLed.state := lsOn;
        ShortImageRex.Transparent := False;
      end
      else
      begin
        RexLed.state := lsOff;
        ShortImageRex.Transparent := True;
      end;

    end;
    3: begin
      if RexLed.state = lsOff then
      begin
        RexLed.state := lsOn;
        CutImageRex.Transparent := False;
      end
      else
      begin
        RexLed.state := lsOff;
        CutImageRex.Transparent := True;
      end;
    end;

  end;

  if ioValues.outputs[Strike_Index] = 1 then
  begin
    StopLightSensor1.state := slRED;
  end
  else
  begin
    StopLightSensor1.state := slGreen;
  end;
  exit;

end;


procedure TDoor2Form.RexSensorDblClick(Sender: TObject);
begin
  if ioValues.inputs[Rex_Index] < 2 then
  begin
    if RexLed.state = lsOff then
    begin
      MainAppForm.SendCommand('setsupervisedinput', IntToStr(Rex_Number) + ' 1');
      RexLed.state := lsOn;
    end
    else
    begin
      MainAppForm.SendCommand('setsupervisedinput', IntToStr(Rex_Number) + ' 0');
      RexLed.state := lsOff;
    end;
  end
  else
  begin
    MainAppForm.SendCommand('setsupervisedinput', IntToStr(Rex_Number) + ' 0');
    RexLed.state := lsOff;
  end;
end;


procedure TDoor2Form.ClosedDoorClick(Sender: TObject);
begin
  MainAppForm.SendCommand('setsupervisedinput', IntToStr(DS_Number) + ' 0');
end;

procedure TDoor2Form.CutImageRexClick(Sender: TObject);
begin
  MainAppForm.SendCommand('setsupervisedinput', IntToStr(Rex_Number) + ' 3');
end;

procedure TDoor2Form.FormCreate(Sender: TObject);
begin
  Caption := 'Door ' + IntToStr(DoorNumber);
end;

procedure TDoor2Form.updateData(var jData: TJSONData; cmd: string);
var
  readerId: integer;
begin
  readerId := jdata.FindPath('reader').AsInteger;
  if readerId <> DoorNumber then exit;
  case cmd of
    'getreaderconfig': begin
      ReaderTypeCombo.Text := jdata.FindPath('readerType').AsString;
      DoorSensorDefaultStateCombo.Text := jdata.FindPath('diDefaultState').AsString;
      DoorSensorModeCombo.Text := jdata.FindPath('diMode').AsString;
      RexSensorDefaultStateCombo.Text := jdata.FindPath('rexDefaultState').AsString;
      RexSensorModeCombo.Text := jdata.FindPath('rexMode').AsString;
      LinkDStoStrikeCheckbox.Checked := jdata.FindPath('linkStrike').AsBoolean;
      KeyPadModeCombo.Text := jdata.FindPath('keypadMode').AsString;
      StrikeDefaultStateCombo.Text := jdata.FindPath('strikeDefaultState').AsString;
      RdDsWait.Text := jdata.FindPath('RdDsWait').AsString;
      RdDsHold.Text := jdata.FindPath('RdDsHold').AsString;
    end;
    'swipe26', 'keypadpress':
    begin
      SwipeDataBinaryData.Text := jdata.FindPath('data').AsString;
    end;
  end;
end;

end.
