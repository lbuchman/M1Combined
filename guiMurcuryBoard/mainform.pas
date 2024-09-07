unit mainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, ActnList, Menus, Buttons, EditBtn, switches, AdvLed, indSliders,
  MKnob, A3nalogGauge, Sensors, IndLed, indLCDDisplay, fpJSON, jsonparser,
  DateUtils, UDPServer, blinkLed, iovalues, sockets, ConfigurationJson,
  ECProgressBar, ECSlider, ECEditBtns, ECTriangle, ECSpinCtrls, ECLink,
  ECScheme;

const
  INPUT = 0;

const
  OUTPUT = 1;

const
  INPUT_PULLUP = 2;

const
  ANALOG = 3;

type
  IoLine = record
    itemR: ^TComponent;
    itemW: ^TComponent;
    itemM: ^TComponent;
    cmdRead: string;
    cmdWrite: string;
    pinType: integer;
    pinN: integer;
  end;

  { TMainAppForm }
  TMainAppForm = class(TForm)
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label9: TLabel;
    sp1v1: TLCDDisplay;
    sp2v2: TLCDDisplay;
    sp3v3: TLCDDisplay;
    sp4v4: TLCDDisplay;
    Rd1Bz: TAdvLed;
    Ry1: TAdvLed;
    Ry2: TAdvLed;
    Ry3: TAdvLed;
    Ry4: TAdvLed;
    Rd2Bz: TAdvLed;
    Rd2Gled: TAdvLed;
    Rd1RD0: TAdvLed;
    BoardIpLabel: TLabel;
    BoardIpCombo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Rd1RD1: TAdvLed;
    Rd2Rled: TAdvLed;
    Rd2RD1: TAdvLed;
    Rd2RD0: TAdvLed;
    Rd1Rled: TAdvLed;
    Rd1Gled: TAdvLed;
    sp1v: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    RdDebugHwOptionCheckBox: TCheckBox;
    hostName: TEdit;
    MainFormUpdateTimerEvent: TTimer;
    MainFormUpdateTimerTimer: TTimer;
    SP1: TMultiSlider;
    sp2v: TLabel;
    sp3v: TLabel;
    sp4v: TLabel;
    sp1vLabel: TLabel;
    sp1vLabel1: TLabel;
    sp1vLabel2: TLabel;
    sp1vLabel3: TLabel;
    SP2: TMultiSlider;
    SP3: TMultiSlider;
    SP4: TMultiSlider;
    StatusBar1: TStatusBar;
    UpdateTimer: TTimer;
    procedure BoardIpComboChange(Sender: TObject);
    procedure BoardIpComboExit(Sender: TObject);
    procedure BoardIpComboKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure ExitMenuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClearIpsMenuItemClick(Sender: TObject);
    procedure Rd1RD0Click(Sender: TObject);
    procedure Rd1WD0Click(Sender: TObject);
    procedure SP1Click(Sender: TObject);
    procedure SP1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure SP1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sp1vLabelDblClick(Sender: TObject);
    procedure SP2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure SP3MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure SP4MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure UpdateTimerEvent(Sender: TObject);
    procedure MainFormUpdateTimer(Sender: TObject);

  private
    targetBoardConnectionThread: TUDPServerThread;
    errorCount: integer;
    initilized: boolean;
    commStatusLedObj: TBlinkLed;
    boardPort: string;
    ioValues: TIOValues;
    doUpdate: boolean;
    deviceDescr: string;
    ioLines: array[0..17] of IoLine;
    devices: array[0..255] of TDevice;
    datavalidArray: array [1..6] of boolean; //6 forms to update
    function IsIpValid(address: string): boolean;
    function getIpAddressFromControllerData(Data: string): string;
    function BoolToInt(value : Boolean) : Integer;
  public
    procedure UpdateData;
    procedure SendCommand(cmd: string; arg: string);
    function getIoValues(var retValue: TIOValues; dest: dataReceiver): boolean;
    function FindIoLine(pin: integer): integer;
    procedure setDeviceDesc(desc: string);
  end;

  {main: }

var
  MainAppForm: TMainAppForm;

implementation

{$R *.lfm}

{ TMainAppForm }


function TMainAppForm.getIoValues(var retValue: TIOValues; dest: dataReceiver): boolean;
begin
  if datavalidArray[shortint(dest)] then
  begin
    datavalidArray[shortint(dest)] := False;
    retValue := ioValues;
    Result := True;
    exit;
  end;
  Result := False;
  retValue := ioValues;
end;

procedure TMainAppForm.setDeviceDesc(desc: string);
begin
  deviceDescr := desc;
end;


procedure TMainAppForm.SendCommand(cmd: string; arg: string);
begin
  targetBoardConnectionThread.SendCommand(cmd, arg);
end;

function TMainAppForm.getIpAddressFromControllerData(Data: string): string;
var
  controllerData: TStringArray;
  tmpStr: string;
begin
  if Data.Length = 0 then exit('');
  tmpStr := BoardIpCombo.Caption;
  controllerData := tmpStr.Split(' ');
  Result := controllerData[0];
end;

procedure TMainAppForm.UpdateData;
var
  reply: string;
  status: boolean;
  jData: TJSONData;
  tmpObj: TJSONObject;
  values: TJSONArray;
  pin: integer;
  index : Integer;
  Value: integer;
  cmd: string;
  Count: integer;
  errorStr: string;
  sInVoltage : Single;
  sInVoltageSTr : String;
  ledState : TLedState;
begin
  try
    reply := targetBoardConnectionThread.replyJson;
    if reply = '' then
    begin
      exit;
    end;
    commStatusLedObj.BlinkNow(200);
    errorCount := 0;
    jData := GetJSON(reply);
    cmd := jdata.FindPath('cmd').AsString;
    status := jdata.FindPath('status').AsBoolean;
    if not status then
    begin
      errorStr := jdata.FindPath('error').AsString;
      MessageDlg('cmd: ' + cmd + ' ' + errorStr, mtError, [mbClose], 0);
      exit;
    end;
    values := TJSONArray(jdata.FindPath('values'));
    if values <> nil then
    begin

      for Count := 0 to values.Count - 1 do
      begin
        tmpObj := TJSONObject(values.Items[Count]);
        pin := tmpObj.FindPath('pin').AsInteger;
        Value := tmpObj.FindPath('value').AsInteger;
        index := FindIoLine(pin);
        if index > -1 then begin
          if ioLines[index].pinType <> Analog then begin
            if Value = 1 then  ledState := lsOff
            else ledState := lsOn;
            TAdvLed(ioLines[index].itemR^).State := ledState;
          end
          else begin
              sInVoltage :=  Value * 4.93 / 255;
              sInVoltageStr := Format('%3.2f', [sInVoltage]) + 'V';
              TLCDDisplay(ioLines[index].itemW^).Lines.Clear;
              TLCDDisplay(ioLines[index].itemW^).Lines.Add(sInVoltageStr);
          end;
        end;
      end;

    end

  except
    on E: Exception do ;
  end;
end;


function TMainAppForm.FindIoLine(pin: integer): integer;
var
  count1: integer;
  arrayLength : Integer;
begin
  arrayLength := Pred(Length(ioLines));

  for count1 := 0 to arrayLength do
  begin
    if ioLines[count1].pinN = pin then
    begin
      Result := count1;
      exit(count1);
    end;
   end;
  Result := -1;
end;


procedure TMainAppForm.FormCreate(Sender: TObject);
var
  Count, Count1: integer;
  devicesCount: integer;
  ioMissmatch: boolean;
begin

  ioLines[0].cmdRead := 'rd1rd0';
  ioLines[0].pinType := OUTPUT;
  ioLines[0].cmdWrite := 'rd1d0';
  ioLines[0].pinN := 10;
  ioLines[0].itemW := nil;
  ioLines[0].itemR := @Rd1RD0;

  ioLines[1].cmdRead := 'rd1rd1';
  ioLines[1].pinType := OUTPUT;
  ioLines[1].cmdWrite := 'rd1d1';
  ioLines[1].pinN := 9;
  ioLines[1].itemW := nil;
  ioLines[1].itemR := @Rd1RD1;


  ioLines[2].cmdRead := 'Rd1Rled';
  ioLines[2].pinType := OUTPUT;
  ioLines[2].cmdWrite := '';
  ioLines[2].pinN := 5;
  ioLines[2].itemR := @Rd1Rled;
  ioLines[2].itemW := nil;

  ioLines[3].cmdRead := 'Rd1Gled';
  ioLines[3].pinType := INPUT;
  ioLines[3].cmdWrite := '';
  ioLines[3].pinN := 11;
  ioLines[3].itemR := @Rd1Gled;
  ioLines[3].itemW := nil;


  ioLines[4].cmdRead := 'rd1bz';
  ioLines[4].pinType := INPUT;
  ioLines[4].cmdWrite := '';
  ioLines[4].pinN := 40;
  ioLines[4].itemR := @Rd1Bz;
  ioLines[4].itemW := nil;

  ioLines[5].cmdRead := 'rd2rd0';
  ioLines[5].pinType := INPUT;
  ioLines[5].cmdWrite := 'rd2d0';
  ioLines[5].pinN := 26;
  ioLines[5].itemW := nil;
  ioLines[5].itemR := @Rd2RD0;

  ioLines[6].cmdRead := 'rd2rd1';
  ioLines[6].pinType := OUTPUT;
  ioLines[6].cmdWrite := 'rd2d1';
  ioLines[6].pinN := 32;
  ioLines[6].itemW := nil;
  ioLines[6].itemR := @Rd2RD1;

  ioLines[7].cmdRead := 'Rd2Rled';
  ioLines[7].pinType := INPUT;
  ioLines[7].cmdWrite := '';
  ioLines[7].pinN := 12;
  ioLines[7].itemR := @Rd2Rled;
  ioLines[7].itemW := nil;

  ioLines[8].cmdRead := 'Rd2Gled';
  ioLines[8].pinType := INPUT;
  ioLines[8].cmdWrite := '';
  ioLines[8].pinN := 30;
  ioLines[8].itemR := @Rd2Gled;
  ioLines[8].itemW := nil;

  ioLines[9].cmdRead := 'rd2bz';
  ioLines[9].pinType := INPUT;
  ioLines[9].cmdWrite := '';
  ioLines[9].pinN := 39;
  ioLines[9].itemR := @Rd2Bz;
  ioLines[9].itemW := nil;


  ioLines[10].cmdRead := 'Ry1';
  ioLines[10].pinType := INPUT;
  ioLines[10].cmdWrite := '';
  ioLines[10].pinN := 23;
  ioLines[10].itemR := @Ry1;
  ioLines[10].itemW := nil;


  ioLines[11].cmdRead := 'Ry2';
  ioLines[11].pinType := INPUT;
  ioLines[11].cmdWrite := '';
  ioLines[11].pinN := 3;
  ioLines[11].itemR := @Ry2;
  ioLines[11].itemW := nil;

  ioLines[12].cmdRead := 'Ry3';
  ioLines[12].pinType := INPUT;
  ioLines[12].cmdWrite := '';
  ioLines[12].pinN := 33;
  ioLines[12].itemR := @Ry3;
  ioLines[12].itemW := nil;

  ioLines[13].cmdRead := 'Ry4';
  ioLines[13].pinType := INPUT;
  ioLines[13].cmdWrite := '';
  ioLines[13].pinN := 37;
  ioLines[13].itemR := @Ry4;
  ioLines[13].itemW := nil;

 
  ioLines[14].cmdRead := '';
  ioLines[14].pinType := ANALOG;
  ioLines[14].cmdWrite := 'si1s';
  ioLines[14].pinN := 14;
  ioLines[14].itemR := @SP1;
  ioLines[14].itemW := @Sp1v1;
  ioLines[14].itemM := @sp1v;

  ioLines[15].cmdRead := '';
  ioLines[15].pinType := ANALOG;
  ioLines[15].cmdWrite := 'si2s';
  ioLines[15].pinN := 18;
  ioLines[15].itemR := @SP2;
  ioLines[15].itemW := @Sp2v2;
  ioLines[15].itemM := @sp2v;


  ioLines[16].cmdRead := '';
  ioLines[16].pinType := ANALOG;
  ioLines[16].cmdWrite := 'si3s';
  ioLines[16].pinN := 19;
  ioLines[16].itemR := @SP3;
  ioLines[16].itemW := @Sp3v3;
  ioLines[16].itemM := @sp3v;


  ioLines[17].cmdRead := '';
  ioLines[17].pinType := ANALOG;
  ioLines[17].cmdWrite := 'si4s';
  ioLines[17].pinN := 15;
  ioLines[17].itemR := @SP4;
  ioLines[17].itemW := @Sp4v4;
  ioLines[17].itemM := @sp4v;

  ioMissmatch := False;
  for count1 := 0 to Pred(Length(ioLines)) do
  begin
    if ioLines[count1].itemR <> nil then
      if ioLines[count1].pinN <> ioLines[count1].itemR^.Tag then
      begin
        ioMissmatch := True;
      end;

    if ioLines[count1].itemW <> nil then
      if ioLines[count1].pinN <> ioLines[count1].itemW^.Tag then
      begin
        ioMissmatch := True;
      end;
  end;

  if ioMissmatch then MessageDlg('IO definition does not match, fix and recompile',
      mtError, [mbClose], 0);

  doUpdate := False;
  boardPort := '4111';
  targetBoardConnectionThread := TUDPServerThread.Create(True);
  commStatusLedObj := TBlinkLed.Create(self, BoardIpCombo.Top,
    BoardIpCombo.Left + BoardIpCombo.Width + 40, lkGreenLight);
  BoardIpCombo.Font.Size := 9;
  Font.Size := 9;
  ConfigurationGet;
  for Count := 0 to Pred(Length(devices)) do
  begin
    devices[Count].ip := '';
    devices[Count].desc := '';
  end;
  devicesCount := getDevicesJson(Length(devices), devices);
  for Count := 0 to Pred(devicesCount) do
  begin
    BoardIpCombo.Items.Add(devices[Count].ip + ' [' + devices[Count].desc + ']');
  end;
  Count := BoardIpCombo.Items.Count;
  if Count > 0 then
  begin
    BoardIpCombo.Text := BoardIpCombo.Items[0];
    targetBoardConnectionThread.CloseConnection();
    targetBoardConnectionThread.OpenConnection(
      getIpAddressFromControllerData(BoardIpCombo.Text), boardPort);
  end;
end;

procedure TMainAppForm.ExitMenuClick(Sender: TObject);
begin
  Application.Terminate();
end;

procedure TMainAppForm.BoardIpComboKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
var
  newIp: string;
begin
  Shift := Shift;
  if Key = 13 then
  begin
    newIp := getIpAddressFromControllerData(BoardIpCombo.Text);
    newIp := TrimRight(newIp);
    targetBoardConnectionThread.CloseConnection();
    targetBoardConnectionThread.OpenConnection(newIp, boardPort);
    targetBoardConnectionThread.OpenConnection(newIp, boardPort);
  end;
end;

function TMainAppForm.IsIpValid(address: string): boolean;
var
  ip_addr: in_addr;
begin

  ip_addr := StrToHostAddr(PChar(address));
  if ip_addr.s_addr <> 0 then  Result := True
  else
    Result := False;
end;

procedure TMainAppForm.BoardIpComboChange(Sender: TObject);
begin
  if IsIpValid(getIpAddressFromControllerData(BoardIpCombo.Text)) then
  begin
    targetBoardConnectionThread.CloseConnection();
    targetBoardConnectionThread.OpenConnection(
      getIpAddressFromControllerData(BoardIpCombo.Text), boardPort);
  end;
end;

procedure TMainAppForm.BoardIpComboExit(Sender: TObject);
begin
  //  targetBoardConnectionThread.CloseConnection();
  //  targetBoardConnectionThread.OpenConnection(BoardIpCombo.Text, boardPort);
end;

procedure TMainAppForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  targetBoardConnectionThread.Terminate;
  targetBoardConnectionThread.WaitFor;
  targetBoardConnectionThread.Free;
  CloseAction := CloseAction;
  // setFormGeometry('MAINFORM-INFO', top, left, Width, Height);
  setFormGeometryJson('mainForm', top, left, Width, Height);
  writeConfiguration();

end;

procedure TMainAppForm.FormDestroy(Sender: TObject);
begin
  commStatusLedObj.Free;
  inherited;
end;


procedure TMainAppForm.MainFormUpdateTimer(Sender: TObject);
var
  ioValuesLocal: TIOValues;
begin
  Initialize(@ioValuesLocal, SizeOf(ioValues));
  if not getIoValues(ioValuesLocal, mainFormDest) then exit;
  if not doUpdate then exit;
end;

procedure TMainAppForm.ClearIpsMenuItemClick(Sender: TObject);
begin
  BoardIpCombo.Items.Clear;
end;

procedure TMainAppForm.Rd1RD0Click(Sender: TObject);
var
 ledState : TLedState;
 pin, index : Integer;
 state : String;
begin
    pin := TAdvLed(Sender).tag;
    index := FindIoLine(pin);
    if index < 0 then exit;
    ledState := TAdvLed(ioLines[index].itemR^).State;
    if ledState = lsOn then state := '1'
    else state := '0';
    SendCommand(ioLines[index].cmdWrite, state);
end;

procedure TMainAppForm.Rd1WD0Click(Sender: TObject);
var
  pin, index : Integer;
  isChecked :Boolean;
begin
    pin := TCheckBox(Sender).tag;
    index := FindIoLine(pin);
    if index < 0 then exit;
    isChecked := TCheckBox(ioLines[index].itemW^).checked;
    SendCommand(ioLines[index].cmdWrite, IntToStr(BoolToInt(isChecked)));
end;

procedure TMainAppForm.SP1Click(Sender: TObject);
var
   pin, index, sliderPos : Integer;
   value : String;
begin
    pin := TComponent(Sender).tag;
    index := FindIoLine(pin);
    if index < 0 then exit;
    value := TLCDDisplay(ioLines[index].itemW^).Lines[0];
    //TMultiSlider(ioLines[index].itemR^).position := 4.93 * 255 / ;

end;

function TMainAppForm.BoolToInt(value : Boolean) : Integer;
begin
  if not value then result := 1
  else result := 0;

end;

procedure TMainAppForm.SP1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  control: TMultiSlider;
  Value: single;
begin
  control := TMultiSlider(Sender);
  Value := (control.Position * 4.93) / 255;
  sp1v.Caption := Format('%3.2f', [Value]) + 'V';
end;

procedure TMainAppForm.SP1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var pin, index, sliderPos : Integer;
begin
    pin := TComponent(Sender).tag;
    index := FindIoLine(pin);
    if index < 0 then exit;
    sliderPos := TMultiSlider(ioLines[index].itemR^).position;
    SendCommand(ioLines[index].cmdWrite, IntToStr(TMultiSlider(ioLines[index].itemR^).position));
end;

procedure TMainAppForm.sp1vLabelDblClick(Sender: TObject);
var
   pin, index, sliderPos : Integer;
   value : String;
begin
    pin := TComponent(Sender).tag;
    index := FindIoLine(pin);
    if index < 0 then exit;
    value := copy(TLCDDisplay(ioLines[index].itemW^).Lines[0], 0, 4);
    sliderPos :=  Round(STrToFloat(value) * 255 /4.93);
    TMultiSlider(ioLines[index].itemR^).position:= sliderPos;
    TLabel(ioLines[index].itemM^).Caption:= value + 'V';

end;

procedure TMainAppForm.SP2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  control: TMultiSlider;
  Value: single;
begin
  control := TMultiSlider(Sender);
  Value := (control.Position * 4.93) / 255;
  sp2v.Caption := Format('%3.2f', [Value]) + 'V';
end;

procedure TMainAppForm.SP3MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  control: TMultiSlider;
  Value: single;
begin
  control := TMultiSlider(Sender);
  Value := (control.Position * 4.93) / 255;
  sp3v.Caption := Format('%3.2f', [Value]) + 'V';
end;

procedure TMainAppForm.SP4MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  control: TMultiSlider;
  Value: single;
begin
  control := TMultiSlider(Sender);
  Value := (control.Position * 4.93) / 255;
  sp4v.Caption := Format('%3.2f', [Value]) + 'V';
end;

procedure TMainAppForm.UpdateTimerEvent(Sender: TObject);
begin
  doUpdate := True;
  UpdateTimer.Enabled := False;
end;

end.
