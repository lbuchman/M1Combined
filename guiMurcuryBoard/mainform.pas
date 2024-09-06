unit mainForm;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, ActnList, Menus, Buttons, switches, AdvLed, fpJSON, jsonparser,
  DateUtils, UDPServer,  blinkLed, iovalues, sockets, ConfigurationJson;

type

  { TMainAppForm }
  TMainAppForm = class(TForm)
    BoardIpLabel: TLabel;
    BoardIpCombo: TComboBox;
    RdDebugHwOptionCheckBox: TCheckBox;
    hostName: TEdit;
    MainFormUpdateTimerEvent: TTimer;
    MainFormUpdateTimerTimer: TTimer;
    UpdateTimer: TTimer;
    procedure BoardIpComboChange(Sender: TObject);
    procedure BoardIpComboExit(Sender: TObject);
    procedure BoardIpComboKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure ContrPowerSwitchChange(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ClearIpsMenuItemClick(Sender: TObject);
    procedure SwitchChange(Sender: TObject; switchTag: integer);
    procedure PowerfaultSwitchChange(Sender: TObject);
    procedure TamperSwitchChange(Sender: TObject);
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
    deviceDescr : string;
    devices : array[0..255] of TDevice;
    datavalidArray: array [1..6] of boolean; //6 forms to update
    function IsIpValid(address : String) : boolean;
    function getIpAddressFromControllerData(data : string): string;
  public
    procedure reportCRCError;
    procedure UpdateData;
    procedure SendCommand(cmd: string; arg: string);
    function getIoValues(var retValue: TIOValues; dest: dataReceiver): boolean;
    procedure setDeviceDesc(desc : string);
  end;

{main: }

var
  MainAppForm: TMainAppForm;

implementation

{$R *.lfm}

{ TMainAppForm }

procedure TMainAppForm.reportCRCError;
begin
  // writeLog('incomming data CRC error, struct miss-match?');
end;

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

procedure TMainAppForm.setDeviceDesc(desc : string);
begin
  deviceDescr := desc;
end;


procedure TMainAppForm.SendCommand(cmd: string; arg: string);
begin
  targetBoardConnectionThread.SendCommand(cmd, arg);
end;

function TMainAppForm.getIpAddressFromControllerData(data : string) : string;
  var
  controllerData : TStringArray;
  tmpStr : String;
begin
   if data.Length = 0 then exit('');
   tmpStr := BoardIpCombo.Caption;
   controllerData := tmpStr.Split(' ');
   result := controllerData[0];
end;

procedure TMainAppForm.UpdateData;
var
  reply: string;
  status: boolean;
  jData: TJSONData;
  cmd: string;
  errorStr: string;
  ipAddress : string;
begin
  try
    reply := targetBoardConnectionThread.replyJson;
    if reply = '' then
    begin
      exit;
    end;

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

  except
    on E: Exception do ;
  end;
end;

procedure TMainAppForm.FormCreate(Sender: TObject);
var
  count : integer;
  devicesCount : Integer;
begin
  doUpdate := False;
  boardPort := '4111';
  targetBoardConnectionThread := TUDPServerThread.Create(False);
  commStatusLedObj := TBlinkLed.Create(self, BoardIpCombo.Top,
    BoardIpCombo.Left + BoardIpCombo.Width + 40, lkGreenLight);
  BoardIpCombo.Font.Size := 9;
  Font.Size := 9;
  ConfigurationGet;
  for count := 0 to Pred(Length(devices)) do begin
    devices[count].ip := '';
    devices[count].desc := '';
  end;
  devicesCount := getDevicesJson(Length(devices), devices);
  for count := 0 to Pred(devicesCount) do begin
     BoardIpCombo.Items.Add(devices[count].ip + ' [' + devices[count].desc + ']');
  end;
  count := BoardIpCombo.Items.Count;
  if count > 0 then begin
    BoardIpCombo.Text := BoardIpCombo.Items[0];
    targetBoardConnectionThread.CloseConnection();
    targetBoardConnectionThread.OpenConnection(getIpAddressFromControllerData(BoardIpCombo.Text), boardPort);
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

function TMainAppForm.IsIpValid(address : String) : boolean;
var
ip_addr: in_addr;
begin

  ip_addr := StrToHostAddr(PChar(address));
  if ip_addr.s_addr <> 0 then  result := true
  else result := false;
end;

procedure TMainAppForm.BoardIpComboChange(Sender: TObject);
begin
  if IsIpValid(getIpAddressFromControllerData(BoardIpCombo.Text)) then
  begin
    targetBoardConnectionThread.CloseConnection();
    targetBoardConnectionThread.OpenConnection(getIpAddressFromControllerData(BoardIpCombo.Text), boardPort);
  end;
end;

procedure TMainAppForm.BoardIpComboExit(Sender: TObject);
begin
  //  targetBoardConnectionThread.CloseConnection();
  //  targetBoardConnectionThread.OpenConnection(BoardIpCombo.Text, boardPort);
end;

procedure TMainAppForm.ContrPowerSwitchChange(Sender: TObject);
begin
  SwitchChange(Sender, 1);
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

procedure TMainAppForm.FormShow(Sender: TObject);
var
  _top, _left, _width, _height: integer;
begin
  Font.Size := 7;
  _top := 0;
  _left := 0;
  _width := 0;
  _height := 0;
  getFormGeometryJson('mainForm', _top, _left, _width, _height);
  top := _top;
  left := _left;
  Width := _width;
  Height := _height;
  initilized := True;
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

procedure TMainAppForm.PowerfaultSwitchChange(Sender: TObject);
begin
  SwitchChange(Sender, 2);
end;

procedure TMainAppForm.SwitchChange(Sender: TObject; switchTag: integer);
var
  command: string;
  inverted: integer;
begin
  inverted := 0;
  command := '';
  case switchTag of
    0: begin
      command := 'cabtamp';
      inverted := 0;
    end;
    1: begin
      command := 'contrlpower';
      inverted := 1;
    end;

    2: begin
      command := 'powerfault';
      inverted := 0;
    end;
  end;
  if TOnOffSwitch(Sender).Checked then
  begin
    SendCommand(command, IntToStr(inverted));
  end
  else
    SendCommand(command, IntToStr(inverted xor 1));
  TOnOffSwitch(Sender).tag := 1;
  doUpdate := False;
  UpdateTimer.Enabled := False;
  UpdateTimer.Enabled := True;
end;


procedure TMainAppForm.TamperSwitchChange(Sender: TObject);
begin
  SwitchChange(Sender, 0);
end;

procedure TMainAppForm.UpdateTimerEvent(Sender: TObject);
begin
  doUpdate := True;
  UpdateTimer.Enabled := False;
end;

end.
