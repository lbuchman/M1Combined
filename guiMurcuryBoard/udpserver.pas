unit UDPServer;

{$MODE Delphi}

interface

uses blcksock, Classes, SysUtils, simplelog;

const
  UDPServerPort = '3001';      // Send from client side
  UDPClientPort = '3000';      // Recive from client side

type

  TUDPServerThread = class(TThread)
  private
    reply: string;
    dataProtectCS: TRTLCriticalSection;
    RestRequestsList: TStringList;
    FUDPSocket: TUDPBlockSocket;
    procedure DeleteFromQueue(cmd: string);
  public
    property replyJson: string read reply;
    procedure Execute; override;
    constructor Create(CreateSuspended: boolean);
    procedure QueueCommand(newCommand: string);
    procedure SendCommand(cmd: string; arg: string);
    procedure OpenConnection(ipaddress: string; port: string);
    procedure CloseConnection();
  end;

implementation

uses mainForm;

procedure TUDPServerThread.QueueCommand(newCommand: string);
begin
  if (RestRequestsList.Count > 10) then exit;
  EnterCriticalSection(dataProtectCS);
  RestRequestsList.Add(newCommand);
  LeaveCriticalSection(dataProtectCS);
end;

{ TUDPServerThread }
procedure TUDPServerThread.CloseConnection();
begin
 // FUDPSocket.CloseSocket();
end;

procedure TUDPServerThread.OpenConnection(ipaddress: string; port: string);
begin
  FUDPSocket.Connect(ipaddress, port);
  Start;
end;

procedure TUDPServerThread.Execute;
var
  cmd: string;
  pullCmd: string;
begin
  pullCmd := '{ "cmd": "' + 'getalldata' + '","arg": "' + 'null' + '" }';
  // FUDPSocket := TUDPBlockSocket.Create;
  // FUDPSocket.Bind('0.0.0.0', UDPServerPort);
  { FLocalIP:= FUDPSocket.ResolveName(FUDPSocket.LocalName);     }

  while not Terminated do
  begin
    {FUDPSocket.RecvBufferEx( @FUDPRequest, sizeof(TUDPRequest), 100 );}
    cmd := '';
    EnterCriticalSection(dataProtectCS);
    if RestRequestsList.Count <> 0 then cmd := RestRequestsList[0];
    LeaveCriticalSection(dataProtectCS);

    if cmd = '' then
    begin
      cmd := pullCmd;
    end;
    writeLog(cmd);
    FUDPSocket.SendString(cmd);
    reply := FUDPSocket.RecvPacket(300);
    DeleteFromQueue(cmd);
    if reply.Length > 10 then //LastErrorDesc
    begin
      Synchronize(MainAppForm.UpdateData);
    end;
    Sleep(100);
  end;
  RestRequestsList.Free;
  FUDPSocket.Free;
end;

constructor TUDPServerThread.Create(CreateSuspended: boolean);
begin
  InitCriticalSection(dataProtectCS);
  RestRequestsList := TStringList.Create;
  // piHost := _piHost;
  inherited Create(CreateSuspended);
  // FreeOnTerminate := True;
  FUDPSocket := TUDPBlockSocket.Create;
  FUDPSocket.Bind('0.0.0.0', UDPServerPort);
end;

procedure TUDPServerThread.DeleteFromQueue(cmd: string);
var
  index: integer;
begin
  EnterCriticalSection(dataProtectCS);
  index := RestRequestsList.IndexOf(cmd);
  if index <> -1 then RestRequestsList.Delete(index);
  LeaveCriticalSection(dataProtectCS);
end;

procedure TUDPServerThread.SendCommand(cmd: string; arg: string);
var
  json: string;
begin
  json := '{ "cmd": "' + cmd + '", "arg": "' + arg + '" }';
  QueueCommand(json);
end;

end.
