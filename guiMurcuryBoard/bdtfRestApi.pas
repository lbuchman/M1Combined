unit bdtfRestApi;

{$MODE Delphi}

interface

uses Classes, Forms, SysUtils, {$IFDEF UNIX}cthreads,{$ENDIF}
  fphttpapp, httpdefs, fphttpclient, Dialogs, RTTICtrls;

const
  UDPServerPort = '10001';      // Send from client side
  UDPClientPort = '10000';      // Recive from client side

type
  CommException = class(Exception);

type
  TBdtfThread = class(TThread)
  private
    reply: string;
    retries: integer;
    dataProtectCS: TRTLCriticalSection;
    TheURL: string;
    RestRequestsList: TStringList;
    function RestRequest(cmd: string): string;
    procedure DeleteFromQueue(cmd: string);
  public
    property replyJson: string read reply;
    procedure Execute; override;
    constructor Create(CreateSuspended: boolean; url: string);
    procedure QueueCommand(newCommand: string);
    function SendCommand(cmd, arg, timeout: string): string;
  end;

implementation

uses mainForm;

{ TUDPServerThread }

procedure TBdtfThread.QueueCommand(newCommand: string);
begin
  EnterCriticalSection(dataProtectCS);
  RestRequestsList.Add(newCommand);
  LeaveCriticalSection(dataProtectCS);
end;

procedure TBdtfThread.Execute;
const
  MaxRetries = 3;
var
  cmd: string;
begin
  while not Terminated do
  begin
    cmd := '';
    EnterCriticalSection(dataProtectCS);
    if RestRequestsList.Count <> 0 then cmd := RestRequestsList[0];
    LeaveCriticalSection(dataProtectCS);

    if cmd = '' then
    begin
      Sleep(100);
      continue;
    end;

    try
      reply := RestRequest(cmd);
      DeleteFromQueue(cmd);
      retries := 0;
      Synchronize(MainAppForm.UpdateData);
    except
      on E: CommException do
      begin
        if retries > MaxRetries then
        begin
          DeleteFromQueue(cmd);
          retries := 0;
        end
        else
          retries := retries + 1;
      end;
    end;
  end;

end;

function TBdtfThread.RestRequest(cmd: string): string;
var
  Client: TFPHttpClient;
  Response: TStringStream;
begin
  Client := TFPHttpClient.Create(nil);
  Client.AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
  Client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
  Client.AddHeader('Accept', 'application/json');
  Client.AllowRedirect := True;
  client.IOTimeout := 2000;
  client.ConnectTimeout := 5000;

  client.RequestBody := TRawByteStringStream.Create(cmd);
  Response := TStringStream.Create('');
  try
    try
      client.Post(TheURL, Response);
      if (Client.ResponseStatusCode = 200) then Result := Response.DataString
      else
        raise CommException.Create('http returned ' +
          IntToStr(Client.ResponseStatusCode));

    except
      on E: Exception do
      begin
        raise CommException.Create(E.Message);
      end;
    end;
  finally
    Client.RequestBody.Free;
    Client.Free;
    Response.Free;
  end;
end;

procedure TBdtfThread.DeleteFromQueue(cmd: string);
var
  index: integer;
begin
  EnterCriticalSection(dataProtectCS);
  index := RestRequestsList.IndexOf(cmd);
  if index <> -1 then RestRequestsList.Delete(index);
  LeaveCriticalSection(dataProtectCS);
end;

constructor TBdtfThread.Create(CreateSuspended: boolean; url: string);
begin
  InitCriticalSection(dataProtectCS);
  RestRequestsList := TStringList.Create;
  // will not worry for free, prog exit will fix it
  inherited Create(CreateSuspended);
  TheURL := url;
  FreeOnTerminate := True;
  retries := 0;
end;

function TBdtfThread.SendCommand(cmd, arg, timeout: string): string;
var
  json: string;
begin
  json := '{"cmd": "' + cmd + '","arg": "' + arg + '", "timeout":' + timeout + ' }';
  QueueCommand(json);
  Result := '';
end;

end.
