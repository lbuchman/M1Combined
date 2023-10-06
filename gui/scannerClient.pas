unit scannerClient;

{$MODE Delphi}

interface

uses blcksock, Classes, SysUtils;

type

  TUDPScannerServerThread = class(TThread)
  private
    SerialNumber: string;
    udpScannerPort: string;
    FUDPSocket: TUDPBlockSocket;
  public
    procedure Execute; override;
    constructor Create(CreateSuspended: boolean; scannerPort: string);
    function GetSerialNumber: string;
  end;

implementation

uses main;

procedure TUDPScannerServerThread.Execute;
begin
  FUDPSocket := TUDPBlockSocket.Create;
  FUDPSocket.CreateSocket;
  FUDPSocket.Bind('0.0.0.0', udpScannerPort);

  while not Terminated do
  begin
    {FUDPSocket.RecvBufferEx( @FUDPRequest, sizeof(TUDPRequest), 100 );}
    SerialNumber := '';
    SerialNumber := FUDPSocket.RecvPacket(100);

    if SerialNumber.Length > 0 then
    begin
      Synchronize(MainForm.UpdateTargetSerial);
    end;
    Sleep(100);
  end;
  FUDPSocket.CloseSocket();

  FUDPSocket.Free;
end;

function TUDPScannerServerThread.GetSerialNumber: string;
begin
  Result := Trim(SerialNumber);
end;


constructor TUDPScannerServerThread.Create(CreateSuspended: boolean;
  scannerPort: string);
begin
  inherited Create(CreateSuspended);
  udpScannerPort := scannerPort;
end;


end.
