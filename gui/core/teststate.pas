unit teststate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTestResultStatus = (tsNotStarted, tsRunning, tsSuccess, tsFailed, tsAborted);
  TTestingMode = (tmNone, tmCommission, tmRetest);

  { TTestState }
  TTestState = class
  private
    FCurrentTest: integer;
    FTestMode: TTestingMode;
    FStatus: TTestResultStatus;
    FErrorCode: integer;
    FErrorMessage: string;
    FSerialNumber: string;
    FBusyFlag: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;
    procedure SetRunning;
    procedure SetSuccess;
    procedure SetFailed(Code: integer; Msg: string);
    procedure SetAborted;

    property Mode: TTestingMode read FTestMode write FTestMode;
    property Status: TTestResultStatus read FStatus write FStatus;
    property ErrorCode: integer read FErrorCode write FErrorCode;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    property SerialNumber: string read FSerialNumber write FSerialNumber;
    property BusyFlag: boolean read FBusyFlag write FBusyFlag;
    property CurrentTest: integer read FCurrentTest write FCurrentTest;

    function IsRunning: boolean;
    function IsFailed: boolean;
    function IsSuccess: boolean;
  end;

implementation

constructor TTestState.Create;
begin
  inherited Create;
  Reset;
end;

destructor TTestState.Destroy;
begin
  inherited Destroy;
end;

procedure TTestState.Reset;
begin
  FStatus := tsNotStarted;
  FErrorCode := 0;
  FErrorMessage := '';
  FBusyFlag := False;
  FCurrentTest := 0;
  FMode := tmNone;
end;

procedure TTestState.SetRunning;
begin
  FStatus := tsRunning;
  FBusyFlag := True;
end;

procedure TTestState.SetSuccess;
begin
  FStatus := tsSuccess;
  FBusyFlag := False;
  FErrorCode := 0;
end;

procedure TTestState.SetFailed(Code: integer; Msg: string);
begin
  FStatus := tsFailed;
  FBusyFlag := False;
  FErrorCode := Code;
  FErrorMessage := Msg;
end;

procedure TTestState.SetAborted;
begin
  FStatus := tsAborted;
  FBusyFlag := False;
end;

function TTestState.IsRunning: boolean;
begin
  Result := FStatus = tsRunning;
end;

function TTestState.IsFailed: boolean;
begin
  Result := FStatus = tsFailed;
end;

function TTestState.IsSuccess: boolean;
begin
  Result := FStatus = tsSuccess;
end;

end.
