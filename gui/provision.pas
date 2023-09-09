unit provision;

{$MODE Delphi}

interface

uses blcksock, Classes, SysUtils;

const
  NActions = 6; // actual value is + 1


type

  TMyProc = procedure(obj: TObject) of object;

  TProvisionThread = class(TThread)
  private
    event: PRTLEvent;
    TermnateTest: boolean;
  public
    procedure Execute; override;
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
    procedure ExecuteThread;
    procedure UserInterruptTest;
    function GetTermnateTestStatus: boolean;
    procedure ResetTest();
  end;

implementation

uses main;

procedure TProvisionThread.ResetTest();
begin
  TermnateTest := False;
end;

function TProvisionThread.GetTermnateTestStatus: boolean;
begin
  Result := TermnateTest;
end;

procedure TProvisionThread.UserInterruptTest;
begin
  TermnateTest := True;

end;

procedure TProvisionThread.Execute;
begin
  TermnateTest := False;
  while not Terminated do
  begin
    TermnateTest := False;
    RTLEventWaitFor(event);
    if (TermnateTest) then
    begin
      Synchronize(MainForm.ResetLeds);
      TermnateTest := False;
      continue;
    end;
    Synchronize(MainForm.ICTTestSwitchClick_);
    Synchronize(MainForm.Add15ToProgressBar);
    if (TermnateTest) then
    begin
      Synchronize(MainForm.ResetLeds);
      TermnateTest := False;
      Synchronize(MainForm.DoLabelError);
      continue;
    end;
    Synchronize(MainForm.MacProgSwitchClick_);
    Synchronize(MainForm.Add10ToProgressBar);
    if (TermnateTest) then
    begin
      Synchronize(MainForm.ResetLeds);
      Synchronize(MainForm.DoLabelError);
      TermnateTest := False;
      continue;
    end;
    Synchronize(MainForm.FlashSwitchClick_);
    Synchronize(MainForm.Add30ToProgressBar);
    if (TermnateTest) then
    begin
      Synchronize(MainForm.ResetLeds);
      Synchronize(MainForm.DoLabelError);
      TermnateTest := False;
      continue;
    end;
    Synchronize(MainForm.FuncTestSwitchClick_);
    Synchronize(MainForm.Add30ToProgressBar);
    if (TermnateTest) then
    begin
      Synchronize(MainForm.ResetLeds);
      Synchronize(MainForm.DoLabelError);
      TermnateTest := False;
      continue;
    end;
    Synchronize(MainForm.EEPROMSwitchClick_);
    Synchronize(MainForm.Add10ToProgressBar);
    if (TermnateTest) then
    begin
      Synchronize(MainForm.ResetLeds);
      Synchronize(MainForm.DoLabelError);
      TermnateTest := False;
      continue;
    end;
    Synchronize(MainForm.AppsCheckSwitchClick_);
    Synchronize(MainForm.Add10ToProgressBar);
    if (TermnateTest) then
    begin
      Synchronize(MainForm.ResetLeds);
      Synchronize(MainForm.DoLabelError);
      TermnateTest := False;
      continue;
    end;
    Synchronize(MainForm.DoLabelSwitchClick_);
    Synchronize(MainForm.Add10ToProgressBar);
    if (TermnateTest) then
    begin
      Synchronize(MainForm.ResetLeds);
      Synchronize(MainForm.DoLabelError);
      TermnateTest := False;
      continue;
    end;

    Synchronize(MainForm.DoCleanupCmd);
    Synchronize(MainForm.ClearbusyFlag1);
  end;
end;

constructor TProvisionThread.Create(CreateSuspended: boolean);
begin
  event := RTLEventCreate;
  inherited Create(CreateSuspended);
end;

destructor TProvisionThread.Destroy;
begin
  RTLEventDestroy(event);
  inherited;
end;

procedure TProvisionThread.ExecuteThread;
begin
  RTLEventSetEvent(event);
end;

end.
