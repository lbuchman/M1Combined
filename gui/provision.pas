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
    reTestMode: boolean;
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
    // ICT
    Synchronize(MainForm.ICTTestSwitchClick_);
    Synchronize(MainForm.Add15ToProgressBar);
    if (TermnateTest) then
    begin
      Synchronize(MainForm.ResetLeds);
      TermnateTest := False;
      Synchronize(MainForm.DoLabelError);
      Synchronize(MainForm.DoCleanupCmd);
      continue;
    end;

    if not reTestMode then
    begin
      // MAC
      Synchronize(MainForm.MacProgSwitchClick_);
      Synchronize(MainForm.Add3ToProgressBar);
      if (TermnateTest) then
      begin
        Synchronize(MainForm.ResetLeds);
        Synchronize(MainForm.DoLabelError);
        TermnateTest := False;
        Synchronize(MainForm.DoCleanupCmd);
        continue;
      end;

      // Flash
      Synchronize(MainForm.FlashSwitchClick_);
      Synchronize(MainForm.Add40ToProgressBar);
      if (TermnateTest) then
      begin
        Synchronize(MainForm.ResetLeds);
        Synchronize(MainForm.DoLabelError);
        TermnateTest := False;
        Synchronize(MainForm.DoCleanupCmd);
        continue;
      end;

    end
    else begin
      Synchronize(MainForm.Add40ToProgressBar);
      Synchronize(MainForm.Add3ToProgressBar);
    end;

    // Func Test
    Synchronize(MainForm.FuncTestSwitchClick_);
    Synchronize(MainForm.Add40ToProgressBar);
    if (TermnateTest) then
    begin
      Synchronize(MainForm.ResetLeds);
      Synchronize(MainForm.DoLabelError);
      TermnateTest := False;
      Synchronize(MainForm.DoCleanupCmd);
      continue;
    end;

    // EEPROM
    if not reTestMode then
    begin
      Synchronize(MainForm.EEPROMSwitchClick_);
      Synchronize(MainForm.Add3ToProgressBar);
      if (TermnateTest) then
      begin
        Synchronize(MainForm.ResetLeds);
        Synchronize(MainForm.DoLabelError);
        TermnateTest := False;
        Synchronize(MainForm.DoCleanupCmd);
        continue;
      end;
    end
    else Synchronize(MainForm.Add3ToProgressBar);

    // APPs check
    Synchronize(MainForm.AppsCheckSwitchClick_);
    Synchronize(MainForm.Add3ToProgressBar);
    if (TermnateTest) then
    begin
      Synchronize(MainForm.ResetLeds);
      Synchronize(MainForm.DoLabelError);
      TermnateTest := False;
      Synchronize(MainForm.DoCleanupCmd);
      continue;
    end;

    // Print Label
    Synchronize(MainForm.DoLabelSwitchClick_);
    Synchronize(MainForm.Add3ToProgressBar);
    if (TermnateTest) then
    begin
      Synchronize(MainForm.ResetLeds);
      Synchronize(MainForm.DoLabelError);
      TermnateTest := False;
      Synchronize(MainForm.DoCleanupCmd);
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
