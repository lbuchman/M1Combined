program gui;

{$mode objfpc}{$H+}

uses
  cthreads,
  Interfaces, // this includes the LCL widgetset
  Forms,
  lazcontrols,
  laz_synapse,
  main,
  provision,
  macUtils,
  scannerClient,
  logForm,
  logger,
  configurationJson;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TmainForm, mainForm);
  Application.CreateForm(TLoggerForm, LoggerForm);
  Application.Run;
end.
