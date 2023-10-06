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
  logForm,
  logger,
  configurationJson,
  about;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TmainForm, mainForm);
  Application.CreateForm(TLoggerForm, LoggerForm);
  Application.CreateForm(TaboutForm, aboutForm);
  Application.Run;
end.
