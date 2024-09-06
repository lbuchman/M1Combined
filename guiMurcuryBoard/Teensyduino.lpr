program Teensyduino;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
//  CMem,
   {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  runtimetypeinfocontrols,
  lazcontrols,
  mainForm,
  bdtfRestApi,
  blinkLed,
  UDPServer,
  iovalues,
  simpleLog, configurationJson;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainAppForm, MainAppForm);
  Application.Run;
end.
