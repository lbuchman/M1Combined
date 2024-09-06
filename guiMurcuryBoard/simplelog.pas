unit simpleLog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

procedure initLogger();
procedure writeLog(log: string);
procedure closeLog();

implementation

var
  logfFile: TextFile;

procedure initLogger();
var
  homeEnv: string;
  logFileName: string;
  envVarName: string;
begin
  exit;
{$IFDEF WINDOWS}
  envVarName := 'HOMEPATH';
{$ELSE}
  envVarName := 'HOME';
{$ENDIF}
  homeEnv := GetEnvironmentVariable(envVarName);
  logFileName := homeEnv + DirectorySeparator + 'teensyduino.log';
  AssignFile(logfFile, logFileName);
  try
    append(logfFile);
  except
    on E: EInOutError do
      rewrite(logfFile);
  end;
end;

procedure writeLog(log: string);
var
  tS: TTimeStamp;
begin
  exit; // close file is coled before last write, fix me
  try
    tS := DateTimeToTimeStamp(Now);
    writeln(logfFile, IntToStr(tS.Time) + ': ' + log);
  except
    on E: EInOutError do ;
  end;
end;

procedure closeLog();
begin
   exit;
  CloseFile(logfFile);
end;

end.
