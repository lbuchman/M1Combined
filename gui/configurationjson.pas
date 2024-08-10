unit configurationJson;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, rttiutils;

type

  TConfigration = record
    ScannerUdpPort: string;
    Logdir: string;
  end;

function ConfigurationGet: TConfigration;
procedure TruncateLogFileDelete(config: TConfigration; serialN: string);

implementation

function ReadConfigFile(): string;
var
  strList: TStringList;
  homeEnv: string;
  configFileName: string;
  envVarName: string;
begin
  Result := '';
{$IFDEF WINDOWS}
  envVarName := 'HOMEPATH';
{$ELSE}
  envVarName := 'HOME';
{$ENDIF}
  homeEnv := GetEnvironmentVariable(envVarName);
  configFileName := homeEnv + DirectorySeparator + 'configM1.json';
  strList := TStringList.Create();
  strList.LoadFromFile(configFileName);
  Result := strList.Text;
  strList.Free;
end;

function ConfigurationGet: TConfigration;
var
  config: TConfigration;
begin
  config.ScannerUdpPort := '10000'; //jObject.get('scannerUdpPort');
  Result := config;
end;

procedure TruncateLogFileDelete(config: TConfigration; serialN: string);
var
  F: file of longint;
  filePath: string;
begin
  if serialN = '' then exit;
  filepath := config.Logdir + '/' + serialN + '.log';
  Assign(F, filepath);
  Rewrite(F);
  Close(F);
end;

end.
