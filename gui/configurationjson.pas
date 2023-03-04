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

var
  jData: TJSONData;

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

  // jObject := jData as TJSONObject;
  // config.IctFWFilePath := jObject.get('ictFWFilePath');
  // config.LayoutFilePath := jObject.get('layoutFilePath');
  // config.STM32_Programmer_CLI := jObject.get('programmingCommand');
  config.ScannerUdpPort := '10000'; //jObject.get('scannerUdpPort');
  // config.M1SerialDev := jObject.get('m1SerialDev');
  // config.Logdir := jObject.get('logdir');
  // config.TestBoardTerminalDev := jObject.get('testBoardTerminalDev');
  // config.M1TfcExecutable := jObject.get('m1TfcExecutable');
  // config.MemTestSize1MBBlocks := jObject.get('memTestSize1MBBlocks');
  // config.ForceEppromOverwrite := jObject.get('forceEppromOverwrite');
  // config.VendorSite := jObject.get('vendorSite');
  // config.SkipTestpointCheck := jObject.get('skipTestpointCheck');
  // config.UidStartRange := jObject.get('uidStartRange');

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
