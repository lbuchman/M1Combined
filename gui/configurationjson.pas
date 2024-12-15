unit configurationJson;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, rttiutils;

type

  TConfigration = record
    ScannerUdpPort: string;
    Logdir: string;
    ProductName: String;
    fwDir: String;
    error: String;
  end;


var
  config: TConfigration;


function ConfigurationGet: TConfigration;
function ReadConfigFile(): boolean;

implementation

function ReadConfigFile: boolean;
var
  strList: TStringList;
  fileData: String;
  configFileName: string;
  jData: TJSONData;
begin

   with config do begin
    Logdir := '/tmp';
    ProductName := 'mnplus';
    fwDir := 'stm32mp15-lenels2-m1_MNP';
    error := '';
  end;

  configFileName := '/var/snap/m1tfd1/current/config.json';
  try
    strList := TStringList.Create();
    strList.LoadFromFile(configFileName);
  except
    on E: Exception do begin
      config.error := '1 ' + configFileName;
      strList.Free;
      exit(false);
    end;
  end;

  fileData := strList.Text;
  strList.Free;

  if fileData.Length = 0 then exit(false);
   try
    jData := GetJSON(fileData);
    config.productName := jdata.FindPath('productName').asString;
    config.fwDir := GetEnvironmentVariable('HOME') + '/m1mtf/' + jdata.FindPath('fwDir').asString;
    jData.Free;
  except
    on E: Exception do
    begin
      config.error := '2 ' + configFileName;
      jData.Free;
      exit(false);
    end;
  end;
end;

function ConfigurationGet: TConfigration;
begin
  config.ScannerUdpPort := '10000'; //jObject.get('scannerUdpPort');
  Result := config;
end;


end.
