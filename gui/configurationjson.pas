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
  configFileName := '/var/snap/m1tfd1/current/config.json';
  try
    strList := TStringList.Create();
    strList.LoadFromFile(configFileName);
  except
    on E: Exception do exit(false);
  end;

  fileData := strList.Text;
  if fileData.Length = 0 then exit(false);
   try
    jData := GetJSON(fileData);
    config.productName := jdata.FindPath('productName').asString;
    config.fwDir := GetEnvironmentVariable('HOME') + '/m1mtf/' + jdata.FindPath('fwDir').asString;
  except
    on E: Exception do
    begin
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
