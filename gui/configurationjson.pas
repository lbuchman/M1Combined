unit configurationJson;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, rttiutils;

type

  TConfiguration = record
    ScannerUdpPort: string;
    Logdir: string;
    ProductName: String;
    fwDir: String;
    M1TfcExecutable: String;
    IctFWFilePath: String;
    LayoutFilePath: String;
    STM32_Programmer_CLI: String;
    M1SerialDev: String;
    TestBoardTerminalDev: String;
    VendorSite: String;
    UidStartRange: String;
    error: String;
  end;


var
  config: TConfiguration;


function ConfigurationGet: TConfiguration;
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
    ScannerUdpPort := '10000';
    M1TfcExecutable := '';
    IctFWFilePath := '';
    LayoutFilePath := '';
    STM32_Programmer_CLI := '';
    M1SerialDev := '';
    TestBoardTerminalDev := '';
    VendorSite := '';
    UidStartRange := '';
    error := '';
  end;

  configFileName := '/var/snap/m1tfd1/current/config.json';
  try
    strList := TStringList.Create();
    strList.LoadFromFile(configFileName);
  except
    on E: Exception do begin
      config.error := 'Error loading configuration file: ' + configFileName + ' - ' + E.Message;
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
      config.error := 'Error parsing JSON configuration: ' + configFileName + ' - ' + E.Message;
      jData.Free;
      exit(false);
    end;
  end;
end;

function ConfigurationGet: TConfiguration;
begin
  Result := config;
end;


end.
