unit configurationJson;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, rttiutils, constants;

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
  productNameNode, fwDirNode: TJSONData;
  homeDir: string;
begin
  Result := False;  { Default to failure }
  strList := nil;
  jData := nil;

  { Initialize config with defaults }
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
  
  { Load config file }
  try
    strList := TStringList.Create();
    strList.LoadFromFile(configFileName);
  except
    on E: Exception do begin
      config.error := 'Error loading configuration file: ' + configFileName + ' - ' + E.Message;
      if Assigned(strList) then strList.Free;
      exit(False);
    end;
  end;

  fileData := strList.Text;
  if Assigned(strList) then strList.Free;

  if fileData.Length = 0 then
  begin
    config.error := 'Configuration file is empty';
    exit(False);
  end;

  { Parse JSON }
  try
    jData := GetJSON(fileData);
    
    { Validate JSON structure }
    if jData = nil then
      raise Exception.Create('Invalid JSON: parsing failed');

    { Safe property access }
    productNameNode := jData.FindPath('productName');
    if productNameNode <> nil then
      config.productName := productNameNode.asString
    else
      config.productName := 'unknown';

    fwDirNode := jData.FindPath('fwDir');
    if fwDirNode <> nil then
    begin
      { Get home directory with Windows fallback }
      homeDir := GetEnvironmentVariable('HOME');
      if homeDir = '' then
        homeDir := GetEnvironmentVariable('USERPROFILE');  { Windows fallback }
      if homeDir = '' then
        homeDir := GetEnvironmentVariable('TEMP');  { Last resort }
      
      if homeDir <> '' then
        config.fwDir := homeDir + '/m1mtf/' + fwDirNode.asString
      else
        config.fwDir := '/m1mtf/' + fwDirNode.asString;
    end;
    
    if Assigned(jData) then jData.Free;
    Result := True;  { Success }
  except
    on E: Exception do
    begin
      config.error := 'Error parsing JSON configuration: ' + configFileName + ' - ' + E.Message;
      if Assigned(jData) then jData.Free;
      Result := False;
    end;
  end;
end;

function ConfigurationGet: TConfiguration;
begin
  Result := config;
end;


end.
