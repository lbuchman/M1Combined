unit configurationJson;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, rttiutils;

type
  TDevice = record
    ip: string;
    desc: string;
  end;

  DevicesrArray = array of TDevice;


procedure ConfigurationGet;
procedure setFormGeometryJson(form: string; top, left, Width, Height: integer);
procedure writeConfiguration;
procedure getFormGeometryJson(form: string; var top, left, Width, Height: integer);
function getDevicesJson(size: integer; var devices: array of TDevice): integer;
function addDeviceJson(ipAddress : string; deviceDescr : string) : boolean;

implementation

var
  jData: TJSONData;

function readConfigFile(): string;
var
  strList: TStringList;
  homeEnv: string;
  configFileName: string;
  envVarName: string;
begin
  {$IFDEF WINDOWS}
  envVarName := 'HOMEPATH';
{$ELSE}
  envVarName := 'HOME';
{$ENDIF}
  homeEnv := GetEnvironmentVariable(envVarName);
  configFileName := homeEnv + DirectorySeparator + 'teensyduino.json';
  try
    strList := TStringList.Create();
    strList.LoadFromFile(configFileName);
  except
    on E: Exception do Result := '';
  end;

  Result := strList.Text;
  strList.Free;
end;

procedure ConfigurationGet;
var
  fileData: string;

begin
  fileData := readConfigFile();
  if fileData.Length = 0 then
  begin
    jData := GetJSON('{}');
    exit;
  end;
  try
    jData := GetJSON(fileData);
  except
    on E: Exception do
    begin
      jData := GetJSON('{}');
    end;
  end;

end;

procedure setFormGeometryJson(form: string; top, left, Width, Height: integer);
var
  jObject: TJSONObject;
  tmpObj: TJSONObject;
begin

  tmpObj := TJSONObject(jdata.FindPath(form));
  if tmpObj <> nil then
  begin
    jObject := tmpObj as TJSONObject;
    jObject.Integers['top'] := top;
    jObject.Integers['left'] := left;
    jObject.Integers['width'] := Width;
    jObject.Integers['height'] := Height;
  end
  else
  begin
    jObject := jData as TJSONObject;
    tmpObj := TJSONObject.Create(['top', top, 'left', left,
      'width', Width, 'height', Height]);

    jObject.Add(form, tmpObj);
    //ToDo ? tmpObj.Free;
  end;

end;

procedure writeConfiguration;
var
  strList: TStringList;
  homeEnv: string;
  configFileName: string;
  envVarName: string;
  s: string;
begin
    {$IFDEF WINDOWS}
  envVarName := 'HOMEPATH';
{$ELSE}
  envVarName := 'HOME';
{$ENDIF}
  homeEnv := GetEnvironmentVariable(envVarName);
  configFileName := homeEnv + DirectorySeparator + 'teensyduino.json';
  strList := TStringList.Create();
  try
    s := jData.FormatJSON;
    strList.Text := s;
    strList.SaveToFile(configFileName);
  except
    on E: Exception do
    begin
    end;
  end;
  strList.Free;
  if jData <> nil then jData.Free;
end;

procedure getFormGeometryJson(form: string; var top, left, Width, Height: integer);
var
  jObject: TJSONObject;
  tmpObj: TJSONObject;
begin
  tmpObj := TJSONObject(jdata.FindPath(form));
  if tmpObj <> nil then
  begin
    jObject := tmpObj as TJSONObject;
    top := jObject.Integers['top'];
    left := jObject.Integers['left'];
    Width := jObject.Integers['width'];
    Height := jObject.Integers['height'];
  end
  else
  begin
    top := 100;
    left := 100;
    Width := 300;
    Height := 300;
  end;
end;

function getDevicesJson(size: integer; var devices: array of TDevice): integer;
var
  jObject: TJSONObject;
  jArray: TJSonArray;
  Count: integer;
begin
  Result := 0;
  jArray := TJSONArray(jData.FindPath('devices'));
  if jArray <> nil then
  begin
    Result := jArray.Count;
    for Count := 0 to Pred(jArray.Count) do
    begin
      if Count = Pred(size) then exit(Count);
      jObject := jArray.Items[Count] as TJSONObject;
      devices[Count].ip := jObject.Strings['ip'];
      devices[Count].desc := jObject.Strings['desc'];
    end;
  end;
end;

function addDeviceJson(ipAddress : string; deviceDescr : string) : boolean;
var
  jArray: TJSonArray;
  count : integer;
  jObject, tmpObj: TJSONObject;
begin
  jArray := TJSONArray(jData.FindPath('devices'));
  if jArray <> nil then
  begin
    for count := 0 to Pred(jArray.Count) do
    begin
      jObject := jArray.Items[count] as TJSONObject;
      if jObject.Strings['ip'] = ipAddress then begin
           jObject.Strings['desc'] := deviceDescr;
           exit(false);
      end;
    end;
    jObject := TJSONObject.Create(['ip', ipAddress, 'desc', deviceDescr]);
    jArray.Add(jObject);
  end
  else begin
    tmpObj :=  jData as TJSONObject;
    jArray := TJSonArray.Create;
    jObject := TJSONObject.Create(['ip', ipAddress, 'desc', deviceDescr]);
    jArray.Add(jObject);
    tmpObj.Add('devices', jArray);
  end;
  result := true;
end;

end.
