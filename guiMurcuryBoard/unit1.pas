unit Unit1;

uses
  base64, fpjson, rttiutils, fpjsonrtti, Classes, sysutils;

type
  TPeople = class(TPersistent)
  private
    Factive: boolean;
    Fage: byte;
    Flastupdate: TDateTime;
    Fname: string;
    Fweight: double;
  public
  published
    property active: boolean read Factive write Factive default True;
    property Name: string read Fname write Fname;
    property age: byte read Fage write Fage;
    property weight: double read Fweight write Fweight;
    property lastupdate: TDateTime read Flastupdate write Flastupdate;
  end;


  function ObjectToJson(AObject: TObject): TJSONObject;
  var
    VStreamer: TJSONStreamer;
  begin
    VStreamer := TJSONStreamer.Create(nil);
    try
      try
        Result := VStreamer.ObjectToJSON(AObject);
      except
        FreeAndNil(Result);
        raise;
      end;
    finally
      VStreamer.Free;
    end;
  end;

  procedure JsonToObject(AJSON: TJSONObject; AObject: TObject); overload;
  var
    VDeStreamer: TJSONDeStreamer;
  begin
    VDeStreamer := TJSONDeStreamer.Create(nil);
    try
{$IF FPC_FULLVERSION >= 20701}
      VDeStreamer.CaseInsensitive := True;
{$ENDIF}
      VDeStreamer.JSONToObject(AJSON, AObject);
    finally
      VDeStreamer.Free;
    end;
  end;

procedure JsonToObject(AJSON: TJSONStringType; AObject: TObject); overload;
var
  VDeStreamer: TJSONDeStreamer;
begin
  VDeStreamer := TJSONDeStreamer.Create(nil);
  try
{$IF FPC_FULLVERSION >= 20701}
    VDeStreamer.CaseInsensitive := True;
{$ENDIF}
    VDeStreamer.JSONToObject(AJSON, AObject);
  finally
    VDeStreamer.Free;
  end;
end;

var
  JsonObj: TJSONObject;
  I: Integer;
  People1, People2: TPeople;
  es, ds: String;

begin
  People1:=TPeople.Create;
  People1.active:=True;
  People1.age:=10;
  People1.weight:=80.5;
  People1.Name:='John';

  JsonObj := ObjectToJson(People1);
  People1.Free;

  WriteLn('***********************************');
  WriteLn('Object as JSON:');
  WriteLn('***********************************');
  WriteLn(JsonObj.FormatJSON);
  WriteLn('***********************************');
  WriteLn;

  es := EncodeStringBase64(JsonObj.FormatJSON);
  WriteLn('***********************************');
  WriteLn('String encodned Base64:');
  WriteLn('***********************************');
  WriteLn(es);
  WriteLn('***********************************');
  WriteLn;

  ds := DecodeStringBase64(es);
  WriteLn('***********************************');
  WriteLn('Decoded string:');
  WriteLn('***********************************');
  WriteLn(ds);
  WriteLn('***********************************');
  WriteLn;

  People2:=TPeople.Create;
  JsonToObject(ds, People2);

  WriteLn('***********************************');
  WriteLn('Json To Object');
  WriteLn('***********************************');
  WriteLn(People2.Name);
  WriteLn(People2.active);
  WriteLn(People2.age);
  WriteLn(People2.weight);
  WriteLn('***********************************');
  WriteLn;
  People2.Free;
end.

