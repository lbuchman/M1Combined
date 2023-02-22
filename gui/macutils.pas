unit macUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, rttiutils, strutils;

function BinaryToMacString(macInt: int64; insertSpacer: boolean): string;
function MacStringToLong(uidStartRange: string; offset: integer): int64;
function MacStringToUid(uidStartRange: string): string;
function getNextUid(uidStartRange: string; UidCount: integer): string;
function StrToMac(Str: string): string;

implementation

//var
// jData: TJSONData;

function StrToMac(Str: string): string;
var
  counter: integer;
  DecValue: int64;
begin
  counter := 1;
  Result := '';
  DecValue := 1;
  Str := AnsiUpperCase(Str);
  Counter := Length(Str);
  while Counter > 0 do
  begin
    if Result = '' then Result := Str[counter]
    else
      Result := Str[counter] + Result;
    if odd(counter) then
    begin
      if counter > 2 then
        Result := ':' + Result;
    end;
    Dec(counter);
  end;
end;

function BinaryToMacString(macInt: int64; insertSpacer: boolean): string;
begin
  Result := IntToHex(macInt, 12);
  if insertSpacer then
    Result := StrToMac(Result);
end;

function MacStringToLong(uidStartRange: string; offset: integer): int64;
var
  mac: string;
  macInt: int64;
begin
  mac := MacStringToUid(uidStartRange);
  macInt := Hex2Dec64(mac);
  Result := macInt;
end;

function MacStringToUid(uidStartRange: string): string;
var
  mac: string;
begin
  mac := StringReplace(uidStartRange, ':', '', [rfReplaceAll, rfIgnoreCase]);
  mac := StringReplace(mac, '-', '', [rfReplaceAll, rfIgnoreCase]);
  Result := mac;
end;

function getNextUid(UidStartRange: string; UidCount: integer): string;
var
  bunaryUid: int64;
begin
  bunaryUid := MacStringToLong(UidStartRange, UidCount);
  bunaryUid := bunaryUid + UidCount;
  Result := BinaryToMacString(bunaryUid, False);
end;

end.
