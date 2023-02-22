unit logger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, dateutils;

function log(logLevel: string; device: string; str: string): string;
function getEpochTime() : Integer;

implementation

function log(logLevel: string; device: string; str: string): string;
var
  ThisMoment: TDateTime;

begin
  ThisMoment := Now;
  Result := '[' + FormatDateTime('yyyy-mm-dd hh:nn:ss', ThisMoment) +
    ']' + ' [' + device + '] ' + logLevel + ':        ' + str;
end;

function getEpochTime() : Integer;
var
  ThisMoment: TDateTime;
begin
 ThisMoment := Now;
 result := DateTimeToUnix(ThisMoment);
end;

end.
