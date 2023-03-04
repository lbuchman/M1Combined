unit logger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, dateutils;

function log(logLevel: string; device: string; str: string): string;
function getEpochTime(): integer;

implementation

function log(logLevel: string; device: string; str: string): string;
var
  ThisMoment: TDateTime;

begin
  ThisMoment := Now;
  Result := '[' + FormatDateTime('yyyy-mm-dd hh:nn:ss', ThisMoment) +
    ']' + ' [' + device + '] ' + logLevel + ':        ' + str;
end;

function getEpochTime(): integer;
var
  ThisMoment: TDateTime;
begin
  ThisMoment := Now;
  Result := DateTimeToUnix(ThisMoment);
end;

end.
