unit logger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, dateutils;

const
  LOG_FORMAT = '[yyyy-mm-dd hh:nn:ss] [%s] %s:        %s';
  LOG_PADDING = ':        ';

function Log(logLevel: string; device: string; str: string): string;
function GetEpochTime(): integer;

implementation

function Log(logLevel: string; device: string; str: string): string;
var
  ThisMoment: TDateTime;
begin
  ThisMoment := Now;
  Result := '[' + FormatDateTime('yyyy-mm-dd hh:nn:ss', ThisMoment) +
    ']' + ' [' + device + '] ' + logLevel + LOG_PADDING + str;
end;

function GetEpochTime(): integer;
var
  ThisMoment: TDateTime;
begin
  ThisMoment := Now;
  Result := DateTimeToUnix(ThisMoment);
end;

end.
