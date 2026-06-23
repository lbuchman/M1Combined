unit commandlinebuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TCommandLineBuilder - Builds consistent command-line arguments }
  TCommandLineBuilder = class
  public
    { Build command with serial and debug level }
    function BuildTestCommand(Command: string; Serial: string; DebugLevel: string): TStringList;
    
    { Build with additional custom arguments }
    function BuildTestCommandWithExtra(Command: string; Serial: string; 
      DebugLevel: string; ExtraArgs: array of string): TStringList;
    
    { Build cloud operation command }
    function BuildCloudCommand(Operation: string): TStringList;
  end;

implementation

function TCommandLineBuilder.BuildTestCommand(Command: string; Serial: string; DebugLevel: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Add('-s');
  Result.Add(Trim(Serial));
  Result.Add('-d');
  Result.Add(DebugLevel);
end;

function TCommandLineBuilder.BuildTestCommandWithExtra(Command: string; Serial: string; 
  DebugLevel: string; ExtraArgs: array of string): TStringList;
var
  i: integer;
begin
  Result := BuildTestCommand(Command, Serial, DebugLevel);
  for i := Low(ExtraArgs) to High(ExtraArgs) do
    if ExtraArgs[i] <> '' then
      Result.Add(ExtraArgs[i]);
end;

function TCommandLineBuilder.BuildCloudCommand(Operation: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Add(Operation);
end;

end.
