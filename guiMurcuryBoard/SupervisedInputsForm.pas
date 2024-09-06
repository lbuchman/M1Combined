unit SupervisedInputsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, jsonparser, fpJSON, configurationJson, iovalues;

type
  TSInput = record
    input: array [0..3] of ^TRadioButton;
  end;


type

  { TSupervisedInputs }

  TSupervisedInputs = class(TForm)
    F2FReaderGroupBoxSuprevBuz: TGroupBox;
    F2FReaderGroupBoxSuprevBuz1: TGroupBox;
    F2FReaderGroupBoxSuprevBuz2: TGroupBox;
    F2FReaderGroupBoxSuprevBuz3: TGroupBox;
    F2FReaderGroupBoxSuprevBuz4: TGroupBox;
    F2FReaderGroupBoxSuprevBuz5: TGroupBox;
    F2FReaderGroupBoxSuprevRedLed: TGroupBox;
    F2FReaderGroupBoxSuprevRedLed1: TGroupBox;
    Input21k: TRadioButton;
    Input61k: TRadioButton;
    Input11k: TRadioButton;
    Input51k: TRadioButton;
    Input22k: TRadioButton;
    Input62k: TRadioButton;
    Input12k: TRadioButton;
    Input52k: TRadioButton;
    Input2Cut: TRadioButton;
    Input6Cut: TRadioButton;
    Input1Cut: TRadioButton;
    Input5Cut: TRadioButton;
    Input2Short: TRadioButton;
    Input6Short: TRadioButton;
    Input1Short: TRadioButton;
    Input5Short: TRadioButton;
    Input31k: TRadioButton;
    Input41k: TRadioButton;
    Input81k: TRadioButton;
    Input71k: TRadioButton;
    Input32k: TRadioButton;
    Input42k: TRadioButton;
    Input82k: TRadioButton;
    Input72k: TRadioButton;
    Input3Cut: TRadioButton;
    Input4Cut: TRadioButton;
    Input8Cut: TRadioButton;
    Input7Cut: TRadioButton;
    Input3Short: TRadioButton;
    Input4Short: TRadioButton;
    Input8Short: TRadioButton;
    Input7Short: TRadioButton;
    pullTimer: TTimer;
    UpdateStopTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pullTimerTimer(Sender: TObject);
    procedure SuprRadioButtonClick(Sender: TObject);
    procedure UpdateStopTimerTimer(Sender: TObject);
  private
    Inputs: array [0..7] of TSInput;
    doUpdate: boolean;
  public
    procedure SaveGeometry();
    procedure UpdateData(var jData: TJSONData; cmd: string);

  end;

var
  SupervisedInputs: TSupervisedInputs;

implementation

uses mainForm;

{$R *.lfm}

{ TSupervisedInputs }

procedure TSupervisedInputs.SaveGeometry();
begin
  setFormGeometryJson('supervisedForm', top, left, Width, Height);
end;

procedure TSupervisedInputs.FormCreate(Sender: TObject);
begin
  Inputs[0].input[3] := @Input1Cut;
  Inputs[0].input[2] := @Input1Short;
  Inputs[0].input[1] := @Input11k;
  Inputs[0].input[0] := @Input12k;
  Inputs[1].input[3] := @Input2Cut;
  Inputs[1].input[2] := @Input2Short;
  Inputs[1].input[1] := @Input21k;
  Inputs[1].input[0] := @Input22k;
  Inputs[2].input[3] := @Input3Cut;
  Inputs[2].input[2] := @Input3Short;
  Inputs[2].input[1] := @Input31k;
  Inputs[2].input[0] := @Input32k;
  Inputs[3].input[3] := @Input4Cut;
  Inputs[3].input[2] := @Input4Short;
  Inputs[3].input[1] := @Input41k;
  Inputs[3].input[0] := @Input42k;
  Inputs[4].input[3] := @Input5Cut;
  Inputs[4].input[2] := @Input5Short;
  Inputs[4].input[1] := @Input51k;
  Inputs[4].input[0] := @Input52k;
  Inputs[5].input[3] := @Input6Cut;
  Inputs[5].input[2] := @Input6Short;
  Inputs[5].input[1] := @Input61k;
  Inputs[5].input[0] := @Input62k;
  Inputs[6].input[3] := @Input7Cut;
  Inputs[6].input[2] := @Input7Short;
  Inputs[6].input[1] := @Input71k;
  Inputs[6].input[0] := @Input72k;
  Inputs[7].input[3] := @Input8Cut;
  Inputs[7].input[2] := @Input8Short;
  Inputs[7].input[1] := @Input81k;
  Inputs[7].input[0] := @Input82k;

  doUpdate := True;
end;

procedure TSupervisedInputs.FormShow(Sender: TObject);
var
  _top, _left, _width, _height: integer;
begin
  _top := 0;
  _left := 0;
  _width := 0;
  _height := 0;
  getFormGeometryJson('supervisedForm', _top, _left, _width, _height);
  top := _top;
  left := _left;
  Width := _width;
  Height := _height;
end;

procedure TSupervisedInputs.pullTimerTimer(Sender: TObject);
var
  ioValues: TIOValues;
  i: integer;
  retStatus: boolean;
begin
  if not doUpdate then exit;
  Initialize(@ioValues, SizeOf(ioValues));
  retStatus := MainAppForm.getIoValues(ioValues, inputsFormDest);
  if not retStatus then exit;
  for i := 0 to 7 do
  begin
    if ioValues.inputs[i] < 4 then
      Inputs[i].input[ioValues.inputs[i]]^.Checked := True;
  end;
end;

procedure TSupervisedInputs.SuprRadioButtonClick(Sender: TObject);
var
  input, state: integer;
begin
  if not Visible then exit;
  input := TRadioButton(Sender).Parent.tag;
  state := TRadioButton(Sender).tag;
  MainAppForm.SendCommand('setsupervisedinput', IntToStr(input) + ' ' + IntToStr(state));
  doUpdate := False;
  UpdateStopTimer.Enabled := False;
  UpdateStopTimer.Enabled := True;
end;

procedure TSupervisedInputs.UpdateStopTimerTimer(Sender: TObject);
begin
  doUpdate := True;
  UpdateStopTimer.Enabled := False;
end;

procedure TSupervisedInputs.UpdateData(var jData: TJSONData; cmd: string);
var
  jArray: TJSonArray;
  i: integer;
begin
  exit;
  if (cmd <> 'getsupervisedinputs') then exit;
  try
    jArray := TJSONArray(jData.FindPath('inputs'));
    for i := 0 to Pred(jArray.Count) do
    begin
      Inputs[i].input[jArray.Items[i].AsInteger]^.Checked := True;
    end

  except
    on E: Exception do ;
  end;
end;

end.
