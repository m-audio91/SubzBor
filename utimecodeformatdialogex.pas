unit uTimeCodeFormatDialogEx;
{ SubzBor: Free subtitle cutting tool with the help of FFmpeg and MKVToolNix

  Copyright (C) 2017 Mohammadreza Bahrami m.audio91@gmail.com

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, Spin,
  uTimeCodeFormatDialog, uResourcestrings;

type

  { TTimeCodeFormatDialogEx }

  TTimeCodeFormatDialogEx = class(TTimeCodeFormatDialog)
  private
    FIsMillisecondAFrameNo: TCheckBox;
    FIsMillisecondAFrameNoL: TLabel;
    FFrameNoToMillisecFormula: TLabel;
    FFramerate: TSpinEdit;
    procedure LoadExControls(Sender: TObject);
    function GetFrameNoToMillisecState: Boolean;
    function GetFramerate: Integer;
  public
    property IsMillisecondAFrameNo: Boolean read GetFrameNoToMillisecState;
    property Framerate: Integer read GetFramerate;
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

implementation

const
  FrameNoToFPSFormula = '(1000/FPS)*FrameNo = Millisecond';

{ TTimeCodeFormatDialogEx }

procedure TTimeCodeFormatDialogEx.LoadExControls(Sender: TObject);
begin
  //FIsMillisecondAFrameNo
 FIsMillisecondAFrameNo := TCheckBox.Create(Self);
  with FIsMillisecondAFrameNo do
  begin
    Parent := Self;
    BiDiMode := bdRightToLeft;
    Caption := rsIsMillisecondAFrameNo;
  end;

  //FIsMillisecondAFrameNoL
  FIsMillisecondAFrameNoL := TLabel.Create(Self);
  with FIsMillisecondAFrameNoL do
  begin
    Parent := Self;
    Alignment := taRightJustify;
    Caption := rsIsMillisecondAFrameNoDesc;
    ParentFont := False;
    Font.Color := clGrayText;
  end;

  //FFramerate
  FFramerate := TSpinEdit.Create(Self);
  with FFramerate do
  begin
    Parent := Self;
    MinValue := 1;
    MaxValue := 1000;
    Alignment := taCenter;
    Increment := 1;
    Hint := rsFramerate;
    ShowHint := True;
    Value := 25;
  end;

  //FFrameNoToMillisecFormula
  FFrameNoToMillisecFormula := TLabel.Create(Self);
  with FFrameNoToMillisecFormula do
  begin
    Parent := Self;
    Alignment := taLeftJustify;
    Caption := FrameNoToFPSFormula;
    ParentFont := False;
    Font.Color := clGrayText;
  end;
end;

function TTimeCodeFormatDialogEx.GetFrameNoToMillisecState: Boolean;
begin
  Result := FIsMillisecondAFrameNo.State = cbChecked;
end;

function TTimeCodeFormatDialogEx.GetFramerate: Integer;
begin
  Result := FFramerate.Value;
end;

constructor TTimeCodeFormatDialogEx.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  OnShow2 := @LoadExControls;
end;

end.

