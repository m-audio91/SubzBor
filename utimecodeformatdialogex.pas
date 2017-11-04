unit uTimeCodeFormatDialogEx;
{ this file is part of SubzBor.
  SubzBor is a Free subtitle cutting tool with the help of FFmpeg, MKVToolNix
  and it's own set of internal codecs.

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
  Classes, SysUtils, Graphics, StdCtrls, Spin, uTimeCode,
  uTimeCodeFormatDialog, uResourcestrings;

type

  { TTimeCodeFormatDialogEx }

  TTimeCodeFormatDialogEx = class(TTimeCodeFormatDialog)
  private
    FIsNormalTimeCode: TRadioButton;
    FIsFramePos: TRadioButton;
    FFramePosToSecondsHint: TLabel;
    FHasFramePart: TCheckBox;
    FFramerateL: TLabel;
    FFramerate: TFloatSpinEdit;
    procedure LoadExControls(Sender: TObject);
    procedure UpdateValue(Sender: TObject; var CanClose: Boolean);
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

implementation

const
  FramePartToMillisecCap = 'HH:MM:SS:FF >> HH:MM:SS:MS';
  FramePosToSecondsHint = '11111-22222' +LineEnding +'33333-44444';

{ TTimeCodeFormatDialogEx }

procedure TTimeCodeFormatDialogEx.LoadExControls(Sender: TObject);
begin
  //FHasFramePart
 FHasFramePart := TCheckBox.Create(Self);
  with FHasFramePart do
  begin
    Parent := Self;
    Caption := FramePartToMillisecCap;
    ShowHint := True;
    Hint := rsTimecodeHasFramePartdesc;
  end;

  //FIsFramePos
  FIsFramePos := TRadioButton.Create(Self);
  with FIsFramePos do
  begin
    Parent := Self;
    BiDiMode := bdRightToLeft;
    Caption := rsIsFramePos;
    ShowHint := True;
    Hint := rsIsFramePosHint;
  end;

  //FFramePosToSecondsHint
  FFramePosToSecondsHint := TLabel.Create(Self);
  with FFramePosToSecondsHint do
  begin
    Parent := Self;
    Alignment := taLeftJustify;
    Caption := FramePosToSecondsHint;
    ParentFont := False;
  end;

  //FFramerateL
  FFramerateL := TLabel.Create(Self);
  with FFramerateL do
  begin
    Parent := Self;
    BiDiMode := bdRightToLeft;
    Caption := rsFramerate;
    ParentFont := False;
    ShowHint := True;
    Hint := rsFramerateHint;
  end;

  //FFramerate
  FFramerate := TFloatSpinEdit.Create(Self);
  with FFramerate do
  begin
    Parent := Self;
    MinValue := 0.0001;
    MaxValue := 1000;
    DecimalPlaces := 3;
    Alignment := taCenter;
    Increment := 1;
    ShowHint := True;
    Value := 25;
  end;

  FIsNormalTimeCode.State := cbChecked;
  FIsNormalTimeCode.Invalidate;
end;

procedure TTimeCodeFormatDialogEx.UpdateValue(Sender: TObject;
  var CanClose: Boolean);
var
  f: TTimeCodeFormatSettings;
begin
  CanClose := True;
  f := FValue.TimeCodeFormat;
  f.SourceFPS := FFramerate.Value;
  f.HasFrame := FHasFramePart.State = cbChecked;
  f.IsFrame := FIsFramePos.State = cbChecked;
  FValue.TimeCodeFormat := f;
end;

constructor TTimeCodeFormatDialogEx.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  OnShow2 := @LoadExControls;
  OnCloseQuery := @UpdateValue;

  //FIsNormalTimeCode
  FIsNormalTimeCode := TRadioButton.Create(Self);
  with FIsNormalTimeCode do
  begin
    Parent := Self;
    BiDiMode := bdRightToLeft;
    Caption := rsIsNormalTimeCode;
    ShowHint := True;
  end;
end;

end.

