unit uCharEnc;
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniPropStorage, uUrlLabel, uSBConst;

type

  { TSBCharEnc }

  TSBCharEnc = class(TForm)
    EncodingName: TComboBox;
    FaSubripAdL: TLabel;
    IniProps: TIniPropStorage;
    EncodingNameL: TLabel;
    procedure FormCreate(Sender: TObject);
  end;

var
  SBCharEnc: TSBCharEnc;

implementation

{$R *.lfm}

{ TSBCharEnc }

procedure TSBCharEnc.FormCreate(Sender: TObject);
var
  AUrl: TUrlLabel;
begin
  AUrl := TUrlLabel.Create(Self);
  with AUrl do
  begin
    Parent := Self;
    Align := alTop;
    Font.Color := clBlue;
    BorderSpacing.Top := 2;
    BorderSpacing.Bottom := 8;
    BorderSpacing.Left := 8;
    BorderSpacing.Right := 8;
    Caption := urlFasubRip;
    Top := MaxInt;
  end;
end;

end.

