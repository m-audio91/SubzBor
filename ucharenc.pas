unit uCharEnc;
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
  Classes, SysUtils, LCLType, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, uUrlLabel, uSBConst;

type

  { TSBCharEnc }

  TSBCharEnc = class(TForm)
    EncodingName: TComboBox;
    FaSubripAdL: TLabel;
    EncodingNameL: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FShowInternalEncodings: Boolean;
  public
    property ShowInternalEncodings: Boolean read FShowInternalEncodings write FShowInternalEncodings;
  end;

var
  SBCharEnc: TSBCharEnc;

implementation

{$R *.lfm}
{$R charencsiconv.res}
{$R charencslaz.res}

const
  IconvEncsResName = 'charencsiconv';
  LazEncsResName = 'charencslaz';

{ TSBCharEnc }

procedure TSBCharEnc.FormCreate(Sender: TObject);
var
  AUrl: TUrlLabel;
begin
  FShowInternalEncodings := False;

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

procedure TSBCharEnc.FormShow(Sender: TObject);
var
  rs: TResourceStream;
  rname: String;
begin
  if FShowInternalEncodings then
    rname := LazEncsResName
  else
    rname := IconvEncsResName;
  rs := TResourceStream.Create(HInstance, rname, RT_RCDATA);
  try
    EncodingName.Items.LoadFromStream(rs);
    EncodingName.ItemIndex := EncodingName.Items.IndexOf(encUTF8);
  finally
    rs.Free;
  end;
end;

end.

