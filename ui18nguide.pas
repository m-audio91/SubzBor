unit ui18nGuide;
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, uSBConst, uUrlLabel;

type

  { TSBi18nGuide }

  TSBi18nGuide = class(TForm)
    i18nTranslateAllLinesL: TLabel;
    UpdateExistingPoL: TLabel;
    Image1: TImage;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image13: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    i18nInstructionL: TLabel;
    LangCodesURLL: TUrlLabel;
    ContactMailL: TUrlLabel;
    GithubPageL: TUrlLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  SBi18nGuide: TSBi18nGuide;

implementation

{$R *.lfm}

{ TSBi18nGuide }

procedure TSBi18nGuide.FormCreate(Sender: TObject);
begin
  LangCodesURLL := TUrlLabel.Create(Self);
  with LangCodesURLL do
  begin
    Parent := Self;
    Font.Color := clBlue;
    Caption := urlLangCodes;
  end;
  ContactMailL := TUrlLabel.Create(Self);
  with ContactMailL do
  begin
    Parent := Self;
    Font.Color := clBlue;
    Caption := ContactMail;
  end;
  GithubPageL := TUrlLabel.Create(Self);
  with GithubPageL do
  begin
    Parent := Self;
    Font.Color := clBlue;
    Caption := urlSBLanguages;
  end;
end;

end.

