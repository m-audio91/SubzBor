unit uAbout;
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, uUrlLabel, uSBConst;

type

  { TSBAbout }

  TSBAbout = class(TForm)
    Description: TLabel;
    LinksBox: TPanel;
    ContactBox: TPanel;
    ContactMe: TLabel;
    UsedTools: TLabel;
    SBVersionL: TLabel;
    Logo: TImage;
    Logo1: TImage;
    procedure FormCreate(Sender: TObject);
  end;

var
  SBAbout: TSBAbout;

implementation

{$R *.lfm}

{ TSBAbout }

procedure TSBAbout.FormCreate(Sender: TObject);
var
  AUrl: TUrlLabel;
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    AUrl := TUrlLabel.Create(Self);
    with AUrl do
    begin
      Parent := ContactBox;
      Font.Color := clBlue;
      case i of
      0: Caption := urlIssueTracker;
      1: Caption := urlHome;
      2: Caption := ContactMail;
      end;
    end;
  end;
  for i := 0 to 4 do
  begin
    AUrl := TUrlLabel.Create(Self);
    with AUrl do
    begin
      Parent := LinksBox;
      Font.Color := clBlue;
      case i of
      0: Caption := urlFFmpeg;
      1: Caption := urlMkvToolNix;
      2: Caption := urlLazarus;
      3: Caption := urlInkScape;
      4: Caption := urlGreenfishIconEditor;
      end;
    end;
  end;
end;

end.

