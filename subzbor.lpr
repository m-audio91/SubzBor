program subzbor;
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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SysUtils, uMain, uDatas, uPrefs, ucharenc, uResourcestrings, uAbout,
  ui18nGuide;

{$R *.res}

begin
  Application.Title:='SubzBor';
  Application.Scaled:=True;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TSBMain, SBMain);
  Application.CreateForm(TSBDatas, SBDatas);
  Application.CreateForm(TSBPrefs, SBPrefs);
  Application.CreateForm(TSBCharEnc, SBCharEnc);
  Application.CreateForm(TSBAbout, SBAbout);
  Application.CreateForm(TSBi18nGuide, SBi18nGuide);
  DefaultFormatSettings.DecimalSeparator:='.';
  Application.Run;
end.

