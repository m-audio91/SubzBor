unit uDatas;
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Buttons, EditBtn,
  Dialogs, ExtCtrls, uSBConst;

type

  { TSBDatas }

  TSBDatas = class(TDataModule)
    GlyphImages144: TImageList;
    GlyphImages96: TImageList;
    GlyphImages192: TImageList;
    GlyphImages120: TImageList;
    GlyphImages: TImageList;
    OpenDlg: TOpenDialog;
    SaveDlg: TSaveDialog;
    TheTimer: TTimer;
    TaskDlg: TTaskDialog;
    procedure DataModuleCreate(Sender: TObject);
  public
    procedure PrepareGlyphs(DPI: Word);
    procedure ChangeGlyph(SBtn: TSpeedButton); overload;
    procedure ChangeGlyph(EdtBtn: TFileNameEdit); overload;
    procedure HandleTranslation;
  end;

resourcestring
  rsTextFiles = 'فایل متنی';
  rsSBDAllFiles = 'همه فایل ها';
  rsSBDOK = 'تایید';
  rsSBDCancel = 'صرف نظر';
  rsTimingsFileFormat = 'ساختار فایل زمان بندی ها';
  rsChooseCorrectTimingsFileFormatWarn = 'ساختار فایل زمان بندی وارد شده را به درستی مشخص نمایید. در غیر اینصورت خوانده نمی شود.';
  rsSBVersion13UpFormat = 'نسخه 1.0.3 و بالا تر';
  rsSBVersion12LowerFormat = 'نسخه 1.0.2 و پایین تر';

var
  SBDatas: TSBDatas;

implementation

{$R *.lfm}

{ TSBDatas }

procedure TSBDatas.DataModuleCreate(Sender: TObject);
begin
  PrepareGlyphs(Screen.PixelsPerInch);
  TaskDlg.RadioButtons.Add;
  TaskDlg.RadioButtons.Add;
  TaskDlg.RadioButtons.Add;
  TaskDlg.Buttons.Add;
  TaskDlg.Buttons.Add;
  HandleTranslation;
end;

procedure TSBDatas.PrepareGlyphs(DPI: Word);
var
  Imgs: TImageList;
begin
  Imgs := GlyphImages96;
  case DPI of
  120..143: Imgs := GlyphImages120;
  144..191: Imgs := GlyphImages144;
  192..1000: Imgs := GlyphImages192;
  end;
  GlyphImages.Clear;
  GlyphImages.Width := Imgs.Width;
  GlyphImages.Height := Imgs.Height;
  GlyphImages.Assign(Imgs);
end;

procedure TSBDatas.ChangeGlyph(SBtn: TSpeedButton);
begin
  SBtn.Glyph.Assign(nil);
  GlyphImages.GetBitmap(SBtn.Tag, SBtn.Glyph);
end;

procedure TSBDatas.ChangeGlyph(EdtBtn: TFileNameEdit);
begin
  EdtBtn.Glyph.Assign(nil);
  GlyphImages.GetBitmap(EdtBtn.Tag, EdtBtn.Glyph);
end;

procedure TSBDatas.HandleTranslation;
begin
  TaskDlg.Title := rsTimingsFileFormat;
  TaskDlg.Text := rsChooseCorrectTimingsFileFormatWarn;
  TaskDlg.RadioButtons.Items[0].Caption := rsSBVersion13UpFormat+'\n'+SB13TimingsFormat;
  TaskDlg.RadioButtons.Items[1].Caption := rsSBVersion12LowerFormat+'\n'+SB12TimingsFormat;
  TaskDlg.RadioButtons.Items[2].Caption := 'SolveigMM Video Splitter\n'+SMMVideoSplitterTimingsFormat;
  TaskDlg.RadioButtons.Items[0].Default := True;
  TaskDlg.Buttons.Items[0].Caption := rsSBDOK;
  TaskDlg.Buttons.Items[0].ModalResult := mrOk;
  TaskDlg.Buttons.Items[1].Caption := rsSBDCancel;
  TaskDlg.Buttons.Items[1].ModalResult := mrCancel;
  SaveDlg.Filter := rsTextFiles+'|*.txt';
  OpenDlg.Filter := rsTextFiles+'|*.txt|'+rsSBDAllFiles+'|*';
end;

end.

