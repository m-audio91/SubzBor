unit uDatas;
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Buttons, EditBtn,
  Dialogs, ExtCtrls, PopupNotifier, CommonGUIUtils;

type

  { TSBDatas }

  TSBDatas = class(TDataModule)
    GlyphImages192: TImageList;
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
    procedure ChangeGlyph(PN: TPopupNotifier); overload;
    procedure HandleTranslation;
  end;

resourcestring
  rsTextFiles = 'فایل متنی';
  rsSBDAllFiles = 'همه فایل ها';
  rsSBDOK = 'تایید';
  rsSBDCancel = 'صرف نظر';
  rsInputTimingsFormat = 'ساختار کدزمان های ورودی';
  rsChooseCorrectInputTimingsFormatWarn = 'ساختار کدزمان های مدنظر برای ورودی را به درستی مشخص نمایید. این ساختار در زمان چسباندن از حافظه و همچنین در زمان خواندن از فایل استفاده می شود. یعنی اگر کدزمان های خود را از نرم افزار خاصی به داخل سابزبر کپی می کنید این ساختار را باید مطابق آن تعیین کنید.';
  rsThisSettingsWillRemainUntilProgramClose = 'این تنظیمات تا زمان بستن نرم افزار یا کلیک روی دکمه "از نو (ریست)" باقی می ماند';

var
  SBDatas: TSBDatas;

implementation

{$R *.lfm}

{ TSBDatas }

procedure TSBDatas.DataModuleCreate(Sender: TObject);
begin
  PrepareGlyphs(Screen.PixelsPerInch);
  with TaskDlg.Buttons.Add do
    ModalResult := mrOK;
  HandleTranslation;
end;

procedure TSBDatas.PrepareGlyphs(DPI: Word);
begin
  CopyImageList(GlyphImages, GlyphImages192, DPI, 192, clBtnFace);
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

procedure TSBDatas.ChangeGlyph(PN: TPopupNotifier);
begin
  PN.Icon.Bitmap.Assign(nil);
  GlyphImages.GetBitmap(PN.Tag, PN.Icon.Bitmap);
end;

procedure TSBDatas.HandleTranslation;
begin
  TaskDlg.Title := rsInputTimingsFormat;
  TaskDlg.Text := rsChooseCorrectInputTimingsFormatWarn;
  TaskDlg.FooterText := rsThisSettingsWillRemainUntilProgramClose;
  TaskDlg.Buttons.Items[0].Caption := rsSBDOK;
  SaveDlg.Filter := rsTextFiles+'|*.txt';
  OpenDlg.Filter := rsTextFiles+'|*.txt|'+rsSBDAllFiles+'|*';
end;

end.

