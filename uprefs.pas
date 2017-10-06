unit uPrefs;
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
  EditBtn, IniPropStorage;

type

  { TSBPrefs }

  TSBPrefs = class(TForm)
    ToolsFolderAddress: TDirectoryEdit;
    ToolsFolderAddressL: TLabel;
    SaveToolsLogs: TCheckBox;
    AutoSaveTSList: TCheckBox;
    FFmpegAddress: TFileNameEdit;
    MkvMergeAddress: TFileNameEdit;
    MkvExtractAddress: TFileNameEdit;
    ToolAddresses: TGroupBox;
    IniProps: TIniPropStorage;
    FFmpegAddressL: TLabel;
    MkvMergeAddressL: TLabel;
    MkvExtractAddressL: TLabel;
    UseInternalSplitter: TCheckBox;
    UseInternalCodecs: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure IniPropsRestoreProperties(Sender: TObject);
  end;

var
  SBPrefs: TSBPrefs;

implementation

{$R *.lfm}

{ TSBPrefs }

procedure TSBPrefs.FormCreate(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  FFmpegAddress.Filter := 'ffmpeg|ffmpeg.exe';
  MkvMergeAddress.Filter := 'mkvmerge|mkvmerge.exe';
  MkvExtractAddress.Filter := 'mkvextract|mkvextract.exe';
  {$ENDIF}
  {$IFDEF UNIX}
  FFmpegAddress.Filter := 'ffmpeg|ffmpeg';
  MkvMergeAddress.Filter := 'mkvmerge|mkvmerge';
  MkvExtractAddress.Filter := 'mkvextract|mkvextract';
  {$ENDIF}
end;

procedure TSBPrefs.IniPropsRestoreProperties(Sender: TObject);
begin
  with ToolsFolderAddress do
  begin
    if Text = EmptyStr then
      Text := ProgramDirectory + 'tools' + PathDelim;
  end;
  {$IFDEF WINDOWS}
  if FFmpegAddress.Text = EmptyStr then
    FFmpegAddress.Text := ToolsFolderAddress.Text + 'win\ffmpeg.exe';
  if MkvMergeAddress.Text = EmptyStr then
    MkvMergeAddress.Text := ToolsFolderAddress.Text + 'win\mkvmerge.exe';
  if MkvExtractAddress.Text = EmptyStr then
    MkvExtractAddress.Text := ToolsFolderAddress.Text + 'win\mkvextract.exe';
  {$ENDIF}
  {$IFDEF UNIX}
  if FFmpegAddress.Text = EmptyStr then
    FFmpegAddress.Text := ToolsFolderAddress.Text + 'lin' +
    {$IFDEF CPU32}
    '32'
    {$ENDIF}
    {$IFDEF CPU64}
    '64'
    {$ENDIF}
    + PathDelim + 'ffmpeg';
  if MkvMergeAddress.Text = EmptyStr then
    MkvMergeAddress.Text := '/usr/bin/mkvmerge';
  if MkvExtractAddress.Text = EmptyStr then
    MkvExtractAddress.Text := '/usr/bin/mkvextract';
  {$ENDIF}
end;

end.

