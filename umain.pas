unit uMain;
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
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  LclIntf, IniPropStorage, ExtCtrls, StdCtrls, Menus, EditBtn, ComCtrls,
  Buttons, uDatas, uPrefs, uAbout, uProbe, uProc, uTimeSlice, ui18nGuide,
  CommonFileUtils, CommonGUIUtils, uTimeCode, uCharEnc, uResourcestrings,
  uSBConst, uTimeSliceEditEx, uListBoxUtils, uTimeCodeFormatDialogEx,
  CommonNumeralUtils, LCLTranslator;

type

  { TSBMain }

  TSBMain = class(TForm)
    DoSplit: TBitBtn;
    SubtitleFile: TFileNameEdit;
    SubtitleFileL: TLabel;
    StatusMsg: TLabel;
    TimeSlicesListL: TLabel;
    TimeSlicesList: TListBox;
    Logo: TImage;
    IniProps: TIniPropStorage;
    Description: TLabel;
    MenuSBPrefs: TMenuItem;
    MenuSBAbout: TMenuItem;
    StatusBar: TPanel;
    DoSplitPanel: TPanel;
    TimeSlicesListActions: TPanel;
    AddTimeSlice: TSpeedButton;
    EditTimeSlice: TSpeedButton;
    DeleteTimeSlice: TSpeedButton;
    ClearTimeSlices: TSpeedButton;
    SaveTimeSlices: TSpeedButton;
    LoadTimeSlices: TSpeedButton;
    TopMenu: TMainMenu;
    Header: TPanel;
    Progress: TProgressBar;
    MenuSBLang: TMenuItem;
    MenuSBLangTranslateIt: TMenuItem;
    MenuSBLangDownloadMore: TMenuItem;
    MenuSBLangSep: TMenuItem;
    MenuSBGuide: TMenuItem;
    procedure IniPropsRestoringProperties(Sender: TObject);
    procedure IniPropsRestoreProperties(Sender: TObject);
    procedure MenuSBLangDownloadMoreClick(Sender: TObject);
    procedure DoSplitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure EditTimeSliceClick(Sender: TObject);
    procedure DeleteTimeSliceClick(Sender: TObject);
    procedure ClearTimeSlicesClick(Sender: TObject);
    procedure LoadTimeSlicesClick(Sender: TObject);
    procedure MenuSBAboutClick(Sender: TObject);
    procedure MenuSBGuideClick(Sender: TObject);
    procedure MenuSBLangTranslateItClick(Sender: TObject);
    procedure SaveTimeSlicesClick(Sender: TObject);
    procedure MenuSBPrefsClick(Sender: TObject);
    procedure SubtitleFileAcceptFileName(Sender: TObject; var Value: String);
    procedure TimeSlicesListDblClick(Sender: TObject);
    procedure AddTimeSliceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FLangID: String;
    FLangsDir: String;
    FFormatSettings: TTimeCodeFormatSettings;
    FInputsFormatDefined: Boolean;
    FTimecodeHasFrameNo: Boolean;
    FInputFramerate: Integer;
    FProbeInfo: TSubzBorProbeInfo;
    FProbeResult: TSubzBorProbeResult;
    FProbeThread: TSubzBorProbeThread;
    FProcInfo: TSubzBorProcInfo;
    FProcResult: TSubzBorProcResult;
    FProcThread: TSubzBorProcThread;
    procedure CurrectFormSize;
    procedure Status(const MsgType, Msg: String; Bar: boolean = False;
      BarPos: Word = 0; HideBarAfter: Word = 0);
    procedure SetGlyphs;
    procedure DefineUserInputsFormat;
    procedure ConvertFrameNoToMillisec(var TS: TTimeSlice);
    procedure SaveDummyVidResToFile(const Dir: String);
    procedure LoadTimeSlicesFromFile(const F: String);
    procedure StartProbe;
    procedure StartProcess;
    procedure SetInitialDirs(const Dir: String);
    procedure OnLangSelect(Sender: TObject);
    procedure ListTranslations;
    procedure SetDefaultLangID;
    procedure HandleTranslation(const LID: String);
  public
    procedure ProbeDone;
    procedure ProcDone;
    procedure StepProgress;
    procedure HideProgress(Sender: TObject);
  published
    property LangID: String read FLangID write FLangID;
  end;

var
  SBMain: TSBMain;

implementation

{$R *.lfm}
{$R dummyvidres.rc}

{ TSBMain }

procedure TSBMain.FormCreate(Sender: TObject);
begin
  FInputsFormatDefined := False;
  FFormatSettings := DefaultTimeCodeFormatSettings;
  FTimecodeHasFrameNo := False;
end;

procedure TSBMain.SetGlyphs;
begin
  SBDatas.ChangeGlyph(AddTimeSlice);
  SBDatas.ChangeGlyph(EditTimeSlice);
  SBDatas.ChangeGlyph(DeleteTimeSlice);
  SBDatas.ChangeGlyph(ClearTimeSlices);
  SBDatas.ChangeGlyph(SaveTimeSlices);
  SBDatas.ChangeGlyph(LoadTimeSlices);
  SBDatas.ChangeGlyph(SubtitleFile);
end;

procedure TSBMain.DefineUserInputsFormat;
var
  fd: TTimeCodeFormatDialogEx;
begin
  SBDatas.TaskDlg.Execute(Self.Handle);
  fd := TTimeCodeFormatDialogEx.Create(Self);
  try
    fd.Value.TimeCodeFormat := FFormatSettings;
    fd.ShowModal;
    if fd.ModalResult = mrOK then
    begin
      FFormatSettings := fd.Value.TimeCodeFormat;
      FTimecodeHasFrameNo := fd.IsMillisecondAFrameNo;
      FInputFramerate := fd.Framerate;
    end;
  finally
    fd.Free;
  end;
  FInputsFormatDefined := True;
end;

procedure TSBMain.ConvertFrameNoToMillisec(var TS: TTimeSlice);
  function ApplyConversion(const fps,val: Integer): Integer;
  begin
    Result := val;
    ForceInRange(Result,0,fps);
    Result := Round(1000/fps)*Result;
    ForceInRange(Result,0,999);
  end;

var
  a: TBasicTimeCodeArray;
begin
  a := TS.Value.StartPos.ValueAsArray;
  a[3] := ApplyConversion(FInputFramerate,a[3]);
  TS.Value.StartPos.ValueAsArray := a;
  a := TS.Value.EndPos.ValueAsArray;
  a[3] := ApplyConversion(FInputFramerate,a[3]);
  TS.Value.EndPos.ValueAsArray := a;
end;

procedure TSBMain.SaveDummyVidResToFile(const Dir: String);
var
  pic: TPicture;
  f: String;
begin
  f := IncludeTrailingPathDelimiter(Dir) + DummyVidResName;
  pic := TPicture.Create;
  try
    pic.LoadFromResourceName(HInstance, 'dummyvidres');
    pic.SaveToFile(f);
  finally
    pic.Free;
  end;
end;

procedure TSBMain.CurrectFormSize;
var
  MinW: Integer;
begin
  MinW := Canvas.TextWidth(rsHint + ': ' + rsExportingImgBased);
  MinW := MinW + StatusMsg.BorderSpacing.Left + StatusMsg.BorderSpacing.Right+4;
  Constraints.MinWidth := MinW;
  TimeSlicesList.ScrollWidth := 0;
end;

procedure TSBMain.FormShow(Sender: TObject);
begin
  SetGlyphs;
  CurrectFormSize;
  ListTranslations;
  HandleTranslation(LangID);
end;

procedure TSBMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not DoSplit.Enabled then
    CanClose := ShowWarnYN(rsCloseWhileProcWarn, rsWarn)
  else
    CanClose := True;
end;

procedure TSBMain.IniPropsRestoringProperties(Sender: TObject);
begin
  SessionProperties := SessionProperties+';LangID';
end;

procedure TSBMain.IniPropsRestoreProperties(Sender: TObject);
begin
  SetDefaultLangID;
end; 

procedure TSBMain.MenuSBLangDownloadMoreClick(Sender: TObject);
begin
  OpenUrl(urlSBLanguages);
end;

procedure TSBMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  if CompareFileExt(FileNames[0], extText) = 0 then
    LoadTimeSlicesFromFile(FileNames[0])
  else
    SubtitleFile.Text := FileNames[0];
  SetInitialDirs(ExtractFilePath(FileNames[0]));
end;

procedure TSBMain.SubtitleFileAcceptFileName(Sender: TObject;
  var Value: String);
begin
  SetInitialDirs(ExtractFilePath(Value));
end;

procedure TSBMain.TimeSlicesListDblClick(Sender: TObject);
begin
  EditTimeSliceClick(EditTimeSlice);
end;

procedure TSBMain.AddTimeSliceClick(Sender: TObject);
var
  tse: TTimeSliceEditEx;
  ts: TTimeSlice;
begin
  if not FInputsFormatDefined then
    DefineUserInputsFormat;
  tse := TTimeSliceEditEx.Create(Self);
  try
    tse.PasteFormat := FFormatSettings;
    tse.ShowModal;
    if tse.ModalResult = mrOk then
    begin
      ts.ValueAsStringEx := tse.Value;
      if FTimecodeHasFrameNo then
        ConvertFrameNoToMillisec(ts);
      TimeSlicesList.Items.Add(ts.ValueAsStringEx);
    end;
  finally
    tse.Free;
  end;
end;

procedure TSBMain.EditTimeSliceClick(Sender: TObject);
var
  tse: TTimeSliceEditEx;
begin
  tse := TTimeSliceEditEx.Create(Self);
  try
    if TimeSlicesList.ItemIndex >= 0 then
    begin
      tse.Value := TimeSlicesList.Items[TimeSlicesList.ItemIndex];
      tse.ShowModal;
      if tse.ModalResult = mrOk then
        TimeSlicesList.Items[TimeSlicesList.ItemIndex] := tse.Value;
    end;
  finally
    tse.Free;
  end;
end;

procedure TSBMain.DeleteTimeSliceClick(Sender: TObject);
begin
  ListBoxUtils.DeleteItems(TimeSlicesList);
end;

procedure TSBMain.ClearTimeSlicesClick(Sender: TObject);
begin
  TimeSlicesList.Items.Clear;
end;

procedure TSBMain.LoadTimeSlicesClick(Sender: TObject);
begin
  with SBDatas.OpenDlg do
  begin
    if Not Execute Then Exit;
    LoadTimeSlicesFromFile(FileName);
  end;
end;

procedure TSBMain.LoadTimeSlicesFromFile(const F: String);
var
  tsl: TTimeSliceList;
  ts: TTimeSlice;
  sl: TStringList;
  i: Integer;
begin
  if not FInputsFormatDefined then
    DefineUserInputsFormat;

  sl := TStringList.Create;
  try
    try
      tsl.LoadFromFileEx(F);
      if tsl.Incremental then Exit;

      sl.LoadFromFile(F);
      for i := 0 to sl.Count-1 do
      begin
        ts.Initialize(FFormatSettings.MillisecondPrecision, FFormatSettings.MajorSep,
          FFormatSettings.MinorSep, DefaultTimeSliceSep);
        ts.ValueAsString := sl[i];
        ts.Initialize;
        if FTimecodeHasFrameNo then
          ConvertFrameNoToMillisec(ts);
        sl[i] := ts.ValueAsStringEx;
      end;
      tsl.ExtendedValue := sl.Text;
      if not tsl.Incremental then
        raise Exception.Create(rsFatal);
    finally
      TimeSlicesList.Items.Text := tsl.ExtendedValue;
      sl.Free;
    end;
  except
    ShowError(rsTimeSliceFileNotRead, rsFatal);
  end;
end;

procedure TSBMain.SaveTimeSlicesClick(Sender: TObject);
begin
  if TimeSlicesList.Items.Count < 1 then Exit;
  with SBDatas.SaveDlg do
  begin
    if SubtitleFile.Text <> EmptyStr then
      FileName := GenFileName(SubtitleFile.Text, wCuts, extText, False);
    if not Execute then Exit;
    if CompareFileExt(FileName, extText) <> 0 then
      FileName := FileName + extText;
    try
      TimeSlicesList.Items.SaveToFile(FileName);
    except
      on E: Exception do
        ShowError(E.Message + LineEnding + rsTimeSliceFileNotSaved, rsFatal);
    end;
  end;
end;

procedure TSBMain.DoSplitClick(Sender: TObject);
begin
  StartProbe;
end;

procedure TSBMain.StartProbe;
begin
  Status(rsHint, rsProbing, True, 20);
  with FProbeInfo do
  begin
    ToolsFolder := SBPrefs.ToolsFolderAddress.Text;
    FFmpeg := SBPrefs.FFmpegAddress.Text;
    MkvMerge := SBPrefs.MkvMergeAddress.Text;
    MkvExtract := SBPrefs.MkvExtractAddress.Text;
    InputFile := SubtitleFile.Text;
    if LowerCase(ExtractFileExt(InputFile)).Equals(extSub) then
      InputFile := GenFileName(InputFile, '', extIdx);
    TimeSlices := TimeSlicesList.Items.Text;
  end;
  FProbeThread := TSubzBorProbeThread.Create(FProbeInfo);
  FProbeThread.OnProbeDone := @ProbeDone;
  FProbeThread.Start;
end;

procedure TSBMain.ProbeDone;
begin
  FProbeResult := FProbeThread.ProbeResult;
  try
    if not FProbeResult.ToolsFolderValid then
      raise Exception.Create(rsNotToolsFolder);
    if not FProbeResult.FFmpegValid then
      raise Exception.Create(rsNoFFmpeg);
    if not FProbeResult.InputFileValid then
      raise Exception.Create(rsNoValidSub);
    if not FProbeResult.TimeSlicesValid then
      raise Exception.Create(rsInvalidTimeSlices);
    if not FProbeResult.InputFileIsText then
    begin
      if not FProbeResult.DummyVidExists then
        SaveDummyVidResToFile(FProbeInfo.ToolsFolder);
      if not FProbeResult.MkvMergeValid then
        raise Exception.Create(rsNoMkvMerge);
      if not FProbeResult.MkvExtractValid then
        raise Exception.Create(rsNoMkvExtract);
    end;
  except
    on E: Exception do
    begin
      ShowError(E.Message, rsFatal);
      Status(rsFatal, rsCanceled);
      Exit;
    end;
  end;
  StartProcess;
end;

procedure TSBMain.StartProcess;
begin
  with FProcInfo do
  begin
    InputFileIsText := FProbeResult.InputFileIsText;
    if not InputFileIsText then
      Status(rsHint, rsExportingImgBased, True, 40)
    else
    begin
      Status(rsHint, rsChoosingCharacterEncoding, True, 60);
      SBCharEnc.ShowModal;
      TextEncoding := SBCharEnc.EncodingName.Text;
      Status(rsHint, rsExporting, True, 70);
    end;
    SubzBorPath := FProbeResult.SubzBorPath;
    DummyVidExists := FProbeResult.DummyVidExists;
    ToolsFolder := FProbeInfo.ToolsFolder;
    FFmpeg := FProbeInfo.FFmpeg;
    MkvMerge := FProbeInfo.MkvMerge;
    MkvExtract := FProbeInfo.MkvExtract;
    InputFile := FProbeInfo.InputFile;
    TimeSlices := FProbeInfo.TimeSlices;
    SaveTimeSlices := SBPrefs.AutoSaveTSList.State = cbChecked;
    NeedReport := SBPrefs.SaveToolsLogs.State = cbChecked;
    UseInternalSplitter := SBPrefs.UseInternalSplitter.State = cbChecked;
  end;
  FProcThread := TSubzBorProcThread.Create(FProcInfo);
  FProcThread.OnProgress := @StepProgress;
  FProcThread.OnProcDone := @ProcDone;
  FProcThread.Start;
end;

procedure TSBMain.ProcDone;
begin
  FProcResult := FProcThread.ProcResult;
  if FProcResult.LastError <> EmptyStr then
  begin
    ShowError(FProcResult.LastError, rsFatal);
    Status(rsFatal, rsExportingFailed);
    Exit;
  end;
  Status(rsHint, rsExportingDone, True, 100, 1);
end;

procedure TSBMain.MenuSBPrefsClick(Sender: TObject);
begin
  SBPrefs.ShowModal;
end;

procedure TSBMain.MenuSBAboutClick(Sender: TObject);
begin
  SBAbout.ShowModal;
end; 

procedure TSBMain.MenuSBGuideClick(Sender: TObject);
begin
  OpenURL(urlGuide);
end;

procedure TSBMain.MenuSBLangTranslateItClick(Sender: TObject);
begin
  SBi18nGuide.ShowModal;
end;

procedure TSBMain.SetInitialDirs(const Dir: String);
begin
  SubtitleFile.InitialDir := Dir;
  SBDatas.SaveDlg.FileName := EmptyStr;
  SBDatas.SaveDlg.InitialDir := Dir;
  SBDatas.OpenDlg.FileName := EmptyStr;
  SBDatas.OpenDlg.InitialDir := Dir;
end;

procedure TSBMain.OnLangSelect(Sender: TObject);
begin
  LangID := (Sender as TMenuItem).Caption;
  HandleTranslation(LangID);
end;

procedure TSBMain.ListTranslations;
var
  sl: TStringList;
  s: String;
  i: Word;
  begin
    s := ExcludeTrailingPathDelimiter(ProgramDirectory)+PathDelim+'languages';
    FLangsDir := s;
    sl := TStringList.Create;
    try
      FindAllFiles(sl, s, '*.po', False);
      if sl.Count > 0 then
        for i := 0 to sl.Count-1 do
        begin
          s := sl[i].Substring(sl[i].LastIndexOf(PathDelim)+1);
          s := s.Substring(s.IndexOf('.')+1);
          s := s.Substring(0, s.IndexOf('.'));
          if s = EmptyStr then Continue;
          MenuSBLang.Add(
            NewItem(s, 0, False, True, @OnLangSelect, 0, 'MenuLangID'+s));
          MenuSBLang.Items[MenuSBLang.Count-1].RadioItem := True;
        end;
    finally
      sl.Free;
    end;
end;

procedure TSBMain.SetDefaultLangID;
begin
  if LangID = EmptyStr then
    LangID := 'en_US';
end;

procedure TSBMain.HandleTranslation(const LID: String);
var
  bd: TBiDiMode;
  mi: TMenuItem;
begin
  SetDefaultLang(LID, FLangsDir);
  SBDatas.HandleTranslation;

  SubtitleFile.Filter := rsCommonFormats+'|*.srt;*.ass;*.ssa;*.idx;*.sup|'
    +rsAllFiles+'|*';

  bd := Application.Direction(GetDefaultLang);
  TimeSlicesListL.BiDiMode := bd;
  SubtitleFileL.BiDiMode := bd;
  StatusMsg.BiDiMode := bd;
  SBPrefs.SaveToolsLogs.BiDiMode := bd;
  SBPrefs.AutoSaveTSList.BiDiMode := bd;
  SBPrefs.UseInternalSplitter.BiDiMode := bd;
  if bd = bdRightToLeft then
    SBDatas.TaskDlg.Flags := SBDatas.TaskDlg.Flags+[tfRtlLayout]
  else
    SBDatas.TaskDlg.Flags := SBDatas.TaskDlg.Flags-[tfRtlLayout];
  SBCharEnc.EncodingNameL.BiDiMode := bd;
  SBCharEnc.FaSubripAdL.BiDiMode := bd;
  SBi18nGuide.i18nInstructionL.BiDiMode := bd;
  SBi18nGuide.i18nTranslateAllLinesL.BiDiMode := bd;
  SBi18nGuide.UpdateExistingPoL.BiDiMode := bd;
  mi := MenuSBLang.Find(LID);
  if Assigned(mi) then
    mi.Checked := True;
  SBAbout.UsedTools.BiDiMode := bd;
  SBAbout.ContactMe.BiDiMode := bd;

  Status(rsHint, rsReady);
end;

procedure TSBMain.Status(const MsgType, Msg: String; Bar: boolean; BarPos: Word;
  HideBarAfter: Word);
begin
  StatusMsg.Caption := MsgType + ': ' + Msg;
  StatusMsg.Hint := MsgType + ': ' + Msg;
  Progress.Visible := Bar;
  DoSplit.Enabled := not Bar;
  Progress.Position := BarPos;
  if Bar and (HideBarAfter > 0) then
  begin
    SBDatas.TheTimer.Interval := HideBarAfter * 1000;
    SBDatas.TheTimer.OnTimer := @HideProgress;
    SBDatas.TheTimer.Enabled := True;
  end;
end;

procedure TSBMain.HideProgress(Sender: TObject);
begin
  Progress.Visible := False;
  DoSplit.Enabled := True;
  SBDatas.TheTimer.Enabled := False;
  SBDatas.TheTimer.OnTimer := nil;
end;

procedure TSBMain.StepProgress;
begin
  Progress.Position := FProcThread.Progress;
end;

end.

