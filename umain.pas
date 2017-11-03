unit uMain;
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
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  LclIntf, IniPropStorage, ExtCtrls, StdCtrls, Menus, EditBtn, ComCtrls,
  Buttons, uDatas, uPrefs, uAbout, uProbe, uProc, uTimeSlice, ui18nGuide,
  CommonFileUtils, CommonGUIUtils, uTimeCode, uCharEnc, uResourcestrings,
  uSBConst, uTimeSliceEditEx, uListBoxUtils, uTimeCodeFormatDialogEx,
  CommonNumeralUtils, uNumEditFloat, LCLTranslator, ActnList, PopupNotifier,
  CommonStrUtils;

type

  { TSBMain }

  TSBMain = class(TForm)
    DoSplit: TButton;
    NewTimingMI: TMenuItem;
    EditTimingMI: TMenuItem;
    DeleteTimingsMI: TMenuItem;
    HelpNotifier: TPopupNotifier;
    SelectTimingsMI: TMenuItem;
    AddOffsetToTimingsMI: TMenuItem;
    SaveTimingsMI: TMenuItem;
    OpenTimingsMI: TMenuItem;
    TimingsListMenu: TPopupMenu;
    ResetFormAct: TAction;
    AddOffsetToTimingsAct: TAction;
    SelectTimingsAct: TAction;
    DeleteTimingsAct: TAction;
    EditTimingAct: TAction;
    NewTimingAct: TAction;
    SaveTimingsAct: TAction;
    OpenTimingsAct: TAction;
    SaveSubtitleAct: TAction;
    OpenSubtitleAct: TAction;
    SBActions: TActionList;
    SubtitleFile: TFileNameEdit;
    SubtitleFileL: TLabel;
    TimingsListL: TLabel;
    TimingsList: TListBox;
    Logo: TImage;
    IniProps: TIniPropStorage;
    Description: TLabel;
    MenuSBPrefs: TMenuItem;
    MenuSBAbout: TMenuItem;
    DoSplitPanel: TPanel;
    TimeSlicesListActions: TPanel;
    NewTiming: TSpeedButton;
    EditTiming: TSpeedButton;
    DeleteTimings: TSpeedButton;
    ResetForm: TSpeedButton;
    SaveTimingsFile: TSpeedButton;
    OpenTimingsFile: TSpeedButton;
    TopMenu: TMainMenu;
    Header: TPanel;
    Progress: TProgressBar;
    MenuSBLang: TMenuItem;
    MenuSBLangTranslateIt: TMenuItem;
    MenuSBLangDownloadMore: TMenuItem;
    MenuSBLangSep: TMenuItem;
    MenuSBGuide: TMenuItem;
    AddOffsetToTimings: TSpeedButton;
    StatsBar: TStatusBar;
    procedure HelpNotifierClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure IniPropsRestoringProperties(Sender: TObject);
    procedure IniPropsRestoreProperties(Sender: TObject);
    procedure MenuSBLangDownloadMoreClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure MenuSBAboutClick(Sender: TObject);
    procedure MenuSBGuideClick(Sender: TObject);
    procedure MenuSBLangTranslateItClick(Sender: TObject);
    procedure OpenSubtitleActExecute(Sender: TObject); 
    procedure SaveSubtitleActExecute(Sender: TObject); 
    procedure OpenTimingsActExecute(Sender: TObject);
    procedure SaveTimingsActExecute(Sender: TObject);
    procedure NewTimingActExecute(Sender: TObject);
    procedure EditTimingActExecute(Sender: TObject);
    procedure SBActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure SelectTimingsActExecute(Sender: TObject);
    procedure DeleteTimingsActExecute(Sender: TObject);
    procedure AddOffsetToTimingsActExecute(Sender: TObject);
    procedure ResetFormActExecute(Sender: TObject);
    procedure MenuSBPrefsClick(Sender: TObject);
    procedure SubtitleFileAcceptFileName(Sender: TObject; var Value: String);
    procedure TimingsListDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StatsBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
  private
    FStatusMsg: String;
    FStatusTextStyle: TTextStyle;
    FLangID: String;
    FLangsDir: String;
    FFormatSettings: TTimeCodeFormatSettings;
    FInputsFormatDefined: Boolean;
    FProbeInfo: TSubzBorProbeInfo;
    FProbeResult: TSubzBorProbeResult;
    FProbeThread: TSubzBorProbeThread;
    FProcInfo: TSubzBorProcInfo;
    FProcResult: TSubzBorProcResult;
    FProcThread: TSubzBorProcThread;
    FUserNotified: Boolean;
    procedure CorrectFormSize;
    procedure Status(const MsgType, Msg: String; Bar: boolean = False;
      BarPos: Word = 0; HideBarAfter: Word = 0);
    procedure SetGlyphs;
    procedure DefineUserInputsFormat;
    function HasInternalCodec(const Fmt: String): Boolean;
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
    property UserNotified: Boolean read FUserNotified write FUserNotified;
  end;

var
  SBMain: TSBMain;

implementation

{$R *.lfm}
{$R dummyvidres.res}

{ TSBMain }

procedure TSBMain.FormCreate(Sender: TObject);
begin
  FUserNotified := False;
  FStatusMsg := EmptyStr;
  FStatusTextStyle := Default(TTextStyle);
  FStatusTextStyle.Layout := tlCenter;
  FStatusTextStyle.EndEllipsis := True;
  FInputsFormatDefined := False;
  FFormatSettings := DefaultTimeCodeFormatSettings;
end;

procedure TSBMain.SetGlyphs;
begin
  SBDatas.ChangeGlyph(NewTiming);
  SBDatas.ChangeGlyph(EditTiming);
  SBDatas.ChangeGlyph(DeleteTimings);
  SBDatas.ChangeGlyph(ResetForm);
  SBDatas.ChangeGlyph(SaveTimingsFile);
  SBDatas.ChangeGlyph(OpenTimingsFile);
  SBDatas.ChangeGlyph(SubtitleFile);
  SBDatas.ChangeGlyph(AddOffsetToTimings);
  SBDatas.ChangeGlyph(HelpNotifier);
end;

procedure TSBMain.CorrectFormSize;
var
  MinW: Integer;
begin
  MinW := Canvas.TextWidth(rsHint +': ' +rsExportingImgBased);
  StatsBar.Panels[0].Width := MinW;
  Constraints.MinWidth := MinW;
  TimingsList.ScrollWidth := 0;
end;

procedure TSBMain.FormShow(Sender: TObject);
begin
  SetGlyphs;
  CorrectFormSize;
  ListTranslations;
  HandleTranslation(LangID);
  if not UserNotified then
    HelpNotifier.ShowAtPos(Left+Width-(Width div 4),Top+Header.Top+Header.Height);
end; 

procedure TSBMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  SubtitleFile.Text := FileNames[0];
  SetInitialDirs(ExtractFilePath(FileNames[0]));
end;

procedure TSBMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not SaveSubtitleAct.Enabled then
    CanClose := ShowWarnYN(rsCloseWhileProcWarn, rsWarn)
  else
    CanClose := True;
end;

procedure TSBMain.IniPropsRestoringProperties(Sender: TObject);
begin
  SessionProperties := SessionProperties+';LangID;UserNotified';
end; 

procedure TSBMain.IniPropsRestoreProperties(Sender: TObject);
begin
  SetDefaultLangID;
end;

procedure TSBMain.HelpNotifierClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FUserNotified := True;
end;

procedure TSBMain.MenuSBLangDownloadMoreClick(Sender: TObject);
begin
  OpenUrl(urlSBLanguages);
end;

procedure TSBMain.SubtitleFileAcceptFileName(Sender: TObject;
  var Value: String);
begin
  SetInitialDirs(ExtractFilePath(Value));
end;

procedure TSBMain.TimingsListDblClick(Sender: TObject);
begin
  EditTimingActExecute(Sender);
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
      FFormatSettings := fd.Value.TimeCodeFormat;
  finally
    fd.Free;
  end;
  FInputsFormatDefined := True;
end;

function TSBMain.HasInternalCodec(const Fmt: String): Boolean;
begin
  if FormatsWithInternalCodecs.Contains(Fmt) then
    Result := True
  else
    Result := False;
end;

procedure TSBMain.SaveDummyVidResToFile(const Dir: String);
var
  pic: TPicture;
  f: String;
begin
  f := IncludeTrailingPathDelimiter(Dir) +DummyPicFileName;
  pic := TPicture.Create;
  try
    pic.LoadFromResourceName(HInstance, DummyPicResName);
    pic.SaveToFile(f);
  finally
    pic.Free;
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
      for i := sl.Count-1 downto 0 do
        if IsEmptyStr(sl[i]) then
          sl.Delete(i);
      ts.Initialize(FFormatSettings, DefaultTimeSliceSep);
      tsl.Count := sl.Count;
      for i := 0 to sl.Count-1 do
      begin
        ts.ValueAsString := sl[i];
        tsl.Values[i] := ts;
      end;
      if not tsl.Incremental then
        raise Exception.Create(rsFatal);
    finally
      TimingsList.Items.Text := tsl.ExtendedValue;
      sl.Free;
    end;
  except
    ShowError(rsTimeSliceFileNotRead, rsFatal);
  end;
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
    TimeSlices := TimingsList.Items.Text;
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
    InputFile := FProbeInfo.InputFile;
    InputFileExtension := FProbeResult.InputFileExtension;
    if FProbeResult.InputFileFormat.Equals(DVDSubtitleFormat) then
    begin
      InputFile := GenFileName(InputFile, EmptyStr, extIdx);
      InputFileExtension := extIdx;
    end;
    UseInternalCodecs := SBPrefs.UseInternalCodecs.State = cbChecked;
    if UseInternalCodecs and not HasInternalCodec(FProbeResult.InputFileFormat) then
      UseInternalCodecs := False;
    InputFileIsText := FProbeResult.InputFileIsText;
    if not InputFileIsText then
      Status(rsHint, rsExportingImgBased, True, 40)
    else
    begin
      Status(rsHint, rsChoosingCharacterEncoding, True, 60);
      SBCharEnc.ShowInternalEncodings := UseInternalCodecs;
      SBCharEnc.ShowModal;
      TextEncoding := SBCharEnc.EncodingName.Text;
      Status(rsHint, rsExporting, True, 70);
    end;
    if FProcInfo.TextEncoding.Contains(encUTF16)
    and FProcInfo.InputFileExtension.Equals(extSrt)
    and not UseInternalCodecs then
      TextEncoding := encUTF8;
    SubzBorPath := FProbeResult.SubzBorPath;
    DummyVidExists := FProbeResult.DummyVidExists;
    ToolsFolder := FProbeInfo.ToolsFolder;
    FFmpeg := FProbeInfo.FFmpeg;
    MkvMerge := FProbeInfo.MkvMerge;
    MkvExtract := FProbeInfo.MkvExtract;
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

procedure TSBMain.OpenSubtitleActExecute(Sender: TObject);
begin
  SubtitleFile.RunDialog;
end;

procedure TSBMain.SaveSubtitleActExecute(Sender: TObject);
begin
  StartProbe;
end;

procedure TSBMain.OpenTimingsActExecute(Sender: TObject);
begin
  with SBDatas.OpenDlg do
  begin
    if Not Execute Then Exit;
    LoadTimeSlicesFromFile(FileName);
  end;
end;

procedure TSBMain.SaveTimingsActExecute(Sender: TObject);
begin
  if TimingsList.Items.Count < 1 then Exit;
  with SBDatas.SaveDlg do
  begin
    if SubtitleFile.Text <> EmptyStr then
      FileName := GenFileName(SubtitleFile.Text, wCuts, extText, False);
    if not Execute then Exit;
    if CompareFileExt(FileName, extText) <> 0 then
      FileName := FileName +extText;
    try
      TimingsList.Items.SaveToFile(FileName);
    except
      on E: Exception do
        ShowError(E.Message +LineEnding +rsTimeSliceFileNotSaved, rsFatal);
    end;
  end;
end;

procedure TSBMain.NewTimingActExecute(Sender: TObject);
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
      TimingsList.Items.Add(ts.ValueAsStringEx);
    end;
  finally
    tse.Free;
  end;
end;

procedure TSBMain.EditTimingActExecute(Sender: TObject);
var
  tse: TTimeSliceEditEx;
begin
  if TimingsList.ItemIndex < 0 then Exit;
  tse := TTimeSliceEditEx.Create(Self);
  try
    tse.Value := TimingsList.Items[TimingsList.ItemIndex];
    tse.ShowModal;
    if tse.ModalResult = mrOk then
      TimingsList.Items[TimingsList.ItemIndex] := tse.Value;
  finally
    tse.Free;
  end;
end;

procedure TSBMain.SBActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  EditTimingAct.Enabled := (Self.ActiveControl = TimingsList)
    and (TimingsList.ItemIndex > -1);
  SelectTimingsAct.Enabled := TimingsList.Count > 0;
  DeleteTimingsAct.Enabled := (Self.ActiveControl = TimingsList)
    and (TimingsList.SelCount > 0);
  AddOffsetToTimingsAct.Enabled := (Self.ActiveControl = TimingsList)
    and (TimingsList.SelCount > 0);
  SaveTimingsAct.Enabled := TimingsList.Count > 0;
end;

procedure TSBMain.SelectTimingsActExecute(Sender: TObject);
begin
  TimingsList.SelectAll;
end;

procedure TSBMain.DeleteTimingsActExecute(Sender: TObject);
begin
  ListBoxUtils.DeleteItems(TimingsList);
end;

procedure TSBMain.AddOffsetToTimingsActExecute(Sender: TObject);
var
  ne: TNumEditFloat;
  ts: TTimeSlice;
  i: Integer;
begin
  ne := TNumEditFloat.Create(Self);
  try
    ne.DecimalPlaces := 3;
    ne.HeaderText := rsApplyGlobalOffset;
    ne.ShowModal;
    if ne.ModalResult = mrOK then
      for i := 0 to TimingsList.Count-1 do
      begin
        if TimingsList.Selected[i] then
        begin
          ts.ValueAsStringEx := TimingsList.Items[i];
          ts.Delay := ts.Delay+ne.Value;
          TimingsList.Items[i] := ts.ValueAsStringEx;
        end;
      end;
  finally
    ne.Free;
  end;
end;

procedure TSBMain.ResetFormActExecute(Sender: TObject);
begin
  SubtitleFile.Clear;
  TimingsList.Items.Clear;
  FormCreate(Self);
  Status(rsHint, rsReady);
  HandleTranslation(FLangID);
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
  if not SaveSubtitleAct.Enabled then Exit;
  LangID := (Sender as TMenuItem).Caption;
  HandleTranslation(LangID);
end;

procedure TSBMain.ListTranslations;
var
  sl: TStringList;
  s: String;
  i: Word;
  begin
    s := IncludeTrailingPathDelimiter(ProgramDirectory) +'languages';
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

  SubtitleFile.Filter :=
    rsCommonFormats +CommonFilesMask +'|' +rsAllFiles +AllFilesMask;
  SBPrefs.UseInternalCodecs.Hint :=
    rsUseInternalCodecsHint +LineEnding +FormatsWithInternalCodecs;

  bd := Application.Direction(GetDefaultLang);
  TimingsListL.BiDiMode := bd;
  SubtitleFileL.BiDiMode := bd;
  TimingsListMenu.BiDiMode := bd;
  FStatusTextStyle.RightToLeft := bd = BdRightToLeft;
  if FStatusTextStyle.RightToLeft then
    FStatusTextStyle.Alignment := taRightJustify
  else
    FStatusTextStyle.Alignment := taLeftJustify;
  SBPrefs.SaveToolsLogs.BiDiMode := bd;
  SBPrefs.AutoSaveTSList.BiDiMode := bd;
  SBPrefs.UseInternalSplitter.BiDiMode := bd;
  SBPrefs.UseInternalCodecs.BiDiMode := bd;
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
  SBAbout.Description.BiDiMode := bd;
  SBAbout.UsedTools.BiDiMode := bd;
  SBAbout.ContactMe.BiDiMode := bd;

  Status(rsHint, rsReady);
end;

procedure TSBMain.Status(const MsgType, Msg: String; Bar: boolean; BarPos: Word;
  HideBarAfter: Word);
begin
  FStatusMsg := MsgType +': ' +Msg;
  StatsBar.Repaint;
  Progress.Visible := Bar;
  SaveSubtitleAct.Enabled := not Bar;
  Progress.Position := BarPos;
  if Bar and (HideBarAfter > 0) then
  begin
    SBDatas.TheTimer.Interval := HideBarAfter*1000;
    SBDatas.TheTimer.OnTimer := @HideProgress;
    SBDatas.TheTimer.Enabled := True;
  end;
end;

procedure TSBMain.StatsBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
var
  TheRect: TRect;
begin
  if Panel.Index <> 0 then Exit;
  TheRect := Rect;
  TheRect.Inflate(0,0,-16,0);
  StatusBar.Canvas.TextRect(TheRect, 6, 0, FStatusMsg, FStatusTextStyle);
end;

procedure TSBMain.HideProgress(Sender: TObject);
begin
  Progress.Visible := False;
  SaveSubtitleAct.Enabled := True;
  SBDatas.TheTimer.Enabled := False;
  SBDatas.TheTimer.OnTimer := nil;
end;

procedure TSBMain.StepProgress;
begin
  Progress.Position := FProcThread.Progress;
end;

end.

