unit uProc;
{ this file is part of SubzBor.
  SubzBor is a Free subtitle cutting tool with the help of FFmpeg, MKVToolNix
  and it's own set of internal codecs.

  Copyright (C) 2018 Mohammadreza Bahrami m.audio91@gmail.com

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
  Classes, SysUtils, FileUtil, Process, UTF8Process, LazUTF8, LConvEncoding,
  CommonStrUtils, CommonFileUtils, {$IFDEF WINDOWS}windirs,{$ENDIF} uTimeSlice,
  uResourcestrings, uSBConst, uSubripFile, uSubStationAlphaFile;

type

  { TSubzBorProcInfo }

  TSubzBorProcInfo = record
    SubzBorPath,
    ToolsFolder,
    FFmpeg,
    MkvMerge,
    MkvExtract,
    InputFile,
    InputFileExtension,
    TextEncoding,
    TimeSlices: String;
    InputFileIsText,
    SaveTimeSlices,
    DummyVidExists,
    NeedReport,
    UseInternalSplitter,
    UseInternalCodecs: Boolean;
  end;

  { TSubzBorProcResult }

  TSubzBorProcResult = record
    LastError: String;
  end;

  { TSubzBorProcThread }

  TSubzBorProcThread = class(TThread)
  private
    FTempsDir: String;
    FReportsDir: String;
    FOutputDir: String;
    FOutputFile: String;
    FProgress: Word;
    FProcInfo: TSubzBorProcInfo;
    FTimeSlices: TTimeSliceList;
    FOnProcDone: TThreadMethod;
    FProcResult: TSubzBorProcResult;
    FOnProgress: TThreadMethod;
    procedure StepProgress(Perc: Word);
    procedure RunCmd(const Exec, Cmd: String);
    procedure SubzBorDirectProcessSub;
    procedure SubzBorDirectSplitSub(const Sub: String);
    procedure SubzBorSplitSub;
    procedure FFmpegSplitSub;
    procedure FFmpegExportSub(const Sub: String);
    procedure FFmpegDummyVid;
    procedure MkvMergeSplitSub;
    procedure MkvMergeAppendSub(const Files: TStringArray);
    procedure MkvExtractSub;
    procedure ExportTimeSlices;
    procedure ClearTemps;
    procedure GatherReports;
  protected
    procedure Execute; override;
  public
    property Progress: Word read FProgress;
    property OnProcDone: TThreadMethod read FOnProcDone write FOnProcDone;
    property OnProgress: TThreadMethod read FOnProgress write FOnProgress;
    property ProcResult: TSubzBorProcResult read FProcResult;
    constructor Create(const ProcInfo: TSubzBorProcInfo);
    destructor Destroy; override;
  end;

implementation

const
  FFmpegSplitCmd = '-hide_banner %report% %delay% -copyts -start_at_zero -sub_charenc %cp% -i %i% -map 0:s:0 -ss %splitstart% -to %splitend% %offset% -y %o%';
  FFmpegExportCmd = '-hide_banner %report% -i %i% -map 0:s:0 -y %o%';
  FFmpegStraightExportCmd = '-hide_banner %report% -sub_charenc %cp% -i %i% -map 0:s:0 -y %o%';
  FFmpegDummyVidCmd = '-hide_banner %report% -framerate 60 -loop true -i %i% -t 4:00:00 -pix_fmt bgr24 -c:v zlib -y %o%';
  MkvMergeSplitCmd  = '%report% --output %o% --language 0:und ( %i% ) --language 0:und --sync 0:%delay% ( %i% ) --split parts:%timeslice% --track-order 0:0,1:0';
  MkvMergeAppendCmd = '%report% --output %o% --language 0:und --default-track 0:yes --language 1:und --default-track 1:yes %i% --track-order 0:0,0:1';
  MkvExtractSubCmd = '%report% tracks %i% %o%';

{ TSubzBorProcThread }

procedure TSubzBorProcThread.StepProgress(Perc: Word);
begin
  FProgress := Perc;
  if Assigned(OnProgress) then
    Synchronize(OnProgress);
end;

procedure TSubzBorProcThread.RunCmd(const Exec, Cmd: String);
var
  Proc: TProcessUTF8;
  i: Integer;
begin
  Proc := TProcessUTF8.Create(nil);
  try
    with Proc do
    begin
      Executable := Exec;
      Options := Options+[poNoConsole];
      with Parameters do
      begin
        StrictDelimiter := True;
        Delimiter := ' ';
        DelimitedText := NoChainedSpaces(Cmd);
        for i := 0 to Count-1 do
          Strings[i] := DeEscape(Strings[i]);
      end;
      Execute;
      while Running do
        Sleep(150);
    end;
  finally
    Proc.Free;
  end;
end;

procedure TSubzBorProcThread.SubzBorDirectProcessSub;
const
  UTFBOMs: array[0..4] of String = (UTF8BOM,UTF16BEBOM,UTF16LEBOM,UTF32BEBOM,
    UTF32LEBOM);
var
  bs: TBytesStream;
  sl: TStringList;
  Enc: TEncoding;
  s,bom: String;
begin
  Enc := Default(TEncoding);
  bs := TBytesStream.Create;
  sl := TStringList.Create;
  try
    bs.LoadFromFile(FProcInfo.InputFile);
    bs.Position := 0;
    case FProcInfo.TextEncoding of
    encUTF16:
      s := UTF16ToUTF8(PWideChar(bs.Bytes), bs.Size div SizeOf(WideChar));
    encUTF8: begin
      sl.LoadFromStream(bs, Enc.UTF8);
      s := sl.Text;
      end;
    else
      s := ConvertEncoding(PChar(bs.Bytes), FProcInfo.TextEncoding, EncodingUTF8);
    end;
    for bom in UTFBOMs do
      DeleteAllOccurrences(bom, s);
  finally
    bs.Free;
    sl.Free;
  end;
  SubzBorDirectSplitSub(s);
end;

procedure TSubzBorProcThread.SubzBorDirectSplitSub(const Sub: String);
var
  ssa: TSubStationAlphaFile;
  Subrip: TSubripFile;
  Enc: TEncoding;
begin
  FOutputFile := GenFileName(FProcInfo.InputFile, wSubzBor,
    FProcInfo.InputFileExtension, True, FOutputDir);
  ssa := TSubStationAlphaFile.Create;
  Subrip := TSubripFile.Create;
  Enc := Default(TEncoding);
  try
    if FProcInfo.InputFileExtension.Equals(extAss)
    or FProcInfo.InputFileExtension.Equals(extSsa) then
    begin
      ssa.LoadFromString(Sub);
      ssa.Events.Value := ssa.MakeNewFromRanges(FTimeSlices);
      ssa.FixOverlapsForward;
      ssa.SaveToFile(FOutputFile, Enc.UTF8);
    end
    else if FProcInfo.InputFileExtension.Equals(extSrt) then
    begin
      Subrip.LoadFromString(Sub);
      Subrip.Events.Value := Subrip.MakeNewFromRanges(FTimeSlices);
      Subrip.FixOverlapsForward;
      Subrip.SaveToFile(FOutputFile, Enc.UTF8);
    end;
  finally
    ssa.Free;
    Subrip.Free;
  end;
end;

procedure TSubzBorProcThread.SubzBorSplitSub;
var
  Subrip: TSubripFile;
  s,Cmd: String;
begin
  Cmd := FFmpegStraightExportCmd;
  s := EmptyStr;
  if FProcInfo.NeedReport then
    s := '-report';
  Cmd := Cmd.Replace('%report%', s, []);
  Cmd := Cmd.Replace('%cp%', FProcInfo.TextEncoding, []);
  Cmd := Cmd.Replace('%i%', QuoteAndEscape(FProcInfo.InputFile), []);
  FOutputFile := GenFileName(FProcInfo.InputFile, wSubzBor, extSrt, True, FOutputDir);
  Cmd := Cmd.Replace('%o%', QuoteAndEscape(FOutputFile), []);
  RunCmd(FProcInfo.FFmpeg, Cmd);
  Subrip := TSubripFile.Create;
  try
    Subrip.LoadFromFile(FOutputFile);
    Subrip.Events.Value := Subrip.MakeNewFromRanges(FTimeSlices);
    Subrip.FixOverlapsForward;
    Subrip.SaveToFile(FOutputFile);
  finally
    Subrip.Free;
  end;
  AdjustFileLineBreaks(FOutputFile);
end;

procedure TSubzBorProcThread.FFmpegSplitSub;
var
  f,s,Cmd: String;
  i: Integer;
  Offs: Double;
begin
  f := EmptyStr;
  Offs := 0;
  with FTimeSlices do
  begin
    for i := 0 to Count-1 do
    begin
      Cmd := FFmpegSplitCmd;
      s := EmptyStr;
      if FProcInfo.NeedReport then
        s := '-report';
      Cmd := Cmd.Replace('%report%', s, []);
      s := EmptyStr;
      if Values[i].Delay <> 0 then
        s := '-itsoffset ' +Values[i].Delay.ToString;
      Cmd := Cmd.Replace('%delay%', s, []);
      Cmd := Cmd.Replace('%cp%', FProcInfo.TextEncoding, []);
      Cmd := Cmd.Replace('%i%', QuoteAndEscape(FProcInfo.InputFile), []);
      Cmd := Cmd.Replace('%splitstart%', Values[i].Value.StartPos.ValueAsString, []);
      Cmd := Cmd.Replace('%splitend%', Values[i].Value.EndPos.ValueAsString, []);
      s := EmptyStr;
      if i > 0 then
        s := '-output_ts_offset ' +Offs.ToString;
      Cmd := Cmd.Replace('%offset%', s, []);
      s := GenFileName(FProcInfo.InputFile, i.ToString, extSrt, True, FTempsDir);
      Cmd := Cmd.Replace('%o%', QuoteAndEscape(s), []);
      RunCmd(FProcInfo.FFmpeg, Cmd);
      f := f+'|'+s;
      Offs := Offs +Values[i].Duration.ValueAsDouble;
    end;
  end;
  f := f.Substring(1);
  FFmpegExportSub(f);
end;

procedure TSubzBorProcThread.FFmpegExportSub(const Sub: String);
var
  s,Cmd: String;
begin
  Cmd := FFmpegExportCmd;
  s := EmptyStr;
  if FProcInfo.NeedReport then
    s := '-report';
  Cmd := Cmd.Replace('%report%', s, []);
  Cmd := Cmd.Replace('%i%', QuoteAndEscape('concat:' +Sub), []);
  FOutputFile := GenFileName(FProcInfo.InputFile, wSubzBor, extSrt, True, FOutputDir);
  Cmd := Cmd.Replace('%o%', QuoteAndEscape(FOutputFile), []);
  RunCmd(FProcInfo.FFmpeg, Cmd);
  AdjustFileLineBreaks(FOutputFile);
end;

procedure TSubzBorProcThread.FFmpegDummyVid;
var
  s,Cmd: String;
begin
  if FProcInfo.DummyVidExists then Exit;
  Cmd := FFmpegDummyVidCmd;
  s := EmptyStr;
  if FProcInfo.NeedReport then
    s := '-report';
  Cmd := Cmd.Replace('%report%', s, []);
  s := IncludeTrailingPathDelimiter(FProcInfo.ToolsFolder) +DummyPicFileName;
  Cmd := Cmd.Replace('%i%', QuoteAndEscape(s), []);
  s := FProcInfo.ToolsFolder +DummyVidFileName;
  Cmd := Cmd.Replace('%o%', QuoteAndEscape(s), []);
  RunCmd(FProcInfo.FFmpeg, Cmd);
end;

procedure TSubzBorProcThread.MkvMergeSplitSub;
var
  f: TStringArray;
  s,Cmd: String;
  i: Integer;
begin
  with FTimeSlices do
  begin
    SetLength(f, Count);
    for i := 0 to Count-1 do
    begin
      Cmd := MkvMergeSplitCmd;
      s := EmptyStr;
      if FProcInfo.NeedReport then
      begin
        s := QuoteAndEscape(FReportsDir +'mkvmerge-' +NowToString +extLog);
        s := '--redirect-output '+s;
      end;
      Cmd := Cmd.Replace('%report%', s, []);
      s := Trunc(Values[i].Delay * 1000).ToString;
      Cmd := Cmd.Replace('%delay%', s, []);
      s := FProcInfo.ToolsFolder +DummyVidFileName;
      Cmd := Cmd.Replace('%i%', QuoteAndEscape(s), []);
      Cmd := Cmd.Replace('%i%', QuoteAndEscape(FProcInfo.InputFile), []);
      s := GenFileName(FProcInfo.InputFile, i.ToString, extMkv, True, FTempsDir);
      Cmd := Cmd.Replace('%o%', QuoteAndEscape(s), []);
      Cmd := Cmd.Replace('%timeslice%', Values[i].ValueAsString, []);
      RunCmd(FProcInfo.MkvMerge, Cmd);
      f[i] := s;
      StepProgress(FProgress+(20 div Count));
    end;
  end;
  MkvMergeAppendSub(f);
end;

procedure TSubzBorProcThread.MkvMergeAppendSub(const Files: TStringArray);
var
  f,s,Cmd: String;
begin
  Cmd := MkvMergeAppendCmd;
  s := EmptyStr;
  if FProcInfo.NeedReport then
  begin
    s := QuoteAndEscape(FReportsDir +'mkvmerge-' +NowToString +extLog);
    s := '--redirect-output '+s;
  end;
  Cmd := Cmd.Replace('%report%', s, []);
  s := GenFileName(FProcInfo.InputFile, wSubzBor, extMkv, True, FTempsDir);
  Cmd := Cmd.Replace('%o%', QuoteAndEscape(s), []);
  s := EmptyStr;
  for f in Files do
    s := s+'( ' +QuoteAndEscape(f) +' ) + ';
  s := s.Substring(0, s.Length-(' + ').Length);
  Cmd := Cmd.Replace('%i%', s, []);
  RunCmd(FProcInfo.MkvMerge, Cmd);
end;

procedure TSubzBorProcThread.MkvExtractSub;
var
  s,Cmd: String;
begin
  Cmd := MkvExtractSubCmd;
  s := EmptyStr;
  if FProcInfo.NeedReport then
  begin
    s := QuoteAndEscape(FReportsDir +'mkvextract-' +NowToString +extLog);
    s := '--redirect-output '+s;
  end;
  Cmd := Cmd.Replace('%report%', s, []);
  s := GenFileName(FProcInfo.InputFile, wSubzBor, extMkv, True, FTempsDir);
  Cmd := Cmd.Replace('%i%', QuoteAndEscape(s), []);
  FOutputFile := GenFileName(FProcInfo.InputFile, wSubzBor,
    FProcInfo.InputFileExtension, True, FOutputDir);
  Cmd := Cmd.Replace('%o%', QuoteAndEscape('1:' +FOutputFile), []);
  RunCmd(FProcInfo.MkvExtract, Cmd);
end;

procedure TSubzBorProcThread.ExportTimeSlices;
var
  sl: TStringList;
  f: String;
begin
  if not FProcInfo.SaveTimeSlices then Exit;
  f := GenFileName(FProcInfo.InputFile, wCuts, extText, True, FOutputDir);
  sl := TStringList.Create;
  try
    sl.Text := FProcInfo.TimeSlices;
    sl.SaveToFile(f);
  finally
    sl.Free;
  end;
end;

procedure TSubzBorProcThread.ClearTemps;
begin
  DeleteDirectory(FTempsDir, False);
end;

procedure TSubzBorProcThread.GatherReports;
var
  sl: TStringList;
  i: Integer;
begin
  if not FProcInfo.NeedReport then Exit;
  sl := TStringList.Create;
  try
    FindAllFiles(sl, FProcInfo.SubzBorPath, '*' +extLog, False);
    if sl.Count > 0 then
      for i := sl.Count-1 downto 0 do
      begin
        CopyFile(sl.Strings[i], FReportsDir +ExtractFileName(sl.Strings[i]),
          [cffOverwriteFile], True);
        DeleteFile(sl.Strings[i]);
      end;
  except
    on E: Exception do
      FProcResult.LastError := FProcResult.LastError +LineEnding +E.Message;
  end;
    sl.Free;
end;

procedure TSubzBorProcThread.Execute;
begin
  try
    if not DirectoryExists(FTempsDir) and not CreateDir(FTempsDir) then
      raise Exception.Create(rsNoTempsDir);
    if not DirectoryExists(FReportsDir) and not CreateDir(FReportsDir) then
      raise Exception.Create(rsNoReportsDir);
    if FProcInfo.InputFileIsText then
    begin
      if FProcInfo.UseInternalCodecs then
        SubzBorDirectProcessSub
      else
        begin
          if FProcInfo.UseInternalSplitter then
            SubzBorSplitSub
          else
            FFmpegSplitSub;
        end;
      StepProgress(90);
    end
    else
    begin
      FFmpegDummyVid;
      StepProgress(50);
      if Assigned(OnProgress) then
        Synchronize(OnProgress);
      MkvMergeSplitSub;
      StepProgress(85);
      MkvExtractSub;
      StepProgress(95);
    end;
    if not FileExists(FOutputFile) then
      raise Exception.Create(rsSeemsNoFileSaved);
  except
    on E: Exception do
      FProcResult.LastError := E.Message;
  end;
  ExportTimeSlices;
  ClearTemps;
  GatherReports;
end;

constructor TSubzBorProcThread.Create(const ProcInfo: TSubzBorProcInfo);
begin
  inherited Create(True);
  FProcInfo := ProcInfo;
  FTimeSlices.ExtendedValue := FProcInfo.TimeSlices;
  FTempsDir := FProcInfo.ToolsFolder +'temps' +PathDelim;
  FReportsDir := FProcInfo.ToolsFolder +'reports' +PathDelim;
  FOutputDir := ExtractFilePath(FProcInfo.InputFile);
  if not TryDirectoryIsWritable(FOutputDir) then
  begin
    {$IFDEF WINDOWS}
    FOutputDir := GetWindowsSpecialDir(CSIDL_PERSONAL);
    {$ENDIF}
    {$IFDEF UNIX}
    FOutputDir := GetUserDir;
    {$ENDIF}
  end;
  FreeOnTerminate := True;
end;

destructor TSubzBorProcThread.Destroy;
begin
  if Assigned(OnProcDone) then
    Synchronize(OnProcDone);
  FProcInfo := Default(TSubzBorProcInfo);
  inherited;
end;

end.

