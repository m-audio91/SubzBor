program subzbor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, uDatas, uPrefs, ucharenc, uResourcestrings, uAbout,
  uTimeSliceEditEx, ui18nGuide;

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
  Application.Run;
end.
