program Kakaotv_chat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  uCEFApplication,
  Forms, kakaotv_chat_main, lnetvisual, uniqueinstance_package,
  uWebsockSimple, form_portset, uformDebug,
  ucustomCEFResHandler;

{$R *.res}

begin
  RequireDerivedFormResource:=True;

  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormKakaoTVChat, FormKakaoTVChat);
  Application.CreateForm(TFormDebug, FormDebug);
  Application.Run;
  end;

  DestroyGlobalCEFApp;
end.

