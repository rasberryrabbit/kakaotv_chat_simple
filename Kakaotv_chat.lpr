program Kakaotv_chat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, kakaotv_chat_main, lnetvisual, uniqueinstance_package, uChatBuffer,
  uKakaoCEF, uWebsockSimple, usimplewebsockcallback;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormKakaoTVChat, FormKakaoTVChat);
  Application.Run;
end.

