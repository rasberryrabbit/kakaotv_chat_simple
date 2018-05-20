unit uformDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  loglistfpc;

type

  { TFormDebug }

  TFormDebug = class(TForm)
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    log: TLogListFPC;

    procedure logdebug(s:string);
  end;

var
  FormDebug: TFormDebug;

implementation

{$R *.lfm}

uses kakaotv_chat_main;

{ TFormDebug }

procedure TFormDebug.FormCreate(Sender: TObject);
begin
  log:=TLogListFPC.Create(self);
  log.Parent:=Panel1;
  log.Align:=alClient;
end;

procedure TFormDebug.FormShow(Sender: TObject);
begin
  Top:=FormKakaoTVChat.Top+FormKakaoTVChat.Height+5;
  Left:=FormKakaoTVChat.Left;
end;

procedure TFormDebug.logdebug(s: string);
begin
  if Showing then
    log.AddLog(s);
end;

end.

