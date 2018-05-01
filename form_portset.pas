unit form_portset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormPortSet }

  TFormPortSet = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    EditInt: TEdit;
    EditHttp: TEdit;
    EditChat: TEdit;
    EditAlert: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    PortHTTP, PortChat, PortAlert: string;
    Interval:Integer;
  end;

var
  FormPortSet: TFormPortSet;

implementation

{$R *.lfm}

uses DefaultTranslator;

function CheckAssign(const s, defvalue:string):string;
var
  x:Integer;
begin
  x:=StrToIntDef(s,0);
  if x=0 then
    Result:=defvalue
    else
      Result:=s;
end;

{ TFormPortSet }

procedure TFormPortSet.ButtonOkClick(Sender: TObject);
begin
  PortHTTP:=CheckAssign(EditHttp.Text,'8090');
  PortChat:=CheckAssign(EditChat.Text,'8092');
  PortAlert:=CheckAssign(EditAlert.Text,'8094');
  Interval:=StrToIntDef(EditInt.Text,300);
end;

procedure TFormPortSet.FormShow(Sender: TObject);
begin
  EditHttp.Text:=PortHTTP;
  EditChat.Text:=PortChat;
  EditAlert.Text:=PortAlert;
  EditInt.Text:=IntToStr(Interval);
end;

end.

