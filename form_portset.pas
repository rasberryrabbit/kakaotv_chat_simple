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
    EditHttp: TEdit;
    EditChat: TEdit;
    EditAlert: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    PortHTTP, PortChat, PortAlert: string;
  end;

var
  FormPortSet: TFormPortSet;

implementation

{$R *.lfm}

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
end;

procedure TFormPortSet.FormShow(Sender: TObject);
begin
  EditHttp.Text:=PortHTTP;
  EditChat.Text:=PortChat;
  EditAlert.Text:=PortAlert;
end;

end.

