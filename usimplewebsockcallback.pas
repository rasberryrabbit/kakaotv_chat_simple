unit usimplewebsockcallback;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynCommons, mORMot;

type
  IWebsockSimpleCallBack=interface(IInvokable)
   ['{DFD41821-AE13-42A8-A239-B7F450C24CAF}']
   procedure NotifyMessage(const pseudo, msg:string);
  end;

  IWebsockSimpleService=interface(IServiceWithCallbackReleased)
   ['{336B0689-64B9-4096-95FD-FDD8F48EB203}']
   procedure Join(const pseudo:string; const callback: IWebsockSimpleCallBack);
   procedure NotifyMsg(const pseudo, msg: string);
  end;

const
  SimpleWebsocket_Trans_Key='kakaochatsimple';


implementation

initialization
  TInterfaceFactory.RegisterInterfaces([TypeInfo(IWebsockSimpleService),
  TypeInfo(IWebsockSimpleCallBack)]);

end.

