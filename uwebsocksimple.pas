unit uWebsockSimple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynCommons, SynCrtSock, SynBidirSock;

type

  { TWebSocketProtocolEcho }

  TWebSocketProtocolEcho = class(TWebSocketProtocolChat)
  end;

  { TSimpleWebsocketServer }

  TSimpleWebsocketServer=class
    private
      fServer:TWebSocketServer;
    protected
    public
      constructor Create(const Port:string);
      destructor Destroy; override;

      procedure BroadcastMsg(const msg: RawByteString);
  end;

implementation

{ TSimpleWebsocketServer }

constructor TSimpleWebsocketServer.Create(const Port: string);
var
  protocol:TWebSocketProtocolEcho;
begin
  fServer:=TWebSocketServer.Create(Port,nil,nil,'kakaochat');
  protocol:=TWebSocketProtocolEcho.Create('chat','');
  fServer.WebSocketProtocols.Add(protocol);
end;

destructor TSimpleWebsocketServer.Destroy;
begin
  fServer.Free;
  inherited Destroy;
end;

procedure TSimpleWebsocketServer.BroadcastMsg(const msg:RawByteString);
var
  outmsg:TWebSocketFrame;
begin
  outmsg.opcode:=focText;
  outmsg.payload:=msg;
  fServer.WebSocketBroadcast(outmsg);
end;


end.
