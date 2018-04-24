unit uWebsockSimple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynCommons, SynCrtSock, SynBidirSock, uChatBuffer;

type

  { TSimpleWebsocketServer }

  TSimpleWebsocketServer=class
    private
      fServer:TWebSocketServer;
    protected
    public
      ChatBuffer: TCefChatBuffer;

      constructor Create(const Port:string; ChatBuf:TCefChatBuffer=nil);
      destructor Destroy; override;

      procedure BroadcastMsg(const msg: RawByteString);
      procedure AddChatbuf(const msg: RawByteString);
  end;

  { TWebSocketProtocolEcho }

  TWebSocketProtocolEcho = class(TWebSocketProtocolChat)
    public
      Server: TSimpleWebsocketServer;

      procedure onExIncomeFrame(Sender: THttpServerResp; const Frame: TWebSocketFrame);
  end;


implementation


{ TWebSocketProtocolEcho }

procedure TWebSocketProtocolEcho.onExIncomeFrame(Sender: THttpServerResp;
  const Frame: TWebSocketFrame);
begin
  case Frame.opcode of
  focBinary, focText :
      if Assigned(Server) then begin
        Server.BroadcastMsg(Frame.payload);
        Server.AddChatbuf(Frame.payload);
      end;
  end;
end;

{ TSimpleWebsocketServer }

constructor TSimpleWebsocketServer.Create(const Port: string;
  ChatBuf: TCefChatBuffer);
var
  protocol:TWebSocketProtocolEcho;
begin
  fServer:=TWebSocketServer.Create(Port,nil,nil,'kakaochat');
  ChatBuffer:=ChatBuf;
  protocol:=TWebSocketProtocolEcho.Create('chat','');
  protocol.Server:=Self;
  protocol.OnIncomingFrame:=@protocol.onExIncomeFrame;
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

procedure TSimpleWebsocketServer.AddChatbuf(const msg: RawByteString);
begin
  if Assigned(ChatBuffer) then
    ChatBuffer.Add(msg);
end;


end.
