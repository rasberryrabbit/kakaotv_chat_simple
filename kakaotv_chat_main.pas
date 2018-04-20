unit kakaotv_chat_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  cef3types, cef3lib, cef3intf, cef3lcl, cef3ref, cef3api, cef3own, cef3gui,
  lNetComponents, lhttp, lNet, UniqueInstance, loglistfpc, syncobjs;

type

  { TKaKaoRenderProcessHandler }

  TKaKaoRenderProcessHandler=class(TCefRenderProcessHandlerOwn)
    protected
      function OnProcessMessageReceived(const browser: ICefBrowser;
        sourceProcess: TCefProcessId; const message: ICefProcessMessage
        ): Boolean; override;
      procedure OnBrowserCreated(const browser: ICefBrowser); override;
      procedure OnUncaughtException(const browser: ICefBrowser;
        const frame: ICefFrame; const context: ICefV8Context;
        const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
        override;
  end;


  { TFormKakaoTVChat }

  TFormKakaoTVChat = class(TForm)
    Button1: TButton;
    CheckBoxRemSyS: TCheckBox;
    CheckBoxDisableLog: TCheckBox;
    CheckBoxClearB: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FEventMain:TEvent;
  public
    log:TLogListFPC;

    function TryEnter:Boolean;
    procedure Leave;

    procedure HttpError(const msg: string; aSocket: TLSocket);
    procedure CefLoadStart(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; transitionType: TCefTransitionType);

  end;

var
  FormKakaoTVChat: TFormKakaoTVChat;

implementation

{$R *.lfm}

uses
  sha1, uChatBuffer, uhttpHandleCEF, lMimeTypes, uRequestHandler, uKakaoCEF,
  uWebsockSimple;

const
  MaxChecksum = 3;

var
  cefb : TkakaoCEF;
  MainBrowser : ICefBrowser;

  lastchecksum : array[0..MaxChecksum] of TSHA1Digest;
  lastchkCount : Integer = 0;
  lastDupChk : Integer = 0;

  ChatBuffer:TCefChatBuffer;
  ChatHead:TCefChatBuffer;
  ChatScript:TCefChatBuffer;

  HttpServer:TBigFileLHTTPServerComponent;
  ImgPath:string;

  WebSockChat:TSimpleWebsocketServer;
  WebSockAlert:TSimpleWebsocketServer;


type

  { TElementIdVisitor }

  TElementIdVisitor = class(TCefDomVisitorOwn)
  private
    FNameID: string;
    FEvent:TEvent;
  protected
    procedure Visit(const document: ICefDomDocument); override;
  public
    constructor Create(const AId: string); reintroduce;
    destructor Destroy; override;

    procedure Enter;
    procedure Leave;
  end;


procedure ProcessElementsById(const AFrame: ICefFrame; const AId: string);
var
  Visitor: TElementIdVisitor;
  surl:string;
  retv8, v8:ICefV8Value;
  errv8:ICefV8Exception;
begin
  if Assigned(AFrame) then
  begin
    {
    surl:='<iframe src="'+UTF8Encode(AFrame.Url)+'" ></iframe>';
    if ChatScript.IndexOf(surl)=-1 then
      ChatScript.Add(surl);
    }
    if (0<>Pos('live/chat/',AFrame.GetUrl)) then begin
      Visitor := TElementIdVisitor.Create(AId);
      AFrame.VisitDom(Visitor);
    end;
  end;
end;

{ TKaKaoRenderProcessHandler }

function TKaKaoRenderProcessHandler.OnProcessMessageReceived(
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage): Boolean;
var
  chatframe:ICefFrame;
  fcount, i:TSize;
  fid:array of int64;
  surl:string;
begin
  Result:=inherited OnProcessMessageReceived(browser, sourceProcess, message);
  if message.Name='visitdom' then begin
    { thread-safe? }
    if FormKakaoTVChat.TryEnter then begin
      try
        {
        surl:='<iframe src="'+UTF8Encode(browser.MainFrame.Url)+'" ></iframe>';
        if ChatScript.IndexOf(surl)=-1 then
          ChatScript.Add(surl);
        }
        fcount:=browser.GetFrameCount;
        SetLength(fid,fcount);
        try
          browser.GetFrameIdentifiers(@fcount,@fid[0]);
          for i:=0 to fcount-1 do begin
            chatframe:=browser.GetFrameByident(fid[i]);
            ProcessElementsById(chatframe,'chatArea');
          end;
        finally
          SetLength(fid,0);
        end;
      finally
        FormKakaoTVChat.Leave;
      end;
    end;
  end;
end;

procedure TKaKaoRenderProcessHandler.OnBrowserCreated(const browser: ICefBrowser);
begin
  inherited OnBrowserCreated(browser);
  MainBrowser:=browser;
end;

procedure TKaKaoRenderProcessHandler.OnUncaughtException(
  const browser: ICefBrowser; const frame: ICefFrame;
  const context: ICefV8Context; const exception: ICefV8Exception;
  const stackTrace: ICefV8StackTrace);
begin
  inherited OnUncaughtException(browser, frame, context, exception, stackTrace);
end;


{ TElementNameVisitor }

constructor TElementIdVisitor.Create(const AId: string);
begin
  inherited Create;
  FNameID := AId;
  FEvent:=TEvent.Create(nil,True,True,'CEFELVI'+IntToStr(GetTickCount64));
end;

destructor TElementIdVisitor.Destroy;
begin
  FEvent.Free;
  inherited Destroy;
end;

procedure TElementIdVisitor.Enter;
begin
  while FEvent.WaitFor(0)=wrTimeout do
    Sleep(0);
  FEvent.ResetEvent;
end;

procedure TElementIdVisitor.Leave;
begin
  FEvent.SetEvent;
end;

procedure TElementIdVisitor.Visit(const document: ICefDomDocument);
var
  NodeH : ICefDomNode;
  stemp : string;
  procedure ProcessNode(ANode: ICefDomNode);
  var
    Node, Nodex, NodeN, NodeName, NodeChat: ICefDomNode;
    s, smarkup, sclass, sbuf, scheck, sockchat: UnicodeString;
    checksumN : TSHA1Digest;
    bottomchecksum : array[0..MaxChecksum] of TSHA1Digest;
    chkCount, dupCount, dupCountChk, i, j, ItemCount : Integer;
    matched, skipAddMarkup, disLog, RemoveSys, doAddMsg : Boolean;
  begin
    if Assigned(ANode) then
    begin
      RemoveSys:=FormKakaoTVChat.CheckBoxRemSyS.Checked;
      disLog:=FormKakaoTVChat.CheckBoxDisableLog.Checked;
      Node := ANode.FirstChild;
      while Assigned(Node) do begin
        if Node.GetElementAttribute('id')=FNameID then begin
          ItemCount:=0;
          Nodex:=Node.LastChild;
          chkCount:=0;
          dupCount:=0;
          sockchat:='';
          while Assigned(Nodex) do begin
            sbuf:='';
            s:='';
            doAddMsg:=True;
            if Nodex.HasChildren then begin
              NodeName:=Nodex.FirstChild;
              NodeChat:=NodeName.NextSibling;
            end else
              NodeName:=nil;

            // check MaxChecksum+1 bottom lines
            NodeN:=Nodex;
            matched:=lastchkCount>0;
            i:=0;
            dupCountChk:=lastDupChk+1;
            while Assigned(NodeN) do begin
              scheck:=document.BaseUrl+NodeN.AsMarkup;
              checksumN:=SHA1Buffer(scheck[1],Length(scheck)*SizeOf(WideChar));

              if matched and (i<lastchkCount) then begin
                if SHA1Match(checksumN,lastchecksum[i]) then begin
                  if i=0 then
                    Dec(dupCountChk);
                  if dupCountChk<1 then
                    Inc(i);
                end else
                  matched:=False;
              end;

              // fill bottom checksum
              if chkCount<=MaxChecksum then begin
                // check duplication on first checksum
                if (chkCount=1) and
                  SHA1Match(checksumN,bottomchecksum[0]) then
                  Inc(dupCount)
                  else begin
                    bottomchecksum[chkCount]:=checksumN;
                    Inc(chkCount);
                  end;
              end else
                break;

              NodeN:=NodeN.PreviousSibling;
            end;
            if matched and (dupCountChk>=0) then
              break;

            smarkup:=Nodex.AsMarkup;
            skipAddMarkup:=False;
            // get chat message
            if Assigned(NodeName) and Assigned(NodeChat) then begin
              sbuf:=sbuf+NodeName.ElementInnerText+' : ';
              while Assigned(NodeChat) do begin
                // make log message
                sclass:=NodeChat.GetElementAttribute('CLASS');
                if sclass='txt_talk' then
                  sbuf:=sbuf+NodeChat.ElementInnerText
                else
                if Pos('kakao_emoticon',sclass)<>0 then begin
                  skipAddMarkup:=True;
                  sbuf:=sbuf+NodeChat.AsMarkup;
                end else
                  sbuf:=sbuf+NodeChat.AsMarkup;
                NodeChat:=NodeChat.NextSibling;
              end;
              s:=sbuf+s;
              // change img url
              if skipAddMarkup then begin
                scheck:=Nodex.AsMarkup;
                i:=Pos('?',scheck);
                j:=i;
                while i>0 do begin
                  if scheck[i]='/' then
                    break;
                  Dec(i);
                end;
                if i>0 then begin
                  Dec(i);
                  while i>0 do begin
                    if scheck[i]='/' then
                      break;
                    Dec(i);
                  end;
                end;
                Inc(i);
                sclass:=UnicodeStringReplace(Copy(scheck,i,j-i),'/','_',[rfReplaceAll]);
                i:=Pos('//',scheck);
                if i<>0 then
                  scheck:=Copy(scheck,1,i-1)+'img/'+sclass+Copy(scheck,j);
              end;
            end else if Assigned(NodeName) then begin
              // cookie alert
              if NodeName.GetElementAttribute('CLASS')='box_alert' then begin
                NodeChat:=NodeName.FirstChild;
                while Assigned(NodeChat) do begin
                  sclass:=NodeChat.GetElementAttribute('CLASS');
                  if sclass='txt_cookie' then
                    sbuf:=sbuf+'<< '+NodeChat.ElementInnerText+' >>'
                    else if sclass='txt_name' then
                      sbuf:=sbuf+' '+NodeChat.ElementInnerText
                      else if sclass='txt_msg' then
                        sbuf:=sbuf+' : '+NodeChat.ElementInnerText;
                  NodeChat:=NodeChat.NextSibling;
                end;
                scheck:=Nodex.AsMarkup;
                ChatScript.Add(UTF8Encode(scheck));
                // websock send alert
                WebSockAlert.BroadcastMsg(pchar(UTF8Encode(scheck)));
                s:=sbuf+s;
              end else begin
                if RemoveSys and (Pos('txt_system',smarkup)<>0) then
                  doAddMsg:=False;
                s:=smarkup+s;
              end;
            end else
              s:=smarkup+s;

            if doAddMsg then begin
              // fill by markup
              if not skipAddMarkup then
                scheck:=Nodex.AsMarkup;

              // chat
              i:=ChatBuffer.Count-ItemCount;
              if i<0 then
               i:=0;
              ChatBuffer.Insert(i,UTF8Encode(scheck));
              sockchat:=scheck+#13#10+sockchat;
              // log
              if not disLog then begin
                j:=FormKakaoTVChat.log.Count-ItemCount;
                if j<0 then
                 j:=0;
                FormKakaoTVChat.log.InsertLog(j,UTF8Encode(s));
              end;
            end;

            Nodex:=Nodex.PreviousSibling;
            Inc(ItemCount);
            if ItemCount>=ChatBuffer.MaxLines then
              break;
          end;
          // send chat to websocket
          if sockchat<>'' then
            WebSockChat.BroadcastMsg(pchar(UTF8Encode(sockchat)));

          // set last checksum
          if chkCount>0 then begin
            for i:=0 to chkCount-1 do
              lastchecksum[i]:=bottomchecksum[i];
            lastchkCount:=chkCount;
            lastDupChk:=dupCount;
          end;

          break;

        end;
        ProcessNode(Node);
        Node := Node.NextSibling;
      end;
    end;
  end;

begin
  if Assigned(document.Head) then begin
    NodeH := document.Head.FirstChild;
    while Assigned(NodeH) do begin
      stemp:=UTF8Encode(NodeH.AsMarkup);
      if ChatHead.IndexOf(stemp)=-1 then
        ChatHead.Add(stemp);
      NodeH:=NodeH.NextSibling;
    end;
  end;
  ProcessNode(document.Body);
end;


{ TFormKakaoTVChat }

procedure TFormKakaoTVChat.FormCreate(Sender: TObject);
begin
  IsMultiThread:=True;
  ChatBuffer:=TCefChatBuffer.Create;
  ChatHead:=TCefChatBuffer.Create;
  ChatScript:=TCefChatBuffer.Create;
  log:=TLogListFPC.Create(self);
  log.Parent:=Panel2;
  log.Align:=alClient;
  FEventMain:=TEvent.Create(nil,True,True,'KAKAOMAIN'+IntToStr(GetTickCount64));
  CefSingleProcess:=True; //must be true
  CefNoSandbox:=False;
  CefLogSeverity:=LOGSEVERITY_ERROR_REPORT;
  // doc folder
  ImgPath:=ExtractFilePath(Application.ExeName)+'doc';
  if not DirectoryExists(ImgPath) then
    CreateDir(ImgPath);
  // image fodler
  cefImageFolder:=ImgPath+PathDelim+'img';
  if not DirectoryExists(cefImageFolder) then
    CreateDir(cefImageFolder);
  cefb:=TkakaoCEF.Create(self);
  cefb.Name:='cefKakao';
  cefb.Parent:=Panel1;
  cefb.Align:=alClient;
  cefb.OnLoadStart:=@CefLoadStart;
  WebSockChat:=TSimpleWebsocketServer.Create('0.0.0.0:8190');
  WebSockAlert:=TSimpleWebsocketServer.Create('0.0.0.0:8192');
end;

procedure TFormKakaoTVChat.FormDestroy(Sender: TObject);
begin
  ChatScript.Free;
  ChatHead.Free;
  ChatBuffer.Free;
  FEventMain.Free;
  Sleep(100);
end;

procedure TFormKakaoTVChat.Button1Click(Sender: TObject);
begin
  Timer1.Enabled:=not Timer1.Enabled;
  if Timer1.Enabled then
    Button1.Caption:='Stop'
    else
      Button1.Caption:='Activate';
end;

procedure TFormKakaoTVChat.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  WebSockChat.Free;
  WebSockAlert.Free;

end;

procedure TFormKakaoTVChat.FormShow(Sender: TObject);
var
  x : TBigFileURIHandler;
  b : TStringObject;
begin
  //
  cefb.Load(UTF8Decode('https://tv.kakao.com'));
  //
  InitMimeList('');
  b:=TStringObject.Create;
  b.Str:='text/html';
  MimeList.AddObject('.html', b);
  b:=TStringObject.Create;
  b.Str:='text/html';
  MimeList.AddObject('.htm', b);
  b:=TStringObject.Create;
  b.Str:='text/css';
  MimeList.AddObject('.css', b);
  b:=TStringObject.Create;
  b.Str:='text/javascript';
  MimeList.AddObject('.js', b);
  b:=TStringObject.Create;
  b.Str:='text';
  MimeList.AddObject('.txt', b);
  b:=TStringObject.Create;
  b.Str:='image';
  MimeList.AddObject('.jpg', b);
  b:=TStringObject.Create;
  b.Str:='image';
  MimeList.AddObject('.png', b);
  //
  HttpServer:=TBigFileLHTTPServerComponent.Create(self);
  HttpServer.OnError:=@HttpError;
  x := TBigFileURIHandler.Create;
  x.Methods:=[hmHead,hmGet,hmPost];
  x.DocRoot:=ImgPath; // ExtractFilePath(Application.ExeName);
  x.ChatListBuf:=ChatBuffer;
  x.ChatHead:=ChatHead;
  x.ChatScript:=ChatScript;
  HttpServer.RegisterHandler(x);
  HttpServer.Port:=8090;
  HttpServer.Listen(8090);
end;

procedure TFormKakaoTVChat.Timer1Timer(Sender: TObject);
begin
  cefb.Browser.SendProcessMessage(PID_RENDERER,TCefProcessMessageRef.New('visitdom'));
end;

function TFormKakaoTVChat.TryEnter: Boolean;
begin
  Result:=FEventMain.WaitFor(0)=wrSignaled;
  if Result then
    FEventMain.ResetEvent;
end;

procedure TFormKakaoTVChat.Leave;
begin
  FEventMain.SetEvent;
end;

procedure TFormKakaoTVChat.HttpError(const msg: string; aSocket: TLSocket);
begin
  log.AddLogLine(msg);
end;

procedure TFormKakaoTVChat.CefLoadStart(Sender: TObject; const Browser: ICefBrowser;
  const Frame: ICefFrame; transitionType: TCefTransitionType);
begin
  if TryEnter then begin
    try
      ChatHead.Clear;
      //ChatScript.Clear;
      if CheckBoxClearB.Checked then
        ChatBuffer.Clear;
      log.Font.Name:='Default';
    finally
      Leave;
    end;
  end;
end;

procedure AppExceptProc(Obj : TObject; Addr : CodePointer; FrameCount:Longint; Frame: PCodePointer);
begin
  ShowMessage(Format('%s',[BacktraceStrFunc(Addr)]));
end;

initialization
  CefRenderProcessHandler := TKaKaoRenderProcessHandler.Create;
  //ExceptProc:=@AppExceptProc;

end.

