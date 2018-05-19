unit kakaotv_chat_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ActnList, cef3types, cef3lib, cef3intf, cef3lcl, cef3ref, cef3api,
  cef3own, cef3gui, lNetComponents, lhttp, lNet, UniqueInstance, loglistfpc,
  syncobjs;

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
    ActionAutoStart: TAction;
    ActionPortSet: TAction;
    ActionList1: TActionList;
    ButtonStart: TButton;
    ButtonBrowse: TButton;
    CheckBoxRemSyS: TCheckBox;
    CheckBoxDisableLog: TCheckBox;
    CheckBoxClearB: TCheckBox;
    EditURL: TEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure ActionAutoStartExecute(Sender: TObject);
    procedure ActionPortSetExecute(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonBrowseClick(Sender: TObject);
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
    procedure CefAddressChange(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; const url: ustring);

  end;

var
  FormKakaoTVChat: TFormKakaoTVChat;

implementation

{$R *.lfm}

uses
  uChatBuffer, uhttpHandleCEF, lMimeTypes, uRequestHandler, uKakaoCEF,
  uWebsockSimple, form_portset, IniFiles, Hash, uhashimpl, DefaultTranslator;

const
  MaxChecksum = 3;

var
  cefb : TkakaoCEF;
  MainBrowser : ICefBrowser;

  lastchecksum : array[0..MaxChecksum] of THashDigest;
  lastchkCount : Integer = 0;
  lastDupChk : array[0..MaxChecksum] of Integer;

  ChatBuffer:TCefChatBuffer;
  ChatHead:TCefChatBuffer;
  ChatScript:TCefChatBuffer;

  HttpServer:TBigFileLHTTPServerComponent;
  ImgPath:string;

  WebSockChat:TSimpleWebsocketServer;
  WebSockAlert:TSimpleWebsocketServer;
  WebSockRChat:TSimpleWebsocketServer;

  PortHttp:string = '8090';
  PortChat:string = '8092';
  PortAlert:string= '8094';
  PortRChat:string= '8088';
  cInterval:Integer= 300;

  LogAttrName : UnicodeString = 'id';
  LogAttrValue : UnicodeString = 'chatArea';

  LogChatClass : UnicodeString = 'CLASS';
  LogSessionAttr : UnicodeString = 'data-sessionid';
  LogChatValue : UnicodeString = 'txt_talk';
  LogChatEmoti : UnicodeString = 'kakao_emoticon';

  LogAlertClass : UnicodeString = 'CLASS';
  LogAlertValue : UnicodeString = 'box_alert';
  LogAlertCookie: UnicodeString = 'txt_cookie';
  LogAlertName  : UnicodeString = 'txt_name';
  LogAlertMsg   : UnicodeString = 'txt_msg';
  LogSysValue   : UnicodeString = 'txt_system';

  LogAddAttr    : string = ' class="kakao_chat" ';


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

    function TryEnter:Boolean;
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
  if not Result then
  if message.Name='visitdom' then begin
    { thread-safe? }
    if FormKakaoTVChat.TryEnter then begin
      try
        fcount:=browser.GetFrameCount;
        SetLength(fid,fcount);
        try
          browser.GetFrameIdentifiers(@fcount,@fid[0]);
          for i:=0 to fcount-1 do begin
            chatframe:=browser.GetFrameByident(fid[i]);
            ProcessElementsById(chatframe,LogAttrValue);
          end;
        finally
          SetLength(fid,0);
        end;
      finally
        FormKakaoTVChat.Leave;
      end;
    end;
    Result:=True;
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
  FEvent.SetEvent;
  FEvent.Free;
  inherited Destroy;
end;

function TElementIdVisitor.TryEnter: Boolean;
begin
  Result:=FEvent.WaitFor(0)<>wrTimeout;
  if Result then
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
    Node, Nodex, NodeN, NodeName, NodeChat, NodeStart, NodeEnd: ICefDomNode;
    s, smarkup, sclass, sbuf, srawbuf, scheck, ssocket, utemp: UnicodeString;
    checksumN : THashDigest;
    bottomchecksum : array[0..MaxChecksum] of THashDigest;
    dupCount, dupCountChk : array[0..MaxChecksum] of Integer;
    chkCount, i, j, ItemCount : Integer;
    matched, skipAddMarkup, disLog, RemoveSys, doAddMsg : Boolean;
    stemp: string;
  begin
    if Assigned(ANode) then
    begin
      RemoveSys:=FormKakaoTVChat.CheckBoxRemSyS.Checked;
      disLog:=FormKakaoTVChat.CheckBoxDisableLog.Checked;
      Node := ANode.FirstChild;
      while Assigned(Node) do begin
        if Node.GetElementAttribute(LogAttrName)=LogAttrValue then begin
          ItemCount:=0;
          Nodex:=Node.LastChild;
          NodeEnd:=Nodex;
          chkCount:=0;
          while Assigned(Nodex) do begin
            // check MaxChecksum+1 bottom lines
            NodeN:=Nodex;
            matched:=lastchkCount>0;
            i:=0;
            dupCountChk:=lastDupChk;
            while Assigned(NodeN) do begin
              // checksum
              scheck:='';
              NodeName:=NodeN.FirstChild;
              if Assigned(NodeName) then begin
                scheck:=scheck+NodeName.AsMarkup;
                // chat - skip it that modified by banned
                //NodeChat:=NodeName.NextSibling;
                //if Assigned(NodeChat) then
                //  scheck:=scheck+NodeChat.ElementInnerText;
              end else
                scheck:=NodeN.ElementInnerText;
              checksumN:=MakeHash(@scheck[1],Length(scheck)*SizeOf(WideChar));

              if matched and (i<lastchkCount) then begin
                if CompareHash(checksumN,lastchecksum[i]) then begin
                  Dec(dupCountChk[i]);
                  if dupCountChk[i]=0 then
                    Inc(i);
                end else
                  matched:=False;
              end;

              // fill bottom checksum
              if chkCount<=MaxChecksum then begin
                // check duplication on first checksum
                if (chkCount>0) and CompareHash(checksumN,bottomchecksum[chkCount-1]) then
                  Inc(dupCount[chkCount-1])
                else begin
                  bottomchecksum[chkCount]:=checksumN;
                  dupCount[chkCount]:=1;
                  Inc(chkCount);
                end;
              end else
                if i>=lastchkCount then
                  break;

              NodeN:=NodeN.PreviousSibling;
            end;
            if matched then
              break;

            NodeStart:=Nodex;
            Nodex:=Nodex.PreviousSibling;

            Inc(ItemCount);
            if ItemCount>=ChatBuffer.MaxLines then
              break;
          end;

          // add chat messages
          Nodex:=NodeStart;
          //ssocket:='';
          while Nodex<>nil do begin

            s:='';
            sbuf:='';
            srawbuf:='';
            doAddMsg:=True;

            if Nodex.HasChildren then begin
              NodeName:=Nodex.FirstChild;
              NodeChat:=NodeName.NextSibling;
            end else
              NodeName:=nil;

            smarkup:=Nodex.AsMarkup;
            scheck:=smarkup;
            skipAddMarkup:=False;
            // get chat message
            if Assigned(NodeName) and Assigned(NodeChat) then begin
              sbuf:=sbuf+NodeName.ElementInnerText+' ('+NodeName.GetElementAttribute(LogSessionAttr)+'): ';
              srawbuf:=sbuf;
              while Assigned(NodeChat) do begin
                // make log message
                sclass:=NodeChat.GetElementAttribute(LogChatClass);
                if sclass=LogChatValue then begin
                  utemp:=NodeChat.ElementInnerText;
                  sbuf:=sbuf+utemp;
                  srawbuf:=srawbuf+utemp;
                end
                else
                if Pos(LogChatEmoti,sclass)<>0 then begin
                  skipAddMarkup:=True;
                  sbuf:=sbuf+NodeChat.AsMarkup;
                end else
                  sbuf:=sbuf+NodeChat.AsMarkup;
                NodeChat:=NodeChat.NextSibling;
              end;
              s:=s+sbuf;
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
              if NodeName.GetElementAttribute(LogAlertClass)=LogAlertValue then begin
                NodeChat:=NodeName.FirstChild;
                while Assigned(NodeChat) do begin
                  sclass:=NodeChat.GetElementAttribute(LogAlertClass);
                  if sclass=LogAlertCookie then
                    sbuf:=sbuf+'<< '+NodeChat.ElementInnerText+' >>'
                    else if sclass=LogAlertName then
                      sbuf:=sbuf+' '+NodeChat.ElementInnerText
                      else if sclass=LogAlertMsg then
                        sbuf:=sbuf+' : '+NodeChat.ElementInnerText;
                  NodeChat:=NodeChat.NextSibling;
                end;
                scheck:=Nodex.AsMarkup;
                // websock send alert
                stemp:=pchar(UTF8Encode(scheck));
                i:=Pos('<li',stemp);
                if i<>0 then
                  Insert(LogAddAttr,stemp,i+3);
                WebSockAlert.BroadcastMsg(stemp);
                ChatScript.Add(stemp);
                s:=s+sbuf;
              end else begin
                if RemoveSys and (Pos(LogSysValue,smarkup)<>0) then
                  doAddMsg:=False;
                s:=s+smarkup;
              end;
            end else
              s:=s+smarkup;

            if doAddMsg then begin
              // fill by markup
              if not skipAddMarkup then
                scheck:=Nodex.AsMarkup;

              stemp:=pchar(UTF8Encode(scheck));
              i:=Pos('<li',stemp);
              if i<>0 then
                Insert(LogAddAttr,stemp,i+3);
              WebSockChat.BroadcastMsg(stemp);
              WebSockRChat.BroadcastMsg(UTF8Encode(srawbuf));
              //ssocket:=ssocket+scheck;
              ChatBuffer.Add(stemp);
              // log
              if not disLog then begin
                FormKakaoTVChat.log.AddLog(UTF8Encode(s));
              end;
            end;

            if Nodex=NodeEnd then
              break;
            Nodex:=Nodex.NextSibling;
          end;
          //WebSockChat.BroadcastMsg(pchar(UTF8Encode(ssocket)));

          // set last checksum
          if chkCount>0 then begin
            for i:=0 to chkCount-1 do
              lastchecksum[i]:=bottomchecksum[i];
            lastDupChk:=dupCount;
            lastchkCount:=chkCount;
          end;

          break;

        end;
        ProcessNode(Node);
        Node := Node.NextSibling;
      end;
    end;
  end;

begin
  if TryEnter then begin
    try
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
    finally
      Leave;
    end;
  end;
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
  cefb.OnAddressChange:=@CefAddressChange;
end;

procedure TFormKakaoTVChat.FormDestroy(Sender: TObject);
begin
  ChatScript.Free;
  ChatHead.Free;
  ChatBuffer.Free;
  FEventMain.Free;

  WebSockChat.Free;
  WebSockAlert.Free;
  WebSockRChat.Free;
  Sleep(100);
end;

procedure TFormKakaoTVChat.ButtonStartClick(Sender: TObject);
begin
  Timer1.Enabled:=not Timer1.Enabled;
  if Timer1.Enabled then
    ButtonStart.Caption:='Stop'
    else
      ButtonStart.Caption:='Activate';
end;

procedure TFormKakaoTVChat.ButtonBrowseClick(Sender: TObject);
begin
  cefb.Load(UTF8Decode(EditURL.Text));
end;

procedure TFormKakaoTVChat.ActionPortSetExecute(Sender: TObject);
var
  formPort:TFormPortSet;
  bTimer:Boolean;
begin
  formPort:=TFormPortSet.Create(self);
  try
    formPort.PortHTTP:=PortHttp;
    formPort.PortChat:=PortChat;
    formPort.PortAlert:=PortAlert;
    formPort.PortRChat:=PortRChat;
    formPort.Interval:=cInterval;
    if mrOK=formPort.ShowModal then begin
      PortHttp:=formPort.PortHTTP;
      PortChat:=formPort.PortChat;
      PortAlert:=formPort.PortAlert;
      PortRChat:=formPort.PortRChat;
      cInterval:=formPort.Interval;
      try
        bTimer:=Timer1.Enabled;
        Timer1.Enabled:=False;
        Timer1.Interval:=cInterval;
        HttpServer.Listen(StrToInt(PortHttp));
        HttpServer.Port:=StrToInt(PortHttp);
        WebSockChat.Free;
        WebSockAlert.Free;
        WebSockRChat.Free;
        Sleep(100);
        WebSockChat:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortChat,ChatBuffer);
        WebSockAlert:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortAlert,ChatScript);
        WebSockRChat:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortRChat);
      except
        on e:Exception do begin
          ShowMessage(e.Message);
        end;
      end;
      Timer1.Enabled:=bTimer;
    end;
  finally
    formPort.Free;
  end;
end;

procedure TFormKakaoTVChat.ActionAutoStartExecute(Sender: TObject);
begin
  ActionAutoStart.Checked:=not ActionAutoStart.Checked;
end;

procedure TFormKakaoTVChat.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  config : TIniFile;
begin
  config:=TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try
    config.WriteString('PORT','HTTP',PortHttp);
    config.WriteString('PORT','CHAT',PortChat);
    config.WriteString('PORT','ALERT',PortAlert);
    config.WriteString('PORT','RAWCHAT',PortRChat);
    config.WriteInteger('URL','INT',cInterval);

    config.WriteBool('PARSER','START',ActionAutoStart.Checked);
    config.WriteString('PARSER','LogAttrName',LogAttrName);
    config.WriteString('PARSER','LogAttrValue',LogAttrValue);
    config.WriteString('PARSER','LogChatClass',LogChatClass);
    config.WriteString('PARSER','LogSessionAttr',LogSessionAttr);
    config.WriteString('PARSER','LogChatValue',LogChatValue);
    config.WriteString('PARSER','LogChatEmoti',LogChatEmoti);
    config.WriteString('PARSER','LogAlertClass',LogAlertClass);
    config.WriteString('PARSER','LogAlertValue',LogAlertValue);
    config.WriteString('PARSER','LogAlertCookie',LogAlertCookie);
    config.WriteString('PARSER','LogAlertName',LogAlertName);
    config.WriteString('PARSER','LogAlertMsg',LogAlertMsg);
    config.WriteString('PARSER','LogSysValue',LogSysValue);
    config.WriteString('PARSER','LogAddAttr',LogAddAttr);
  finally
    config.Free
  end;
end;

procedure TFormKakaoTVChat.FormShow(Sender: TObject);
var
  x : TBigFileURIHandler;
  b : TStringObject;
  config : TIniFile;
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

  config:=TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try
    PortHttp:=config.ReadString('PORT','HTTP',PortHttp);
    PortChat:=config.ReadString('PORT','CHAT',PortChat);
    PortAlert:=config.ReadString('PORT','ALERT',PortAlert);
    PortRChat:=config.ReadString('PORT','RAWCHAT',PortRChat);
    cInterval:=config.ReadInteger('URL','INT',300);

    ActionAutoStart.Checked:=config.ReadBool('PARSER','START',ActionAutoStart.Checked);
    LogAttrName:=config.ReadString('PARSER','LogAttrName',LogAttrName);
    LogAttrValue:=config.ReadString('PARSER','LogAttrValue',LogAttrValue);
    LogChatClass:=config.ReadString('PARSER','LogChatClass',LogChatClass);
    LogSessionAttr:=config.ReadString('PARSER','LogSessionAttr',LogSessionAttr);
    LogChatValue:=config.ReadString('PARSER','LogChatValue',LogChatValue);
    LogChatEmoti:=config.ReadString('PARSER','LogChatEmoti',LogChatEmoti);
    LogAlertClass:=config.ReadString('PARSER','LogAlertClass',LogAlertClass);
    LogAlertValue:=config.ReadString('PARSER','LogAlertValue',LogAlertValue);
    LogAlertCookie:=config.ReadString('PARSER','LogAlertCookie',LogAlertCookie);
    LogAlertName:=config.ReadString('PARSER','LogAlertName',LogAlertName);
    LogAlertMsg:=config.ReadString('PARSER','LogAlertMsg',LogAlertMsg);
    LogSysValue:=config.ReadString('PARSER','LogSysValue',LogSysValue);
    LogAddAttr:=' '+config.ReadString('PARSER','LogAddAttr',LogAddAttr)+' ';
  finally
    config.Free
  end;

  Timer1.Interval:=cInterval;
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
  HttpServer.Port:=StrToInt(PortHttp);
  try
    HttpServer.Listen(StrToInt(PortHttp));
    WebSockChat:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortChat,ChatBuffer);
    WebSockAlert:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortAlert,ChatScript);
    WebSockRChat:=TSimpleWebsocketServer.Create('0.0.0.0:'+PortRChat);
  except
    on e:exception do
      ShowMessage(e.Message);
  end;
  if ActionAutoStart.Checked then
    ButtonStart.Click;
end;

procedure TFormKakaoTVChat.Timer1Timer(Sender: TObject);
begin
  cefb.Browser.SendProcessMessage(PID_RENDERER,TCefProcessMessageRef.New('visitdom'));
end;

function TFormKakaoTVChat.TryEnter: Boolean;
begin
  Result:=FEventMain.WaitFor(0)<>wrTimeout;
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

procedure TFormKakaoTVChat.CefAddressChange(Sender: TObject;
  const Browser: ICefBrowser; const Frame: ICefFrame; const url: ustring);
begin
  EditURL.Text:=UTF8Encode(url);
end;

procedure AppExceptProc(Obj : TObject; Addr : CodePointer; FrameCount:Longint; Frame: PCodePointer);
begin
  ShowMessage(Format('%s',[BacktraceStrFunc(Addr)]));
end;

initialization
  CefRenderProcessHandler := TKaKaoRenderProcessHandler.Create;
  //ExceptProc:=@AppExceptProc;

end.

