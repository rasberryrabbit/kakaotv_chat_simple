unit kakaotv_chat_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ActnList, lNetComponents, lhttp, lNet, UniqueInstance,
  uCEFWindowParent, uCEFChromiumWindow, uCEFChromium, loglistfpc, syncobjs,
  uCEFInterfaces, uCEFConstants, Messages, uCEFDomVisitor, uCEFTypes,
  uCEFChromiumEvents, uCEFProcessMessage, uCEFUrlRequestClientComponent;

const
  WM_CEFMsg = WM_USER+$100;

type

  { TFormKakaoTVChat }

  TFormKakaoTVChat = class(TForm)
    ActionAutoSurf: TAction;
    ActionDoImgLog: TAction;
    ActionAutoStart: TAction;
    ActionPortSet: TAction;
    ActionList1: TActionList;
    ButtonStart: TButton;
    ButtonBrowse: TButton;
    CEFWindowParent1: TCEFWindowParent;
    CheckBoxRemSyS: TCheckBox;
    CheckBoxDisableLog: TCheckBox;
    CheckBoxClearB: TCheckBox;
    Chromium1: TChromium;
    EditURL: TEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel2: TPanel;
    Timer1: TTimer;
    TimerChrome: TTimer;
    TimerSurf: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure ActionAutoStartExecute(Sender: TObject);
    procedure ActionAutoSurfExecute(Sender: TObject);
    procedure ActionDoImgLogExecute(Sender: TObject);
    procedure ActionPortSetExecute(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonBrowseClick(Sender: TObject);
    procedure Chromium1AddressChange(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser
      );
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser;
      var aAction: TCefCloseBrowserAction);
    procedure Chromium1GetResourceResponseFilter(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse; out
      Result: ICefResponseFilter);
    procedure Chromium1LoadError(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; errorCode: TCefErrorCode; const errorText,
      failedUrl: ustring);
    procedure Chromium1LoadStart(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; transitionType: TCefTransitionType);
    procedure Chromium1ResourceLoadComplete(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse;
      status: TCefUrlRequestStatus; receivedContentLength: Int64);
    // chrome
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TimerChromeTimer(Sender: TObject);
    procedure TimerSurfTimer(Sender: TObject);
  private
    FEventMain:TEvent;
    FClosing:Boolean;
    FCanClose:Boolean;
    // chrome
    procedure BrowserCreateMsg(var aMsg:TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMsg:TMessage); message CEF_DESTROY;
    procedure WMMove(var aMsg:TMessage); message WM_MOVE;
    procedure WMMoving(var aMsg:TMessage); message WM_MOVING;
    procedure WMCEFMsg(var aMsg:TMessage); message WM_CEFMsg;
  public
    log:TLogListFPC;

    procedure SaveSettings;
    function TryEnter:Boolean;
    procedure Leave;

    procedure HttpError(const msg: string; aSocket: TLSocket);

  end;

  procedure CreateGlobalCEFApp;

var
  FormKakaoTVChat: TFormKakaoTVChat;

implementation

{$R *.lfm}

uses
  Windows, uChatBuffer, uhttpHandleCEF, lMimeTypes, uWebsockSimple,
  form_portset, IniFiles, Hash, uhashimpl, DefaultTranslator,
  StrUtils, uformDebug, uStringHashList, ucustomCEFResHandler,
  uCEFApplication, uCEFSchemeRegistrar;

const
  MaxChecksum = 11; // odd length
  csclass = 'CLASS';

type
  THashChkArray = array[0..MaxChecksum] of THashDigest;
  TDupChkArray = array[0..MaxChecksum] of Integer;

var
  lastchecksum : THashChkArray;
  lastchkCount : Integer = 0;
  lastDupChk : TDupChkArray;

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

  LogChatClass : UnicodeString = csclass;
  LogChatID : UnicodeString = 'link_id';
  LogSessionAttr : UnicodeString = 'data-sessionid';
  LogChatValue : UnicodeString = 'txt_talk';
  LogChatEmoti : UnicodeString = 'kakao_emoticon';
  ImgPathHeader: UnicodeString = '//mk.';

  LogAlertClass : UnicodeString = csclass;
  LogAlertValue : UnicodeString = 'box_alert';
  LogAlertCookie: UnicodeString = 'txt_cookie';
  LogAlertName  : UnicodeString = 'txt_name';
  LogAlertMsg   : UnicodeString = 'txt_msg';
  LogSysValue   : UnicodeString = 'txt_system';
  LogKnownClass : TFPStringHashTableList;

  LogAddAttr    : string = ' class="kakao_chat" ';

  alivelink : UnicodeString = '';

  skipchecksum : THashDigest;


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

  { TLiveResultParser }

  TLiveResultParser = class(TCefDomVisitorOwn)
  private
  protected
    procedure Visit(const document: ICefDomDocument); override;
  public
  end;


function ProcessElementsById(const AFrame: ICefFrame; const AId: string):Boolean;
var
  Visitor: TElementIdVisitor;
  surl:string;
  //retv8, v8:ICefV8Value;
  //errv8:ICefV8Exception;
begin
  Result:=False;
  if Assigned(AFrame) then
  begin
    {
    surl:='<iframe src="'+UTF8Encode(AFrame.Url)+'" ></iframe>';
    if ChatScript.IndexOf(surl)=-1 then
      ChatScript.Add(surl);
    }
    if (0<>Pos('live/chat/',AFrame.GetUrl)) then begin
      Result:=True;
      Visitor := TElementIdVisitor.Create(AId);
      AFrame.VisitDom(Visitor);
    end;
  end;
end;

procedure ProcessLiveResult(const AFrame: ICefFrame);
var
  Visitor: TLiveResultParser;
begin
  if Assigned(AFrame) then
  begin
    Visitor := TLiveResultParser.Create;
    AFrame.VisitDom(Visitor);
  end;
end;

procedure OnCEFProcessMsg(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; var aHandled : boolean);
var
  chatframe:ICefFrame;
  fcount, i:NativeUInt;
  fid:array of int64;
  surl:string;
begin
  aHandled:=False;
  if message.Name='visitdom' then begin
    { thread-safe? }
    if FormKakaoTVChat.TryEnter then begin
      try
        fcount:=browser.GetFrameCount;
        SetLength(fid,fcount);
        try
          browser.GetFrameIdentifiers(fcount,fid);
          for i:=0 to fcount-1 do begin
            chatframe:=browser.GetFrameByident(fid[i]);
            if (not ProcessElementsById(chatframe,LogAttrValue)) and
               FormKakaoTVChat.ActionAutoSurf.Checked then
               // auto surf url
               ProcessLiveResult(chatframe);
          end;
        finally
          SetLength(fid,0);
        end;
      finally
        FormKakaoTVChat.Leave;
      end;
    end;
    aHandled:=True;
  end;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                  := TCefApplication.Create;
  //GlobalCEFApp.RemoteDebuggingPort:=9000;
  //GlobalCEFApp.LogFile          := 'cef.log';
  //GlobalCEFApp.LogSeverity      := LOGSEVERITY_VERBOSE;
  //GlobalCEFApp.WindowlessRenderingEnabled:=True;
  GlobalCEFApp.OnProcessMessageReceived:=@OnCEFProcessMsg;
  GlobalCEFApp.IgnoreCertificateErrors:=True;
  GlobalCEFApp.SingleProcess:=True;
end;

{ TLiveResultParser }

procedure TLiveResultParser.Visit(const document: ICefDomDocument);
  procedure ProcessChildNode(ANode:ICefDomNode);
  var
    Node, NodeA:ICefDomNode;
    livelink:UnicodeString;
  begin
    if alivelink<>'' then
      exit;
    if Assigned(ANode) then begin
      Node:=ANode.FirstChild;
      while Assigned(Node) do begin
        if Assigned(Node) then begin
          if (Node.GetElementTagName='DIV') and
             (Node.GetElementAttribute('CLASS')='inner_videoitem') then
          begin
             NodeA:=Node.FirstChild;
             while Assigned(NodeA) do begin
               if NodeA.GetElementAttribute('CLASS')='link_itembox' then
               begin
                 livelink:=NodeA.GetElementAttribute('HREF');
                 if Pos('/livelink/',livelink)<>0 then begin
                   alivelink:=UnicodeString('https://tv.kakao.com')+livelink;
                   FormDebug.logdebug(UTF8Encode(livelink));
                   break;
                 end;
               end;
               NodeA:=NodeA.NextSibling;
             end;
          end;
          ProcessChildNode(Node);
          Node:=Node.NextSibling;
        end;
      end;
    end;
  end;
begin
  if Assigned(document) then begin
    ProcessChildNode(document.Body);
    if FormKakaoTVChat.ActionAutoSurf.Checked and (alivelink<>'') then
      FormKakaoTVChat.TimerSurf.Enabled:=True;
  end;
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


function IsKnownClass(const s:UnicodeString):Boolean;
begin
  Result:=LogKnownClass.Find(pchar(UTF8Encode(s)))<>nil;
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
    bottomchecksum : THashChkArray;
    dupCount, dupCountChk : TDupChkArray;
    chkCount, i, j, k, l, ItemCount : Integer;
    matched, skipAddMarkup, disLog, RemoveSys, doAddMsg, IsUnknown, SysBreak : Boolean;
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
              // <li>
              //   <span...></span>
              //   <div...></div>
              // </li>
              // checksum
              scheck:='';
              SysBreak:=False;
              // check known patterns, chat + cookie alert
              // link_id, box_alert
              IsUnknown:=True;
              NodeName:=NodeN.FirstChild;
              while Assigned(NodeName) do begin
                sclass:=NodeName.GetElementAttribute(csclass);
                if (Pos(LogChatID,sclass)>0) or
                   (Pos(LogSysValue,sclass)>0) or
                   (Pos(LogAlertValue,sclass)>0)  then
                  IsUnknown:=False;
                if Pos(LogSysValue,sclass)>0 then
                  SysBreak:=True;
                scheck:=scheck+Copy(NodeName.ElementInnerText,1,255);
                NodeName:=NodeName.NextSibling;
              end;

              if not IsUnknown then begin
                if SysBreak then
                  checksumN:=skipchecksum
                  else
                    checksumN:=MakeHash(@scheck[1],Length(scheck)*SizeOf(WideChar));
                // check, skip at sys msg
                if matched then begin
                  if (i<lastchkCount) then begin
                    if SysBreak or
                       CompareHash(checksumN,lastchecksum[i]) then begin
                      Dec(dupCountChk[i]);
                      if dupCountChk[i]=0 then
                        Inc(i);
                    end else
                      matched:=False;
                  end;
                end;
                //FormDebug.logdebug(Format('>> %d %d',[i,lastchkCount]));
              end;

              // fill bottom checksum, skip sys msg
              if chkCount<MaxChecksum then begin
                if not IsUnknown then begin
                  // find dup check on last checksum
                  if (chkCount>0) and CompareHash(checksumN,bottomchecksum[chkCount-1]) then
                    Inc(dupCount[chkCount-1])
                  else begin
                  // find new checksum
                    bottomchecksum[chkCount]:=checksumN;
                    dupCount[chkCount]:=1;
                    Inc(chkCount);
                    //FormDebug.logdebug(scheck);
                  end;
                end;
              end else
                // full new checksum buffers and ending checksum comparing
                if (i>=lastchkCount) or (not matched) then
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
                if Pos(LogChatValue,sclass)<>0 then begin
                  utemp:=NodeChat.ElementInnerText;
                  sbuf:=sbuf+utemp;
                  srawbuf:=srawbuf+utemp;
                end
                else
                if Pos(LogChatEmoti,sclass)<>0 then begin
                  skipAddMarkup:=True;
                  sbuf:=sbuf+NodeChat.ElementInnerText;
                  //FormDebug.logdebug(UTF8Encode(NodeChat.AsMarkup));
                end else
                  sbuf:=sbuf+NodeChat.AsMarkup;
                NodeChat:=NodeChat.NextSibling;
              end;
              s:=s+sbuf;
              // change img url
              if skipAddMarkup then begin
                scheck:=Nodex.AsMarkup;
                k:=1;
                while k>0 do begin
                  // find image tag position
                  k:=PosEx(UnicodeString('<img'),scheck,k);
                  if(k>0) then begin
                    // find patameter position
                    i:=PosEx(UnicodeString(ImageExtPos),scheck,k);
                    j:=i;
                    // emoticon sub id + id
                    while i>0 do begin
                      if scheck[i]='/' then
                        break;
                      Dec(i);
                    end;
                    l:=i;
                    if i>0 then begin
                      Dec(i);
                      while i>0 do begin
                        if scheck[i]='/' then
                          break;
                        Dec(i);
                      end;
                    end;
                    Inc(i);
                    if (i>k) and (l-i>1) then begin
                      sclass:=UnicodeStringReplace(Copy(scheck,i,j-i),'/','_',[rfReplaceAll]);
                      // find img src header loc
                      i:=PosEx(ImgPathHeader,scheck,k);
                      if i<>0 then
                        scheck:=Copy(scheck,1,i-1)+UnicodeString('img/')+sclass+Copy(scheck,j);
                    end;
                    Inc(k);
                    //FormDebug.logdebug('(image) '+pchar(UTF8Encode(Copy(scheck,k,j-k))));
                  end;
                end;
              end;
            end else if Assigned(NodeName) then begin
              // cookie alert
              if Pos(LogAlertValue, NodeName.GetElementAttribute(LogAlertClass))<>0 then begin
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
            dupCount[chkCount]:=0;
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
const
  dummy:string = 'SKIP';
begin
  FClosing:=False;
  FCanClose:=False;

  //skip checksum
  skipchecksum:=MakeHash(@dummy[1],Length(dummy));

  //IsMultiThread:=True;
  ChatBuffer:=TCefChatBuffer.Create;
  ChatHead:=TCefChatBuffer.Create;
  ChatScript:=TCefChatBuffer.Create;
  LogKnownClass:=TFPStringHashTableList.Create;
  log:=TLogListFPC.Create(self);
  log.Parent:=Panel2;
  log.Align:=alClient;
  FEventMain:=TEvent.Create(nil,True,True,'KAKAOMAIN'+IntToStr(Random(65535)));

  // doc folder
  ImgPath:=ExtractFilePath(Application.ExeName)+'doc';
  if not DirectoryExists(ImgPath) then
    CreateDir(ImgPath);
  // image fodler
  cefImageFolder:=ImgPath+PathDelim+'img';
  if not DirectoryExists(cefImageFolder) then
    CreateDir(cefImageFolder);
end;

procedure TFormKakaoTVChat.FormDestroy(Sender: TObject);
begin
  //
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
  Chromium1.LoadURL(UTF8Decode(EditURL.Text));
end;

procedure TFormKakaoTVChat.Chromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  if url<>'about:blank' then
    EditURL.Text:=UTF8Encode(url);
end;

procedure TFormKakaoTVChat.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TFormKakaoTVChat.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
  try
    SaveSettings;
  except
  end;
  // prevent infinite loop on terminate, but crash.
  Application.ProcessMessages;
end;

procedure TFormKakaoTVChat.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  aAction := cbaDelay;
  PostMessage(Handle, CEF_DESTROY, 0, 0);
end;

procedure TFormKakaoTVChat.Chromium1GetResourceResponseFilter(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse; out
  Result: ICefResponseFilter);
begin
  if Pos(ImagePathCheck,request.Url)<>0 then
    Result:=TKakaoResponseFilter.Create(request.Identifier);
end;

procedure TFormKakaoTVChat.Chromium1LoadError(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode;
  const errorText, failedUrl: ustring);
var
  errorstr:string;
begin
  case errorCode of
  ERR_ABORTED: errorstr:='Aborted';
  ERR_ACCESS_DENIED: errorstr:='Access denied';
  ERR_ADDRESS_INVALID: errorstr:='Invalid Address';
  ERR_ADDRESS_UNREACHABLE: errorstr:='Address unreachable';
  ERR_INVALID_URL: errorstr:='Invalid URL';
  ERR_NAME_NOT_RESOLVED: errorstr:='Name not resolved';
  else
    errorstr:='error';
  end;
  FormDebug.logdebug(Format('%s %s, %d, %s',[errorText,failedUrl,errorCode,errorstr]));
end;

procedure TFormKakaoTVChat.Chromium1LoadStart(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  transitionType: TCefTransitionType);
begin
  lastchkCount:=0;
  alivelink:='';
  if TryEnter then begin
    try
      ChatHead.Clear;
      //ChatScript.Clear;
      if CheckBoxClearB.Checked then
        ChatBuffer.Clear;
      //log.Font.Name:='Default';

      RHClearDict;
    finally
      Leave;
    end;
  end;
end;

procedure TFormKakaoTVChat.Chromium1ResourceLoadComplete(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse;
  status: TCefUrlRequestStatus; receivedContentLength: Int64);
begin
  if Pos(ImagePathCheck,request.Url)<>0 then begin
    try
      CEFCompleteRequest(request);
    except
    end;
    //FormDebug.log.AddLog(request.Url);
  end;
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

procedure TFormKakaoTVChat.ActionAutoSurfExecute(Sender: TObject);
begin
  ActionAutoSurf.Checked:=not ActionAutoSurf.Checked;
end;

procedure TFormKakaoTVChat.ActionDoImgLogExecute(Sender: TObject);
begin
  FormDebug.Show;
end;

procedure TFormKakaoTVChat.FormCloseQuery(Sender: TObject; var CanClose: Boolean
  );
begin
  Timer1.Enabled:=False;
  CanClose:=FCanClose;
  if not FClosing then begin
    FClosing:=True;
    Visible:=False;
    //
    FEventMain.Free;
    LogKnownClass.Free;
    ChatScript.Free;
    ChatHead.Free;
    ChatBuffer.Free;

    WebSockChat.Free;
    WebSockAlert.Free;
    WebSockRChat.Free;
    //
    Chromium1.CloseBrowser(True);
  end;
end;

procedure TFormKakaoTVChat.FormShow(Sender: TObject);
var
  x : TBigFileURIHandler;
  b : TStringObject;
  config : TIniFile;
begin
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
    ActionAutoSurf.Checked:=config.ReadBool('PARSER','AUTOSURF',ActionAutoSurf.Checked);
    LogAttrName:=config.ReadString('PARSER','LogAttrName',LogAttrName);
    LogAttrValue:=config.ReadString('PARSER','LogAttrValue',LogAttrValue);
    LogChatClass:=config.ReadString('PARSER','LogChatClass',LogChatClass);
    LogSessionAttr:=config.ReadString('PARSER','LogSessionAttr',LogSessionAttr);
    LogChatValue:=config.ReadString('PARSER','LogChatValue',LogChatValue);
    LogChatEmoti:=config.ReadString('PARSER','LogChatEmoti',LogChatEmoti);
    ImgPathHeader:=config.ReadString('PARSER','ImgPathHeader',ImgPathHeader);
    ImagePathCheck:=config.ReadString('PARSER','ImagePathCheckN',ImagePathCheck);
    ImageExtPos:=config.ReadString('PARSER','ImageExtPos',ImageExtPos);
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

    if ParamCount>1 then
       EditURL.Text:=ParamStr(1)  // auto surf
       else
         EditURL.Text:=UTF8Decode('https://tv.kakao.com');

    if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then
      TimerChrome.Enabled := True;
  except
    on e:exception do
      ShowMessage(e.Message);
  end;

  if ActionAutoStart.Checked then
    ButtonStart.Click;
end;

procedure TFormKakaoTVChat.Timer1Timer(Sender: TObject);
begin
  PostMessage(Handle,WM_CEFMsg,0,0);
end;

procedure TFormKakaoTVChat.TimerChromeTimer(Sender: TObject);
begin
  TimerChrome.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    TimerChrome.Enabled := True;

  if Chromium1.Initialized then
    Chromium1.LoadURL(EditUrl.Text);
end;

procedure TFormKakaoTVChat.TimerSurfTimer(Sender: TObject);
begin
  if alivelink<>'' then
    if Chromium1.Initialized then
      Chromium1.LoadURL(alivelink)
      else
        TimerSurf.Enabled:=False;
end;

procedure TFormKakaoTVChat.BrowserCreateMsg(var aMsg: TMessage);
begin
  CEFWindowParent1.UpdateSize;
end;

procedure TFormKakaoTVChat.BrowserDestroyMsg(var aMsg: TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TFormKakaoTVChat.WMMove(var aMsg: TMessage);
begin
  inherited;
  if Chromium1<>nil then
     Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TFormKakaoTVChat.WMMoving(var aMsg: TMessage);
begin
  inherited;
  if Chromium1<>nil then
     Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TFormKakaoTVChat.WMCEFMsg(var aMsg: TMessage);
var
  i,j:NativeUInt;
  fid:TCefFrameIdentifierArray;
  imsg:ICefProcessMessage;
begin
  if (Chromium1<>nil) and Chromium1.Initialized then begin
    // chrome
    imsg:=TCefProcessMessageRef.New('visitdom');
    Chromium1.SendProcessMessage(PID_RENDERER,imsg);
  end;
end;

function TFormKakaoTVChat.TryEnter: Boolean;
begin
  Result:=FEventMain.WaitFor(0)<>wrTimeout;
  if Result then
    FEventMain.ResetEvent;
end;

procedure TFormKakaoTVChat.SaveSettings;
var
  config: TIniFile;
begin
  config:=TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    config.WriteString('PORT', 'HTTP', PortHttp);
    config.WriteString('PORT', 'CHAT', PortChat);
    config.WriteString('PORT', 'ALERT', PortAlert);
    config.WriteString('PORT', 'RAWCHAT', PortRChat);
    config.WriteInteger('URL', 'INT', cInterval);

    config.WriteBool('PARSER', 'START', ActionAutoStart.Checked);
    config.WriteBool('PARSER', 'AUTOSURF', ActionAutoSurf.Checked);
    config.WriteString('PARSER', 'LogAttrName', LogAttrName);
    config.WriteString('PARSER', 'LogAttrValue', LogAttrValue);
    config.WriteString('PARSER', 'LogChatClass', LogChatClass);
    config.WriteString('PARSER', 'LogSessionAttr', LogSessionAttr);
    config.WriteString('PARSER', 'LogChatValue', LogChatValue);
    config.WriteString('PARSER', 'LogChatEmoti', LogChatEmoti);
    config.WriteString('PARSER', 'ImgPathHeader', ImgPathHeader);
    config.WriteString('PARSER', 'ImagePathCheckN', ImagePathCheck);
    config.WriteString('PARSER', 'ImageExtPos', ImageExtPos);
    config.WriteString('PARSER', 'LogAlertClass', LogAlertClass);
    config.WriteString('PARSER', 'LogAlertValue', LogAlertValue);
    config.WriteString('PARSER', 'LogAlertCookie', LogAlertCookie);
    config.WriteString('PARSER', 'LogAlertName', LogAlertName);
    config.WriteString('PARSER', 'LogAlertMsg', LogAlertMsg);
    config.WriteString('PARSER', 'LogSysValue', LogSysValue);
    config.WriteString('PARSER', 'LogAddAttr', LogAddAttr);
  finally
    config.Free
  end;
end;

procedure TFormKakaoTVChat.Leave;
begin
  FEventMain.SetEvent;
end;

procedure TFormKakaoTVChat.HttpError(const msg: string; aSocket: TLSocket);
begin
  log.AddLogLine(msg);
end;

procedure AppExceptProc(Obj : TObject; Addr : CodePointer; FrameCount:Longint; Frame: PCodePointer);
begin
  ShowMessage(Format('%s',[BacktraceStrFunc(Addr)]));
end;

initialization
  //ExceptProc:=@AppExceptProc;

end.

