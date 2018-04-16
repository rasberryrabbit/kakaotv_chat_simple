unit kakaotv_chat_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  cef3types, cef3lib, cef3intf, cef3lcl, cef3ref, cef3api, cef3own, cef3gui,
  lNetComponents, lhttp, lNet, UniqueInstance, loglistfpc, syncobjs;

type

  { TMyRenderProcessHandler }

  TMyRenderProcessHandler=class(TCefRenderProcessHandlerOwn)
    protected
      function OnProcessMessageReceived(const browser: ICefBrowser;
        sourceProcess: TCefProcessId; const message: ICefProcessMessage
        ): Boolean; override;
      procedure OnBrowserCreated(const browser: ICefBrowser); override;
  end;

  { TMyCefDownloadImage }

  TMyCefDownloadImage = class(TCefDownloadImageCallbackOwn)
    protected
      procedure OnDownloadImageFinished(const imageUrl: ustring;
        httpStatusCode: Integer; image: ICefImage); override;
  end;

  { TFormKakaoTVChat }

  TFormKakaoTVChat = class(TForm)
    Button1: TButton;
    CheckBoxClearB: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure Button1Click(Sender: TObject);
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
    procedure CefLoadStateChange(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
  end;

var
  FormKakaoTVChat: TFormKakaoTVChat;

implementation

{$R *.lfm}

uses
  sha1, uChatBuffer, uhttpHandleCEF, lMimeTypes, uRequestHandler;

const
  MaxChecksum = 2;

var
  cefb : TChromium;
  MainBrowser : ICefBrowser;

  lastchecksum : array[0..MaxChecksum] of TSHA1Digest;
  lastchkCount : Integer = 0;

  ChatBuffer:TCefChatBuffer;
  ChatHead:TCefChatBuffer;
  ChatScript:TCefChatBuffer;

  HttpServer:TBigFileLHTTPServerComponent;
  ImgPath:string;


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


const
  imgscript=';(function() {'+
    'var getDataFromImg = function(img) {;'+
    '    var canvas = document.createElement(''canvas'');'+
    '    canvas.setAttribute("id","myimg");'+
    '    var xdiv=document.querySelector("div");'+
    '    document.body.insertBefore(canvas,xdiv);'+
    '    var context = canvas.getContext(''2d'');'+
    '    context.drawImage(img, 0, 0 );'#13#10+
    '    var b64=canvas.toDataURL("image/png");'+
    '    return b64;'+
    '}'+#13#10+
    'var images = document.querySelectorAll(''.thumb_img'');'+
    'var finalArray = [];'+
    'for ( var i=0; i<images.length; i++ )'+
    '{'+
    '    finalArray.push(images[i].currentSrc);'+
    '    finalArray.push(getDataFromImg(images[i]));'+
    '}'+
    'return finalArray;'+
    '})()';


procedure ProcessElementsById(const AFrame: ICefFrame; const AId: string);
var
  Visitor: TElementIdVisitor;
  surl:string;
  retv8, v8:ICefV8Value;
  errv8:ICefV8Exception;
begin
  if Assigned(AFrame) then
  begin
    surl:='<iframe src="'+UTF8Encode(AFrame.Url)+'" ></iframe>';
    if ChatScript.IndexOf(surl)=-1 then
      ChatScript.Add(surl);
    if (0<>Pos('live/chat/',AFrame.GetUrl)) then begin
      //if AFrame.GetV8Context.Eval(imgscript,AFrame.Url,0,retv8,errv8) then begin
      //  if retv8.GetArrayLength>0 then begin
      //
      //  end;
      //end else
      //  FormKakaoTVChat.log.AddLog(Format('> %s %d',[errv8.GetMessage,errv8.LineNumber]));

      Visitor := TElementIdVisitor.Create(AId);
      AFrame.VisitDom(Visitor);
    end;
  end;
end;

{ TMyCefDownloadImage }

procedure TMyCefDownloadImage.OnDownloadImageFinished(const imageUrl: ustring;
  httpStatusCode: Integer; image: ICefImage);
begin
  inherited OnDownloadImageFinished(imageUrl, httpStatusCode, image);
  FormKakaoTVChat.log.AddLogLine(Format('image %d',[httpStatusCode]));
end;


{ TMyRenderProcessHandler }

function TMyRenderProcessHandler.OnProcessMessageReceived(
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
        surl:='<iframe src="'+UTF8Encode(browser.MainFrame.Url)+'" ></iframe>';
        if ChatScript.IndexOf(surl)=-1 then
          ChatScript.Add(surl);
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

procedure TMyRenderProcessHandler.OnBrowserCreated(const browser: ICefBrowser);
begin
  inherited OnBrowserCreated(browser);
  MainBrowser:=browser;
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
    s, smarkup, sclass, sbuf, scheck: UnicodeString;
    checksumN : TSHA1Digest;
    bottomchecksum : array[0..MaxChecksum] of TSHA1Digest;
    chkCount, i: Integer;
    matched : Boolean;
  begin
    if Assigned(ANode) then
    begin
      Node := ANode.FirstChild;
      while Assigned(Node) do begin
        if Node.GetElementAttribute('id')=FNameID then begin
          s:='';
          Nodex:=Node.LastChild;
          chkCount:=0;
          while Assigned(Nodex) do begin
            sbuf:='';
            if Nodex.HasChildren then begin
              NodeName:=Nodex.FirstChild;
              NodeChat:=NodeName.NextSibling;
            end else
              NodeName:=nil;

            // check MaxChecksum+1 bottom lines
            NodeN:=Nodex;
            i:=0;
            matched:=lastchkCount>0;
            while Assigned(NodeN) do begin
              scheck:=document.BaseUrl+NodeN.AsMarkup;
              checksumN:=SHA1Buffer(scheck[1],Length(scheck)*SizeOf(WideChar));

              // check checksum
              if (lastchkCount>0) and (i<lastchkCount) then begin
                if not SHA1Match(checksumN,lastchecksum[i]) then
                  matched:=False;
              end;
              Inc(i);

              // fill bottom checksum
              if chkCount<=MaxChecksum then begin
                bottomchecksum[chkCount]:=checksumN;
                Inc(chkCount);
              end else
                break;

              NodeN:=NodeN.PreviousSibling;
            end;
            if matched then
              break;

            smarkup:=Nodex.AsMarkup;
            if s<>'' then
              s:=sLineBreak+s;
            // get chat message
            if Assigned(NodeName) and Assigned(NodeChat) then begin
              sbuf:=sbuf+NodeName.ElementInnerText+' : ';
              while Assigned(NodeChat) do begin
                if NodeChat.GetElementAttribute('CLASS')='txt_talk' then
                  sbuf:=sbuf+NodeChat.ElementInnerText
                else
                  sbuf:=sbuf+NodeChat.AsMarkup;
                NodeChat:=NodeChat.NextSibling;
              end;
              s:=sbuf+s;
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
                s:=sbuf+s;
              end else
                s:=smarkup+s;
            end else
              s:=smarkup+s;

            // fill by markup
            ChatBuffer.Add(UTF8Encode(Nodex.AsMarkup));

            Nodex:=Nodex.PreviousSibling;
          end;
          // set last checksum
          if chkCount>0 then begin
            for i:=0 to chkCount-1 do
              lastchecksum[i]:=bottomchecksum[i];
            lastchkCount:=chkCount;
          end;

          if s<>'' then begin
            FormKakaoTVChat.log.AddLogLine(UTF8Encode(s));
          end;
        end else
        if (Node.ElementTagName='SCRIPT') {or (Node.ElementTagName='IFRAME')} then begin
          stemp:=UTF8Encode(Node.AsMarkup);
          if ChatScript.IndexOf(stemp)=-1 then
            ChatScript.Add(stemp);
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
      if NodeH.ElementTagName='SCRIPT' then begin
        if ChatScript.IndexOf(stemp)=-1 then
          ChatScript.Add(stemp);
      end else begin
        if ChatHead.IndexOf(stemp)=-1 then
          ChatHead.Add(stemp);
      end;
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
  // temp folder
  ImgPath:=ExtractFilePath(Application.ExeName)+'doc';
  if not DirectoryExists(ImgPath) then
    CreateDir(ImgPath);
  cefb:=TChromium.Create(self);
  cefb.Parent:=Panel1;
  cefb.Align:=alClient;
  cefb.OnLoadStart:=@CefLoadStart;
  cefb.OnLoadingStateChange:=@CefLoadStateChange;
end;

procedure TFormKakaoTVChat.FormDestroy(Sender: TObject);
begin
  ChatScript.Free;
  ChatHead.Free;
  ChatBuffer.Free;
  FEventMain.Free;
end;

procedure TFormKakaoTVChat.Button1Click(Sender: TObject);
begin
  Timer1.Enabled:=not Timer1.Enabled;
  if Timer1.Enabled then
    Button1.Caption:='Stop'
    else
      Button1.Caption:='Activate';
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
  //lastchkCount:=0;
  ChatHead.Clear;
  ChatScript.Clear;
  if CheckBoxClearB.Checked then
    ChatBuffer.Clear;
  log.Font.Name:='Default';
end;

procedure TFormKakaoTVChat.CefLoadStateChange(Sender: TObject;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if not isLoading then begin
  end;
end;


initialization
  CefRenderProcessHandler := TMyRenderProcessHandler.Create;

end.

