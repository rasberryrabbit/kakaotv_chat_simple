unit uKakaoCEF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cef3lcl, cef3intf, cef3own, cef3ref, cef3lib, cef3types;


type

  { TkakaoCEF }

  TkakaoCEF=class(TChromium)
    protected
      function doOnGetResourceHandler(const Browser_: ICefBrowser;
        const Frame: ICefFrame; const request: ICefRequest): ICefResourceHandler;
        override;
      procedure doOnLoadStart(const Browser_: ICefBrowser;
        const Frame: ICefFrame; transitionType: TCefTransitionType); override;
      procedure doOnLoadEnd(const Browser_: ICefBrowser; const Frame: ICefFrame;
        httpStatusCode: Integer); override;
    public
      doOnLoadCompleted:Boolean;
  end;

  { TKakaoResourceHandler }

  TKakaoResourceHandler=class(TCefResourceHandlerOwn)
    protected
      function ProcessRequest(const request: ICefRequest;
        const callback: ICefCallback): Boolean; override;
      procedure GetResponseHeaders(const response: ICefResponse; out
        responseLength: Int64; out redirectUrl: ustring); override;
      function ReadResponse(const dataOut: Pointer; bytesToRead: Integer;
        var bytesRead: Integer; const callback: ICefCallback): Boolean;
        override;
    public
      FOffset:TSize;
      FCallback:ICefCallback;
      FResponse:ICefResponse;
      FStream:TMemoryStream;
      FBrowser:ICefBrowser;

      constructor Create(const browser: ICefBrowser; const frame: ICefFrame;
  const schemeName: ustring; const request: ICefRequest); override;
      destructor Destroy; override;

      procedure WriteResponse(const Request:ICefUrlRequest; Data:Pointer; Size:TSize); virtual;
      procedure CompleteRequest(const Request:ICefUrlRequest); virtual;
  end;

  { TKakaoRequestClient }

  TKakaoRequestClient=class(TCefUrlrequestClientOwn)
      FHandler:TKakaoResourceHandler;
    protected
      procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer;
        dataLength: TSize); override;
      procedure OnRequestComplete(const request: ICefUrlRequest); override;
    public
      constructor CreateHandle(Handler:TKakaoResourceHandler);
  end;

var
  cefImageFolder:string='';

implementation

uses kakaotv_chat_main;

{ TKakaoRequestClient }

procedure TKakaoRequestClient.OnDownloadData(const request: ICefUrlRequest;
  data: Pointer; dataLength: TSize);
begin
  FHandler.WriteResponse(request,data,dataLength);
end;

procedure TKakaoRequestClient.OnRequestComplete(const request: ICefUrlRequest);
begin
  FHandler.CompleteRequest(request);
end;

constructor TKakaoRequestClient.CreateHandle(Handler: TKakaoResourceHandler);
begin
  inherited Create;
  FHandler:=Handler;
end;

{ TKakaoResourceHandler }

function TKakaoResourceHandler.ProcessRequest(const request: ICefRequest;
  const callback: ICefCallback): Boolean;
begin
  // with browser
  if Assigned(FBrowser) and Assigned(FBrowser.Host) then begin
    try
      FOffset:=0;
      FCallback:=callback;
      FResponse:=nil;
      TCefUrlRequestRef.New(request,TKakaoRequestClient.CreateHandle(Self),FBrowser.Host.GetRequestContext);
      Result:=True;
    except
      Result:=False;
    end;
  end else
      Result:=False;
end;

procedure TKakaoResourceHandler.GetResponseHeaders(
  const response: ICefResponse; out responseLength: Int64; out
  redirectUrl: ustring);
begin
  if Assigned(FStream) then
    responseLength:=FStream.Size;
  if Assigned(FResponse) then begin
    response.Status:=FResponse.Status;
    response.StatusText:=FResponse.StatusText;
    response.MimeType:=FResponse.MimeType;
  end;
  inherited GetResponseHeaders(response, responseLength, redirectUrl);
end;

function TKakaoResourceHandler.ReadResponse(const dataOut: Pointer;
  bytesToRead: Integer; var bytesRead: Integer; const callback: ICefCallback
  ): Boolean;
begin
  if Assigned(FStream) and (FOffset<FStream.Size) then begin
    Result:=True;
    bytesRead:=bytesToRead;
    Move(Pointer(TSize(FStream.Memory)+FOffset)^,dataOut^,bytesRead);
    Inc(FOffset,bytesRead);
  end else
    Result:=False;
end;

constructor TKakaoResourceHandler.Create(const browser: ICefBrowser;
  const frame: ICefFrame; const schemeName: ustring; const request: ICefRequest
  );
begin
  inherited Create(browser, frame, schemeName, request);
  FStream:=TMemoryStream.Create;
  FBrowser:=browser;
end;

destructor TKakaoResourceHandler.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TKakaoResourceHandler.WriteResponse(const Request: ICefUrlRequest;
  Data: Pointer; Size: TSize);
begin
  if Assigned(FStream) then
    FStream.Write(Data^,Size);
end;

procedure TKakaoResourceHandler.CompleteRequest(const Request: ICefUrlRequest);
var
  newName:string;
  i, j, l : Integer;
begin
  if Assigned(FStream) then
    FStream.Position:=0;
  FResponse:=Request.GetResponse;
  if cefImageFolder<>'' then begin
    // save image files
    newName:=pchar(UTF8Encode(CefUriDecode(Request.GetRequest.Url,False,[UU_PATH_SEPARATORS])));

    i:=Pos('?',newName);
    if i<>0 then begin
      j:=i;
      while i>0 do begin
        if newName[i]='/' then
          break;
        Dec(i);
      end;
      l:=i;
      if i>0 then begin
        Dec(i);
        while i>0 do begin
          if newName[i]='/' then
            break;
          Dec(i);
        end;
      end;
      Inc(i);
      if l-i>0 then begin
        newName:=StringReplace(Copy(newName,i,j-i),'/','_',[rfReplaceAll]);
        try
          if not FileExists(cefImageFolder+PathDelim+newName) then
            FStream.SaveToFile(cefImageFolder+PathDelim+newName);
        except
          on e:exception do begin
            FormKakaoTVChat.log.AddLog(e.Message);
          end;
        end;
      end;
    end;
  end;
  if Assigned(FCallback) then
    FCallback.Cont;
end;

{ TkakaoCEF }

function TkakaoCEF.doOnGetResourceHandler(const Browser_: ICefBrowser;
  const Frame: ICefFrame; const request: ICefRequest): ICefResourceHandler;
begin
  Result:=inherited doOnGetResourceHandler(Browser_,Frame,request);
  if doOnLoadCompleted
    and (not Assigned(Result))
    and Assigned(Browser_)
    and (request.GetResourceType=RT_IMAGE)
    and (Pos('/dna/emoticons/',request.Url)<>0) then
    Result:=TKakaoResourceHandler.Create(Browser_,Frame,'KakaoImage',request);
end;

procedure TkakaoCEF.doOnLoadStart(const Browser_: ICefBrowser;
  const Frame: ICefFrame; transitionType: TCefTransitionType);
begin
  inherited doOnLoadStart(Browser_, Frame, transitionType);
  doOnLoadCompleted:=False;
end;

procedure TkakaoCEF.doOnLoadEnd(const Browser_: ICefBrowser;
  const Frame: ICefFrame; httpStatusCode: Integer);
begin
  inherited doOnLoadEnd(Browser_, Frame, httpStatusCode);
  doOnLoadCompleted:=True;
end;

initialization
  cefImageFolder:='';


end.

