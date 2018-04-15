unit uRequestHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cef3own, cef3intf, cef3types;

type


  { TMyClient }

  TMyClient=class(TCefClientOwn)
    protected
      function GetRequestHandler: ICefRequestHandler; override;
  end;

  { TMyRequestHandler }

  TMyRequestHandler = class(TCefRequestHandlerOwn)
    protected
      function OnBeforeResourceLoad(const browser: ICefBrowser;
        const frame: ICefFrame; const request: ICefRequest;
        const callback: ICefRequestCallback): TCefReturnValue; override;
  end;

implementation

{ TMyClient }

function TMyClient.GetRequestHandler: ICefRequestHandler;
begin
  Result:=inherited GetRequestHandler;
end;

{ TMyRequestHandler }

function TMyRequestHandler.OnBeforeResourceLoad(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const callback: ICefRequestCallback): TCefReturnValue;
begin
  Result:=inherited OnBeforeResourceLoad(browser, frame, request, callback);
end;

end.

