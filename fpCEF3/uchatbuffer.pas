unit uChatBuffer;

  { Charbuffer - basic Threadsafe stringlist

  Copyright (c) 2018 rasberryrabbit

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

type

  { TCefChatTime }

  TCefChatTime = class
    public
      TimeCount:Int64;
  end;


  { TCefChatBuffer }

  TCefChatBuffer = class(TStringList)
    private
      FEvent:TEvent;
      FMaxLines:Integer;
      FChatCount:int64;
      function GetTimestamp(Index:Integer):int64;
    protected
      function GetCount: Integer; override;
      procedure InsertItem(Index: Integer; const S: string; O: TObject);
        override;
      function Get(Index: Integer): string; override;
      procedure Put(Index: Integer; const S: string); override;
      function GetText: PChar; override;
      procedure SetText(TheText: PChar); override;
      function GetObject(Index: Integer): TObject; override;
      procedure PutObject(Index: Integer; AObject: TObject); override;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Enter;
      procedure Leave;

      function Add(const S: string): Integer; override;
      procedure Delete(Index: Integer); override;
      procedure Clear; override;
      function GetLines(var etime: int64): string;

      property MaxLines:Integer read FMaxLines write FMaxLines;
      property TimeCount[Index:Integer]:int64 read GetTimestamp;
  end;

implementation

const
  DefaultChatLinesMax = 300;

{ TCefChatBuffer }

function TCefChatBuffer.GetTimestamp(Index: Integer): int64;
var
  tm:TCefChatTime;
begin
  tm:=TCefChatTime(Objects[Index]);
  if Assigned(tm) then
    Result:=tm.TimeCount
    else
      Result:=0;
end;

function TCefChatBuffer.GetCount: Integer;
begin
  Enter;
  try
    Result:=inherited GetCount;
  finally
    Leave;
  end;
end;

procedure TCefChatBuffer.InsertItem(Index: Integer; const S: string; O: TObject
  );
begin
  Enter;
  try
    inherited InsertItem(Index, S, O);
  finally
    Leave;
  end;
end;

function TCefChatBuffer.Get(Index: Integer): string;
begin
  Enter;
  try
    Result:=inherited Get(Index);
  finally
    Leave;
  end;
end;

procedure TCefChatBuffer.Put(Index: Integer; const S: string);
begin
  Enter;
  try
    inherited Put(Index, S);
  finally
    Leave;
  end;
end;

function TCefChatBuffer.GetText: PChar;
begin
  Enter;
  try
    Result:=inherited GetText;
  finally
    Leave;
  end;
end;

procedure TCefChatBuffer.SetText(TheText: PChar);
begin
  Enter;
  try
    inherited SetText(TheText);
  finally
    Leave;
  end;
end;

function TCefChatBuffer.GetObject(Index: Integer): TObject;
begin
  Enter;
  try
    Result:=inherited GetObject(Index);
  finally
    Leave;
  end;
end;

procedure TCefChatBuffer.PutObject(Index: Integer; AObject: TObject);
begin
  Enter;
  try
    inherited PutObject(Index, AObject);
  finally
    Leave;
  end;
end;

constructor TCefChatBuffer.Create;
begin
  inherited Create;
  OwnsObjects:=True;
  FMaxLines:=DefaultChatLinesMax;
  FChatCount:=0;
  FEvent:=TEvent.Create(nil,True,True,'CEFCHATBUF'+IntToStr(GetTickCount));
end;

destructor TCefChatBuffer.Destroy;
begin
  FEvent.Free;
  inherited Destroy;
end;

procedure TCefChatBuffer.Enter;
begin
  while FEvent.WaitFor(0)=wrTimeout do
    Sleep(0);
  FEvent.ResetEvent;
end;

procedure TCefChatBuffer.Leave;
begin
  FEvent.SetEvent;
end;

function TCefChatBuffer.Add(const S: string): Integer;

begin
  Result:=inherited Add(S);
  Objects[Result]:=TCefChatTime.Create;
  Inc(FChatCount);
  TCefChatTime(Objects[Result]).TimeCount:=FChatCount;
  while Count>MaxLines do
    Delete(0);
end;

procedure TCefChatBuffer.Delete(Index: Integer);
begin
  Enter;
  try
    inherited Delete(Index);
  finally
    Leave;
  end;
end;

procedure TCefChatBuffer.Clear;
begin
  Enter;
  try
    inherited Clear;
  finally
    Leave;
  end;
end;

function TCefChatBuffer.GetLines(var etime: int64): string;
var
  l:Integer;
  ltime,ctime:int64;
begin
  Result:='';
  Enter;
  try
    l:=inherited GetCount;
    if l>0 then begin
      ltime:=TCefChatTime(inherited GetObject(l-1)).TimeCount;
      while l>0 do begin
        Dec(l);
        ctime:=TCefChatTime(inherited GetObject(l)).TimeCount;
        if ctime>etime then
          Result:=pchar(inherited Get(l))+#13#10+Result
          else
            break;
      end;
      etime:=ltime;
    end;
  finally
    Leave;
  end;
end;


end.

