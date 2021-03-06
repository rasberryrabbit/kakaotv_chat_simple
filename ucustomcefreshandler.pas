unit ucustomCEFResHandler;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uCEFInterfaces, uCEFTypes, LazUTF8Classes,
  uCEFUrlRequest, uCEFMiscFunctions, uCEFConstants, uCEFResponseFilter,
  Generics.Collections, syncobjs;

type

  { TKakaoResponseFilter }

  TKakaoResponseFilter=class(TCustomResponseFilter)
    protected
      function Filter(data_in: Pointer; data_in_size: NativeUInt;
        var data_in_read: NativeUInt; data_out: Pointer;
        data_out_size: NativeUInt; var data_out_written: NativeUInt
        ): TCefResponseFilterStatus; override;
    public
      rsid:UInt64;
      constructor Create(id:UInt64); overload;
      destructor Destroy; override;
  end;

  procedure CEFCompleteRequest(const Request: ICefRequest);
  procedure RHEnter;
  procedure RHLeave;
  procedure RHClearDict;

var
  cefImageFolder:string='';
  ImagePathCheck:UnicodeString='.png?credential';
  ImageExtPos:UnicodeString='?credential';
  ResourceDict:specialize TObjectDictionary<UInt64,TMemoryStream>;

implementation

uses
  kakaotv_chat_main, uformDebug;

var
  fEvent:TEvent;

{ TKakaoResponseFilter }

function TKakaoResponseFilter.Filter(data_in: Pointer;
  data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer;
  data_out_size: NativeUInt; var data_out_written: NativeUInt
  ): TCefResponseFilterStatus;
var
  ms:TMemoryStream;
begin
  data_in_read:=data_in_size;
  if data_in_read>data_out_size then
    data_out_written:=data_out_size
    else
      data_out_written:=data_in_read;
  system.Move(data_in^,data_out^,data_out_written);

  RHEnter;
  try
    if not ResourceDict.TryGetValue(rsid,ms) then begin
      ms:=TMemoryStream.Create;
      try
        ResourceDict.AddOrSetValue(rsid,ms);
      except
        ms.Free;
        ms:=nil;
      end;
    end;
    if Assigned(ms) then
      ms.Write(data_in^,data_out_written);
  finally
    RHLeave;
  end;

  Result:=Inherited Filter(data_in,data_in_size,data_in_read,
                           data_out,data_out_size,data_out_written);
end;

constructor TKakaoResponseFilter.Create(id: UInt64);
begin
  inherited Create;
  rsid:=id;
end;

destructor TKakaoResponseFilter.Destroy;
begin
  inherited Destroy;
end;

procedure RHEnter;
begin
  while FEvent.WaitFor(0)=wrTimeout do
    Sleep(0);
  FEvent.ResetEvent;
end;

procedure RHLeave;
begin
  FEvent.SetEvent;
end;

procedure RHClearDict;
begin
  RHEnter;
  try
    ResourceDict.Clear;
  finally
    RHLeave;
  end;
end;

procedure CEFCompleteRequest(const Request: ICefRequest);
var
  newName:UnicodeString;
  i, j, l : Integer;
  FFileStream:TFileStreamUTF8;
  Stream:TMemoryStream;
begin
  RHEnter;
  try
  if ResourceDict.TryGetValue(Request.Identifier,Stream) then begin
  if Assigned(Stream) then
    Stream.Position:=0;
  if cefImageFolder<>'' then begin
    // save image files
    newName:=CefUriDecode(Request.Url,False,UU_PATH_SEPARATORS);

    i:=Pos(UnicodeString(ImageExtPos),newName);
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
      if l-i>1 then begin
        newName:=StringReplace(Copy(newName,i,j-i),'/','_',[rfReplaceAll]);
        try
          if not FileExists(UTF8Decode(cefImageFolder)+UnicodeString(PathDelim)+newName) then begin
            FFileStream:=TFileStreamUTF8.Create(cefImageFolder+PathDelim+pchar(UTF8Encode(newName)),fmCreate or fmShareDenyWrite);
            try
               FFileStream.CopyFrom(Stream,Stream.Size);
            finally
              FFileStream.Free;
            end;
          end;
        except
          on e:exception do begin
            FormDebug.logdebug(e.Message);
          end;
        end;
      end;
    end;
  end;
  end;
  finally
    RHLeave;
  end;
end;

initialization
  FEvent:=TEvent.Create(nil,True,True,'RSFILTER'+IntToStr(Random(65535)));
  ResourceDict:=specialize TObjectDictionary<UInt64,TMemoryStream>.Create;

finalization
  ResourceDict.Clear;
  ResourceDict.Free;
  fEvent.SetEvent;
  fEvent.Free;


end.

