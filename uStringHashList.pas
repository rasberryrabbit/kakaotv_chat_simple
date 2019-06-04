unit uStringHashList;


interface

uses
  Classes, Contnrs;

type
  TFPStringHashTableList = class(TFPStringHashTable)
    private
      fText:string;
      function GetText: string;
      procedure SetText(const Value:string);
      procedure _IterStr(Item: String; const Key: string; var Continue: Boolean);
    protected
    public
      property Text:string read GetText write SetText;
  end;


implementation


{ TFPStringHashTableList }

function TFPStringHashTableList.GetText: string;
var
  temp: THTStringNode;
begin
  fText:='';
  ForEachCall(@_IterStr);
  Result:=fText;
end;

procedure TFPStringHashTableList.SetText(const Value: string);
var
  temp : TStringList;
  i : Integer;
begin
  Clear;
  temp:=TStringList.Create;
  try
    temp.Delimiter:=',';
    temp.DelimitedText:=Value;
    for i:=0 to temp.Count-1 do
      try
        Add(temp.Strings[i],'1');
      except
      end;
  finally
    temp.Free;
  end;
end;

procedure TFPStringHashTableList._IterStr(Item: String; const Key: string;
  var Continue: Boolean);
begin
  if fText<>'' then
    fText:=fText+',';
  fText:=fText+Key;
end;


end.
