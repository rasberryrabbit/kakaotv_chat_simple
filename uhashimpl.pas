unit uhashimpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Hash;

function MakeHash(buf:Pointer;Len:Integer):THashDigest;
function CompareHash(const d1, d2:THashDigest):Boolean;

implementation

uses RMD160;

var
  HashDesc:PHashDesc;

function MakeHash(buf:Pointer;Len:Integer):THashDigest;
begin
  HashFull(HashDesc,Result,buf,Len);
end;

function CompareHash(const d1, d2:THashDigest):Boolean;
begin
  Result:=HashSameDigest(HashDesc,@d1,@d2);
end;

initialization
  HashDesc:=FindHash_by_ID(_RMD160);

end.

