unit DWL.Crypt;

interface

uses
  System.SysUtils;

type
  TdwlCrypt = record
    class function TryMD5(const Bytes: TBytes; out Hash: TBytes): boolean; overload; static;
    class function TryMD5(const S: string; out Hash: TBytes): boolean; overload; static;
    class function MD5(const S: string): string; static;
  end;

implementation

uses
  DWL.SysUtils, DWL.Crypt.API;

{ TdwlCrypt }

class function TdwlCrypt.MD5(const S: string): string;
begin
  var Hash: TBytes;
  if TryMD5(S, Hash) then
    Result := BytesToLowerHex(Hash)
  else
    Result := '';
end;

class function TdwlCrypt.TryMD5(const S: string; out Hash: TBytes): boolean;
begin
  Result := TryMD5(TEncoding.UTF8.GetBytes(S), Hash);
end;

class function TdwlCrypt.TryMD5(const Bytes: TBytes; out Hash: TBytes): boolean;
var
  hProv: HCRYPTPROV;
  hHash: HCRYPTHASH;
  dwDataLen: LongWord;
begin
  dwDataLen := 16;
  SetLength(Hash, dwDataLen);
  Result := false;
  if CryptAcquireContext(@hProv, nil, nil, PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
  try
    if CryptCreateHash(hProv, CALG_MD5, 0, 0, @hHash) then
    try
      if CryptHashData(hHash, @Bytes[0], length(Bytes), 0) then
      begin
        Result := CryptGetHashParam(hHash, HP_HASHVAL, @Hash[0], @dwDataLen, 0);
        Assert(dwDataLen=16);
      end;
    finally
      CryptDestroyHash(hHash);
    end;
  finally
    CryptReleaseContext(hProv, 0);
  end;
end;

end.
