unit DWL.Crypt;

interface

uses
  System.SysUtils;

type
  TdwlCrypt = record
    class function TryMD5(const Bytes: TBytes; out Hash: TBytes): boolean; overload; static;
    class function TryMD5(const S: string; out Hash: TBytes): boolean; overload; static;
    class function MD5(const S: string): string; static;

    class function AsymmetricEncryption_GenerateKeyPair(var PrivateKey, PublicKey: TBytes): boolean; static;
    class function AsymmetricEncryption_Encrypt(var Bytes: TBytes; const PublicKey: TBytes): boolean; static;
    class function AsymmetricEncryption_Decrypt(var Bytes: TBytes; const PrivateKey: TBytes): boolean; static;

    class function AsymmetricSigning_GenerateKeyPair(var PrivateKey, PublicKey: TBytes): boolean; static;
    class function AsymmetricSigning_HashAndSign(const Bytes, PrivateKey: TBytes; out Signature: TBytes): boolean; static;
    class function AsymmetricSigning_CheckSignature(const Bytes, PublicKey, Signature: TBytes): boolean; static;

    class function AES_PrepareKey(BareKey:TBytes; var PreparedKey: TBytes): boolean; static;
    class function AES_PrepareKeyFromHexStr(const HexStr: string; var Key: TBytes): boolean; static;
    class function AES_Decrypt(var Bytes: TBytes; const Key: TBytes): boolean; overload; static;
    class function AES_Decrypt(var S: string; const Key: TBytes): boolean; overload; static;
    class function AES_Encrypt(var Bytes: TBytes; const Key: TBytes): boolean; overload; static;
    class function AES_Encrypt(var S: string; const Key: TBytes): boolean; overload; static;

    class function CRC32(Buf: Pointer; Len: LongWord): LongWord; static;
  end;

implementation

uses
  DWL.Crypt.API, Winapi.Windows, System.Classes, DWL.ConvUtils;

{ TdwlCrypt }

class function TdwlCrypt.MD5(const S: string): string;
begin
  var Hash: TBytes;
  if TryMD5(S, Hash) then
    Result := TdwlConvUtils.BytesToLowerHex(Hash)
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

class function TdwlCrypt.AES_PrepareKey(BareKey:TBytes; var PreparedKey: TBytes): boolean;
var
  KeyBufSize: word;
begin
  Result := false;
  try
    KeyBufSize := length(BareKey);
    if not ((KeyBufSize=16) or (KeyBufSize=24) or (KeyBufSize=32)) then
      raise Exception.Create('KeySize is not 16, 24 or 32');
    SetLength(PreparedKey, KeyBufSize+12);
    with PPUBLICKEYSTRUC(@PreparedKey[0])^ do
    begin
      bType := PLAINTEXTKEYBLOB;
      bVersion := CUR_BLOB_VERSION;
      reserved := 0;
      case KeyBufSize of
      16 : aiKeyAlg := CALG_AES_128;
      24 : aiKeyAlg := CALG_AES_192;
      32 : aiKeyAlg := CALG_AES_256;
      end;
    end;
    PDWORD(PByte(@PreparedKey[0])+8)^ := KeyBufSize;
    Move(BareKey[0], PreparedKey[12], KeyBufSize);
    Result := true;
  except
  end;
end;

class function TdwlCrypt.AES_PrepareKeyFromHexStr(const HexStr: string; var Key: TBytes): boolean;
var
  KeyBufSize: word;
begin
  Result := false;
  try
    KeyBufSize := Length(HexStr) div 2;
    if not ((KeyBufSize=16) or (KeyBufSize=24) or (KeyBufSize=32)) then
      raise Exception.Create('KeySize is not 16, 24 or 32');
    SetLength(Key, KeyBufSize+12);
    with PPUBLICKEYSTRUC(@Key[0])^ do
    begin
      bType := PLAINTEXTKEYBLOB;
      bVersion := CUR_BLOB_VERSION;
      reserved := 0;
      case KeyBufSize of
      16 : aiKeyAlg := CALG_AES_128;
      24 : aiKeyAlg := CALG_AES_192;
      32 : aiKeyAlg := CALG_AES_256;
      end;
    end;
    PDWORD(PByte(@Key[0])+8)^ := KeyBufSize;
    if HexToBin(PChar(HexStr), @Key[12], KeyBufSize)=KeyBufSize then
      Result := true;
  except
  end;
end;

class function TdwlCrypt.AsymmetricEncryption_Decrypt(var Bytes: TBytes; const PrivateKey: TBytes): boolean;
begin
  Result := false;
  try
    var hProv: HCRYPTPROV;
    if CryptAcquireContext(@hProv, nil, nil, PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
    try
      //import public key
      var hKey: HCRYPTKEY;
      if not CryptImportKey(hProv, @PrivateKey[0], Length(PrivateKey), 0, 0, @hKey) then
        Exit;
      try
        var DataSize := length(Bytes);
        if not CryptDecrypt(hKey, 0, true, 0, @Bytes[0], @DataSize) then
          Exit;
        SetLength(Bytes, DataSize);
        Result := true;
      finally
        CryptDestroyKey(hKey);
      end;
    finally
      CryptReleaseContext(hProv, 0);
    end;
  except
  end;
end;

class function TdwlCrypt.AsymmetricEncryption_Encrypt(var Bytes: TBytes; const PublicKey: TBytes): boolean;
begin
  Result := false;
  try
    var hProv: HCRYPTPROV;
    if not CryptAcquireContext(@hProv, nil, nil, PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
      Exit;
    try
      //import public key
      var hKey: HCRYPTKEY;
      if not CryptImportKey(hProv, @PublicKey[0], length(PublicKey), 0, 0, @hKey) then
          Exit;
      try
        var DataSize := length(Bytes);
        var BufSize := 1024+DataSize*2;  // rough estimate
        SetLength(Bytes, BufSize);
        if not CryptEncrypt(hKey, 0, true, 0, @Bytes[0], @DataSize, BufSize) then
          Exit;
        SetLength(Bytes, DataSize);
        Result := true;
      finally
        CryptDestroyKey(hKey);
      end;
    finally
      CryptReleaseContext(hProv, 0);
    end;
  except
  end;
end;

class function TdwlCrypt.AsymmetricEncryption_GenerateKeyPair(var PrivateKey, PublicKey: TBytes): boolean;
begin
  Result := false;
  try
    var hProv: HCRYPTPROV;
    if not CryptAcquireContext(@hProv, nil, nil, PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
      Exit;
    try
      var hKey: HCRYPTKEY;
      if not CryptGenKey(hProv, AT_KEYEXCHANGE, RSA1024BIT_KEY+CRYPT_EXPORTABLE, @hKey) then
        Exit;
      try
        // export Private Key
        var BufSize: DWORD;
        if not CryptExportKey(hKey, 0, PRIVATEKEYBLOB, 0, nil, @BufSize) then // get size of buf
          Exit;
        SetLength(PrivateKey, BufSize);
        if not CryptExportKey(hKey, 0, PRIVATEKEYBLOB, 0, @PrivateKey[0], @BufSize) then
          Exit;
        // export Public Key
        if not CryptExportKey(hKey, 0, PUBLICKEYBLOB, 0, nil, @BufSize) then // get size of buf
          Exit;
        SetLength(PublicKey, BufSize);
        if not CryptExportKey(hKey, 0, PUBLICKEYBLOB, 0, @PublicKey[0], @BufSize) then
          Exit;
        Result := true;
      finally
        CryptDestroyKey(hKey);
      end;
    finally
      CryptReleaseContext(hProv, 0);
    end;
  except
  end;
end;

class function TdwlCrypt.AsymmetricSigning_CheckSignature(const Bytes, PublicKey, Signature: TBytes): boolean;
begin
  Result := false;
  try
    var hProv: HCRYPTPROV;
    if not CryptAcquireContext(@hProv, nil, nil, PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
      Exit;
    try
      //import private key
      var  hKey: HCRYPTKEY;
      if not CryptImportKey(hProv, @PublicKey[0], length(PublicKey), 0, 0, @hKey) then
        Exit;
      try
        var hHash: HCRYPTHASH;
        if not CryptCreateHash(hProv, CALG_MD5, 0, 0, @hHash) then
          Exit;
        try
          if not CryptHashData(hHash, @Bytes[0], length(Bytes), 0) then
            Exit;
          Result := CryptVerifySignature(hHash, @Signature[0], Length(Signature), hKey, nil, 0);
        finally
          CryptDestroyHash(hHash);
        end;
      finally
        CryptDestroyKey(hKey);
      end;
    finally
      CryptReleaseContext(hProv, 0);
    end;
  except
  end;
end;

class function TdwlCrypt.AsymmetricSigning_GenerateKeyPair(var PrivateKey, PublicKey: TBytes): boolean;
begin
  Result := false;
  try
    var hProv: HCRYPTPROV;
    if not CryptAcquireContext(@hProv, nil, nil, PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
      Exit;
    try
      var  hKey: HCRYPTKEY;
      if not CryptGenKey(hProv, AT_KEYEXCHANGE, RSA1024BIT_KEY+CRYPT_EXPORTABLE, @hKey) then
        Exit;
      try
        // export Private Key
        var BufSize: DWORD;
        if not CryptExportKey(hKey, 0, PRIVATEKEYBLOB, 0, nil, @BufSize) then // get size of buf
          Exit;
        SetLength(PrivateKey, BufSize);
        if not CryptExportKey(hKey, 0, PRIVATEKEYBLOB, 0, @PrivateKey[0], @BufSize) then
          Exit;
        // export Public Key
        if not CryptExportKey(hKey, 0, PUBLICKEYBLOB, 0, nil, @BufSize) then // get size of buf
          Exit;
        SetLength(PublicKey, BufSize);
        if not CryptExportKey(hKey, 0, PUBLICKEYBLOB, 0, @PublicKey[0], @BufSize) then
          Exit;
        Result := true;
      finally
        CryptDestroyKey(hKey);
      end;
    finally
      CryptReleaseContext(hProv, 0);
    end;
  except
  end;
end;

class function TdwlCrypt.AsymmetricSigning_HashAndSign(const Bytes, PrivateKey: TBytes; out Signature: TBytes): boolean;
begin
  Result := false;
  try
    var hProv: HCRYPTPROV;
    if not CryptAcquireContext(@hProv, nil, nil, PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
      Exit;
    try
      //import private key
      var hKey: HCRYPTKEY;
      if not CryptImportKey(hProv, @PrivateKey[0], length(PrivateKey), 0, 0, @hKey) then
        Exit;
      try
        var hHash: HCRYPTHASH;
        if not CryptCreateHash(hProv, CALG_MD5, 0, 0, @hHash) then
          Exit;
        try
          if not CryptHashData(hHash, @Bytes[0], length(Bytes), 0) then
            Exit;
          var BufSize: DWord;
          if not CryptSignHash(hHash, AT_SIGNATURE, nil, 0, nil, @BufSize) then // get size of buf
            Exit;
          SetLength(Signature, BufSize);
          if not CryptSignHash(hHash, AT_SIGNATURE, nil, 0, @Signature[0], @BufSize) then
            Exit;
        finally
          CryptDestroyHash(hHash);
        end;
      finally
        CryptDestroyKey(hKey);
      end;
    finally
      CryptReleaseContext(hProv, 0);
    end;
    Result := true;
  except
  end;
end;

class function TdwlCrypt.CRC32(Buf: Pointer; Len: LongWord): LongWord;
  function _CRC32(crc:LongWord; Buf: Pointer; Len: LongWord): LongWord;
  const
     crc_32_tab : array[0..255] of longword = (
         $00000000, $77073096, $ee0e612c, $990951ba, $076dc419,
         $706af48f, $e963a535, $9e6495a3, $0edb8832, $79dcb8a4,
         $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07,
         $90bf1d91, $1db71064, $6ab020f2, $f3b97148, $84be41de,
         $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7, $136c9856,
         $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9,
         $fa0f3d63, $8d080df5, $3b6e20c8, $4c69105e, $d56041e4,
         $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
         $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3,
         $45df5c75, $dcd60dcf, $abd13d59, $26d930ac, $51de003a,
         $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599,
         $b8bda50f, $2802b89e, $5f058808, $c60cd9b2, $b10be924,
         $2f6f7c87, $58684c11, $c1611dab, $b6662d3d, $76dc4190,
         $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f,
         $9fbfe4a5, $e8b8d433, $7807c9a2, $0f00f934, $9609a88e,
         $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
         $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed,
         $1b01a57b, $8208f4c1, $f50fc457, $65b0d9c6, $12b7e950,
         $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3,
         $fbd44c65, $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2,
         $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb, $4369e96a,
         $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5,
         $aa0a4c5f, $dd0d7cc9, $5005713c, $270241aa, $be0b1010,
         $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f,
         $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17,
         $2eb40d81, $b7bd5c3b, $c0ba6cad, $edb88320, $9abfb3b6,
         $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615,
         $73dc1683, $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8,
         $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1, $f00f9344,
         $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb,
         $196c3671, $6e6b06e7, $fed41b76, $89d32be0, $10da7a5a,
         $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
         $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1,
         $a6bc5767, $3fb506dd, $48b2364b, $d80d2bda, $af0a1b4c,
         $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef,
         $4669be79, $cb61b38c, $bc66831a, $256fd2a0, $5268e236,
         $cc0c7795, $bb0b4703, $220216b9, $5505262f, $c5ba3bbe,
         $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31,
         $2cd99e8b, $5bdeae1d, $9b64c2b0, $ec63f226, $756aa39c,
         $026d930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
         $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b,
         $e5d5be0d, $7cdcefb7, $0bdbdf21, $86d3d2d4, $f1d4e242,
         $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1,
         $18b74777, $88085ae6, $ff0f6a70, $66063bca, $11010b5c,
         $8f659eff, $f862ae69, $616bffd3, $166ccf45, $a00ae278,
         $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7,
         $4969474d, $3e6e77db, $aed16a4a, $d9d65adc, $40df0b66,
         $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
         $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605,
         $cdd70693, $54de5729, $23d967bf, $b3667a2e, $c4614ab8,
         $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b,
         $2d02ef8d);
  type
    longrec = record
       lo,hi : word;
    end;
  var
    crcl : longrec absolute Result;
    i: integer;
    s: PByte;
  begin
    Result := crc;
     S := PByte(Buf);
    for i := 1 to len do
       begin
         Result :=(crc_32_tab[lo(crcl.lo) xor s^]) xor
                    ((Result shr 8) and $00ffffff);
         inc(S);
       end;
  end;
  function BareCRC32(InitCRC: LongWord; Buf: Pointer; Len: LongWord): LongWord;
  begin
    Result := _CRC32(InitCRC, Buf, Len);
  end;
begin
  Result := not BareCRC32($FFFFFFFF, Buf, Len);
end;

class function TdwlCrypt.AES_Decrypt(var Bytes: TBytes; const Key: TBytes): boolean;
var
  hProv: HCRYPTPROV;
  hKey: HCRYPTKEY;
  dW: DWORD;
  DataSize: DWORD;
begin
  Result := false;
  try
    if CryptAcquireContext(@hProv, nil, nil, PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
    try
      if not CryptImportKey(hProv, @Key[0], length(Key), 0, 0, @hKey) then
        Exit;
      try
        if not CryptSetKeyParam(hKey, KP_IV, @Key[12], 0) then
          Exit;
        dW := CRYPT_MODE_CBC;
        if not CryptSetKeyParam(hKey, KP_MODE, @dW, 0) then
          Exit;
        DataSize := Length(Bytes);
        if DataSize=0 then
          Exit;
        if not CryptDecrypt(hKey, 0, true, 0, @Bytes[0], @DataSize) then
          Exit;
        SetLength(Bytes, DataSize);
        Result := true;
      finally
        CryptDestroyKey(hKey);
      end;
    finally
      CryptReleaseContext(hProv, 0);
    end;
  except
  end;
end;

class function TdwlCrypt.AES_Decrypt(var S: string; const Key: TBytes): boolean;
var
  Bytes: TBytes;
begin
  Result := false;
  try
    Bytes := TdwlConvUtils.HexToBytes(S);
    if AES_Decrypt(Bytes, Key) then
    begin
      S := TEncoding.UTF8.GetString(Bytes);
      Result := true;
    end;
  except
  end;
end;

class function TdwlCrypt.AES_Encrypt(var Bytes: TBytes; const Key: TBytes): boolean;
var
  KeyBufSize: word;
  hProv: HCRYPTPROV;
  hKey: HCRYPTKEY;
  dW: DWORD;
  DataSize: DWORD;
  KeyAlignedDataSize: DWORD;
  BufSize: LongWord;
begin
  Result := false;
  try
    if not CryptAcquireContext(@hProv, nil, nil, PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
      Exit;
    try
      KeyBufSize := Length(Key)-12;
      if not CryptImportKey(hProv, @Key[0], length(Key), 0, 0, @hKey) then
        Exit;
      try
        if not CryptSetKeyParam(hKey, KP_IV, @Key[12], 0) then
          Exit;
        dW := CRYPT_MODE_CBC;
        if not CryptSetKeyParam(hKey, KP_MODE, @dW, 0) then
          Exit;
        DataSize := length(Bytes);
        if DataSize=0 then
          Exit;
        KeyAlignedDataSize := (((DataSize-1) div KeyBufSize)+1)*KeyBufSize;
        BufSize := KeyAlignedDataSize+KeyBufSize; {extra space for padding full key block}
        SetLength(Bytes, BufSize);
        if not CryptEncrypt(hKey, 0, true, 0, @Bytes[0], @DataSize, BufSize) then
          Exit;
        SetLength(Bytes, DataSize);
        Result := true;
      finally
        CryptDestroyKey(hKey);
      end;
    finally
      CryptReleaseContext(hProv, 0);
    end;
  except
  end;
end;

class function TdwlCrypt.AES_Encrypt(var S: string; const Key: TBytes): boolean;
var
  Bytes: TBytes;
begin
  Result := false;
  try
    Bytes := TEncoding.UTF8.GetBytes(S);
    if AES_Encrypt(Bytes, Key) then
    begin
      S := TdwlConvUtils.BytestoLowerHex(Bytes);
      Result := true;
    end;
  except
  end;
end;


end.
