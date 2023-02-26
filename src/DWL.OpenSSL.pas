unit DWL.OpenSSL;

interface

uses
  System.SysUtils, DWL.OpenSSL.API;

type
  IdwlOpenSSLKey = interface
    function key: pEVP_PKEY;
    function Modulus_n: TBytes;
    function PublicExponent_e: TBytes;
  end;

  IdwlX509Cert = interface
    function X509(ThisCallTakesOwnerShip: boolean=false): pX509;
  end;

  TdwlOpenSSL = record
  public
    class function New_PrivateKey: IdwlOpenSSLKey; static;
    class function New_PrivateKey_FromPEMFile(const FileName: string): IdwlOpenSSLKey; static;
    class function New_PrivateKey_FromPEMStr(const PEM: string): IdwlOpenSSLKey; static;
    class function New_PublicKey_FromModulusExponent(const Modulus, Exponent: TBytes): IdwlOpenSSLKey; static;
    class procedure PrivateKey_SaveToPEMFile(PrivateKey: IdwlOpenSSLKey; const FileName: string); static;
    class function Calculate_SHA256(const Data: ansistring): TBytes; overload; static;
    class function Calculate_SHA256(const Data: PByte; DataSize: NativeUInt): TBytes; overload; static;
    class function Calculate_SHA512(const Data: ansistring): TBytes; static;
    class function CalculateSignature(const Data: ansistring; PrivateKey: IdwlOpenSSLKey): TBytes; static;
    class function VerifySignature(const Data: ansistring; const Signature: TBytes; PublicKey: IdwlOpenSSLKey): boolean; static;
    /// <summary>
    ///   Generates <Amount> random bytes using a cryptographically secure pseudo random generator (CSPRNG)
    /// </summary>
    class function RandomBytes(Amount: integer): TBytes; static;
    // derives a bas64url encoded key from a base64url hash salt and a plain password using PBKDF2 and HMAC-SHA256
    /// <summary>
    ///   Derives a Key (base64url encoded) from a salt(bat64url encoded) and a plain password
    /// </summary>
    class function DeriveKeyFromPassword(const Salt_b64utf, Password_plain: string): string; static;
    class function ASN1_StringToDateTime(x: pASN1_STRING): TDateTime; static;
    class function New_Cert_FromPEMStr(const PEM: string): IdwlX509Cert; static;
  end;

implementation

uses
  Winapi.Windows, System.DateUtils, System.NetEncoding;

type
  TdwlOpenSSLKey = class(TInterfacedObject, IdwlOpenSSLKey)
  strict private
    FKey: pEVP_PKEY;
  private
    function key: pEVP_PKEY;
    function Modulus_n: TBytes;
    function PublicExponent_e: TBytes;
  public
    constructor Create(AKey: pEVP_PKEY);
    destructor Destroy; override;
  end;

  TdwlX509Cert = class(TInterfacedObject, IdwlX509Cert)
  strict private
    FX509: pX509;
    FSkipX509_free: boolean;
  private
    function X509(TransferOwnerShip: boolean=false): pX509;
  public
    constructor Create(AX509: pX509);
    destructor Destroy; override;
  end;

{ TdwlOpenSSL }

class function TdwlOpenSSL.ASN1_StringToDateTime(x: pASN1_STRING): TDateTime;
  function Char2Int(d, u: AnsiChar): integer;
  begin
    result := (Ord(d) - Ord('0'))*10 + Ord(u) - Ord('0');
  end;
begin
  var tz := 0;
  var s := 0;
  var Y := 0;
  var M := 0;
  var D := 0;
  var h := 0;
  var n := 0;
  var StrLen := ASN1_STRING_length(x);
  var AsnStr := ASN1_STRING_get0_data(x); 
  if StrLen = 13 then
    begin
      Y := Char2Int(AsnStr[0], AsnStr[1]);
      if Y < 50 then
        Y := Y + 100;
      Y := Y + 1900;
      M := Char2Int(AsnStr[2], AsnStr[3]);
      D := Char2Int(AsnStr[4], AsnStr[5]);
      h := Char2Int(AsnStr[6], AsnStr[7]);
      n := Char2Int(AsnStr[8], AsnStr[9]);
      if (AsnStr[10] >= '0') and (AsnStr[10] <= '9')
          and (AsnStr[11] >= '0') and (AsnStr[11] <= '9') then
        s := Char2Int(AsnStr[10], AsnStr[11]);
      if AsnStr[12] = 'Z' then
        tz := 1;
    end
  else if StrLen = 15 then
    begin
      Y := Char2Int(AsnStr[0], AsnStr[1])*100 + Char2Int(AsnStr[2], AsnStr[3]);;
      M := Char2Int(AsnStr[4], AsnStr[5]);
      D := Char2Int(AsnStr[6], AsnStr[7]);
      h := Char2Int(AsnStr[8], AsnStr[9]);
      n := Char2Int(AsnStr[10], AsnStr[11]);
      if (AsnStr[12] >= '0') and (AsnStr[12] <= '9')
          and (AsnStr[13] >= '0') and (AsnStr[13] <= '9') then
        s := Char2Int(AsnStr[12], AsnStr[13]);
      if AsnStr[14] = 'Z' then
        tz := 1;
    end;
  if tz > 0 then
    Result := IncHour(EncodeDateTime(Y, M, D, h, n, s, 0), tz)
  else
    Result := EncodeDateTime(Y, M, D, h , n, s, 0);
end;

class function TdwlOpenSSL.CalculateSignature(const Data: ansistring; PrivateKey: IdwlOpenSSLKey): TBytes;
begin
  var mdctx := EVP_MD_CTX_new;
  try
    EVP_DigestSignInit(mdctx, nil, EVP_sha256, nil, PrivateKey.key);
    EVP_DigestSignUpdate(mdctx, PAnsiChar(Data), Length(Data));
    var ResultSize: size_t := 512;
    SetLength(Result, ResultSize);
    if EVP_DigestSignFinal(mdctx, PAnsiChar(@Result[0]), ResultSize)<>1 then
      raise Exception.Create('Error Message');
    SetLength(Result, ResultSize);
    EVP_DigestSignFinal(mdctx, PAnsiChar(@Result[0]), ResultSize);
  finally
    EVP_MD_CTX_free(mdctx);
  end;
end;

class function TdwlOpenSSL.Calculate_SHA256(const Data: ansistring): TBytes;
begin
  SetLength(Result, SHA256_DIGEST_LENGTH);
  SHA256(PAnsiChar(Data), Length(Data), @Result[0]);
end;

class function TdwlOpenSSL.Calculate_SHA256(const Data: PByte; DataSize: NativeUInt): TBytes;
begin
  SetLength(Result, SHA256_DIGEST_LENGTH);
  SHA256(PAnsiChar(Data), DataSize,  @Result[0]);
end;

class function TdwlOpenSSL.Calculate_SHA512(const Data: ansistring): TBytes;
begin
  SetLength(Result, SHA512_DIGEST_LENGTH);
  SHA512(PAnsiChar(Data), Length(Data), @Result[0]);
end;

class function TdwlOpenSSL.DeriveKeyFromPassword(const Salt_b64utf, Password_plain: string): string;
const
  Iterations = 131072; // slow enough.. (f.e. LastPass uses 5000)
  OctetCount = 64; // big enought (f.e. Lastpas uses 32)
  // 2021-12-11 AdR calculation time on my development machine: 100ms
  // If needed these constants can become optional parameters if needed in the future
begin
  // hashing using PBKDF2 and HMAC-SHA256 (both FIPS validated implementations)
  var SaltBytes: TBytes := TNetencoding.Base64URL.DecodeStringToBytes(Salt_b64utf);
  var Password_utf8 := UTF8String(Password_plain);
  var Bytes: TBytes;
  SetLength(Bytes, OctetCount);
  if PKCS5_PBKDF2_HMAC(PAnsichar(PUTF8String(Password_utf8)), Length(Password_utf8), @SaltBytes[0], Length(SaltBytes),
    Iterations, EVP_sha256, OctetCount, @Bytes[0])<>1 then
    raise Exception.Create('Error executing key derivation function');
  Result := TNetEncoding.Base64URL.EncodeBytesToString(Bytes);
end;

class function TdwlOpenSSL.New_PrivateKey_FromPEMFile(const FileName: string): IdwlOpenSSLKey;
begin
  var BIO := BIO_new_file(PAnsiChar(ansistring(FileName)), PAnsiChar('r+'));
  try
    var key := PEM_read_bio_PrivateKey(BIO, nil, nil, nil);
    if key=nil then
      Result := nil
    else
      Result := TdwlOpenSSLKey.Create(key);
  finally
    BIO_free(BIO);
  end;
end;

class function TdwlOpenSSL.New_PrivateKey_FromPEMStr(const PEM: string): IdwlOpenSSLKey;
begin
  var BIO := BIO_new(BIO_s_mem);
  try
    var AnsiPEM := ansistring(PEM);
    BIO_write(BIO, PAnsiChar(AnsiPEM), Length(AnsiPEM));
    var key := PEM_read_bio_PrivateKey(BIO, nil, nil, nil);
    if key=nil then
      Result := nil
    else
      Result := TdwlOpenSSLKey.Create(key);
  finally
    BIO_Free(BIO);
  end;
end;

class function TdwlOpenSSL.New_Cert_FromPEMStr(const PEM: string): IdwlX509Cert;
begin
  var AnsiPem := ansistring(PEM);
  var Bio := BIO_new_mem_buf(PAnsiChar(AnsiPem), length(AnsiPEM));
  try
    var X509 := PEM_read_bio_X509(Bio, nil, nil, nil);
    if X509=nil then
      raise Exception.Create('Error Message');
    Result := TdwlX509Cert.Create(x509);
  finally
    BIO_free(Bio);
  end;
end;

class function TdwlOpenSSL.New_PrivateKey: IdwlOpenSSLKey;
begin
  Result := TdwlOpenSSLKey.Create(EVP_PKEY_Q_keygen(nil, nil, 'RSA', 4096));
end;

class function TdwlOpenSSL.New_PublicKey_FromModulusExponent(const Modulus, Exponent: TBytes): IdwlOpenSSLKey;
begin
  var key: pEVP_PKEY := nil;
  var params: pOSSL_PARAM;
  var bld := OSSL_PARAM_BLD_new;
  var BN_n := BN_bin2bn(@Modulus[0], Length(Modulus), nil);
  var BN_e := BN_bin2bn(@Exponent[0], Length(Exponent), nil);
  try
    CheckOpenSSL(OSSL_PARAM_BLD_push_BN(bld, 'n', BN_n));
    CheckOpenSSL(OSSL_PARAM_BLD_push_BN(bld, 'e', BN_e));
    params := OSSL_PARAM_BLD_to_param(bld);
    try
      var ctx := EVP_PKEY_CTX_new_from_name(nil, 'RSA', nil);
      try
        CheckOpenSSL(EVP_PKEY_fromdata_init(ctx));
        CheckOpenSSL(EVP_PKEY_fromdata(ctx, key, EVP_PKEY_PUBLIC_KEY, params));
      finally
        EVP_PKEY_CTX_free(ctx);
      end;
    finally
      OSSL_PARAM_free(params);
    end;
  finally
    OSSL_PARAM_BLD_free(bld);
    BN_free(BN_n);
    BN_free(BN_e);
  end;
  Result := TdwlOpenSSLKey.Create(key);
end;

class function TdwlOpenSSL.RandomBytes(Amount: integer): TBytes;
begin
  SetLength(Result, Amount);
  if RAND_bytes(@Result[0], Amount)<>1 then
    raise Exception.Create('Error generating random bytes');
end;

class procedure TdwlOpenSSL.PrivateKey_SaveToPEMFile(PrivateKey: IdwlOpenSSLKey; const FileName: string);
begin
  var bp := BIO_new_file(PAnsiChar(ansistring(FileName)), PAnsiChar('w+'));
  try
    PEM_write_bio_PrivateKey(bp, PrivateKey.key, nil, nil, 0, nil, nil);
  finally
    BIO_free(bp);
  end;
end;

class function TdwlOpenSSL.VerifySignature(const Data: ansistring; const Signature: TBytes; PublicKey: IdwlOpenSSLKey): boolean;
begin
  var mdctx := EVP_MD_CTX_new;
  try
    CheckOpenSSL(EVP_DigestVerifyInit(mdctx, nil, EVP_sha256, nil, PublicKey.key));
    CheckOpenSSL(EVP_DigestVerifyUpdate(mdctx, PAnsiChar(Data), Length(Data)));
    var Res := EVP_DigestVerifyFinal(mdctx, PAnsiChar(@Signature[0]), Length(Signature));
    CheckOpenSSL(Res);
    Result := Res=1;
  finally
    EVP_MD_CTX_free(mdctx);
  end;
end;

{ TopensslKey }

constructor TdwlOpenSSLKey.Create(AKey: pEVP_PKEY);
begin
  inherited Create;
  FKey := AKey;
end;

destructor TdwlOpenSSLKey.Destroy;
begin
  EVP_PKEY_free(FKey);
  inherited Destroy;
end;

function TdwlOpenSSLKey.PublicExponent_e: TBytes;
begin
  var bn := BN_new;
  try
    if EVP_PKEY_get_bn_param(key, 'e', bn)<>1 then
      raise Exception.Create('Error retrieving e');
    var BN_Size := BN_num_bytes(bn);
    SetLength(Result, BN_Size);
    BN_bn2bin(BN, @Result[0]);
  finally
    BN_free(bn);
  end;
end;

function TdwlOpenSSLKey.key: pEVP_PKEY;
begin
  Result := FKey;
end;

function TdwlOpenSSLKey.Modulus_n: TBytes;
begin
  var bn := BN_new;
  try
    if EVP_PKEY_get_bn_param(key, 'n', bn)<>1 then
      raise Exception.Create('Error retrieving n');
    var BN_Size := BN_num_bytes(bn);
    SetLength(Result, BN_Size);
    BN_bn2bin(BN, @Result[0]);
  finally
    BN_free(bn);
  end;
end;

{ TdwlX509Cert }

constructor TdwlX509Cert.Create(AX509: pX509);
begin
  inherited Create;
  FX509 := AX509;
end;

destructor TdwlX509Cert.Destroy;
begin
  if not FSkipX509_free then
    X509_free(FX509);
  inherited Destroy;
end;

function TdwlX509Cert.X509(TransferOwnerShip: boolean=false): pX509;
begin
  Result := FX509;
  FSkipX509_free := TransferOwnerShip;
end;

end.
