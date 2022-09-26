unit DWL.OpenSSL.Api;

interface

{$WARN SYMBOL_PLATFORM OFF}

const
  BIO_C_SET_FILENAME = 108;
  BIO_C_GET_MD_CTX = 120;
  BIO_CTRL_PENDING = 10;

  BIO_CLOSE = $01;
  BIO_FP_READ = $02;

  RSA_F4 = $10001;

  MBSTRING_FLAG = $1000;
  MBSTRING_ASC = MBSTRING_FLAG or 1;

  NID_rsaEncryption = 6;
  EVP_PKEY_RSA = NID_rsaEncryption;

  SHA256_DIGEST_LENGTH = 32;
  SHA512_DIGEST_LENGTH = 64;
  NID_sha256 = 672;


type
  pBIO = Pointer;
  pBIO_METHOD = Pointer;

  pEVP_MD_CTX = Pointer;
  pEVP_MD = Pointer;

  pEVP_PKEY_CTX = Pointer;
  pEVP_PKEY = Pointer;

  PENGINE = pointer;

  PRSA_METHOD = pointer;
  PBIGNUM = pointer;
  PBN_GENCB = pointer;
  PSTACK = pointer;
  PBN_MONT_CTX = pointer;
  PBN_BLINDING = pointer;

  CRYPTO_EX_DATA = record
    sk : PSTACK;
    dummy : integer;
  end;
  PPRSA = ^PRSA;
  PRSA = pointer; // do not try to use this structure directly by defining the layout, but use the support functions for this

  PX509_REQ = pointer;
  PX509_NAME = pointer;

  PEVP_CIPHER = pointer;

  PASN1_INTEGER = ^asn1_string_st;
  PASN1_TIME = ^asn1_string_st;
  asn1_string_st = record
    length : integer;
    _type : integer;
    data : PAnsiChar;
    { The value of the following field depends on the type being
      held.  It is mostly being used for BIT_STRING so if the
      input data has a non-zero 'unused bits' value, it will be
      handled correctly }
    flags : longint;
  end;

  PX509_VAL = ^X509_VAL;
  X509_VAL = record
    notBefore : PASN1_TIME;
    notAfter : PASN1_TIME;
  end;

  ASN1_ENCODING = record
    enc: PAnsiChar;
    len: LongInt;
    modified: integer;
  end;

  PX509_CINF = ^X509_CINF;
  X509_CINF = record
    version: PASN1_INTEGER;
    serialNumber: PASN1_INTEGER;
    signature: pointer;
    issuer: PX509_NAME;
    validity: PX509_VAL;
    // and more not needed now
  end;

  PPX509 = ^PX509;
  PX509 = ^X509rec;
  X509rec = record
    cert_info: PX509_CINF;
    sig_alg : pointer;
    signature : pointer;
    valid : integer;
    references : integer;
    name : PAnsiChar;
    ex_data : CRYPTO_EX_DATA;
    // These contain copies of various extension values
    ex_pathlen : longint;
    ex_pcpathlen : longint;
    ex_flags : longint;
    ex_kusage : longint;
    ex_xkusage : longint;
    ex_nscert : longint;
    skid : pointer;
    akid : pointer;
    policy_cache : pointer;
    crldp : pointer;
    altname : pointer;
    nc : pointer;
  end;

  TPWCallbackFunction = function(buffer: PAnsiChar; length: Integer; verify: Integer; data: Pointer): Integer; cdecl;
  ppem_password_cb = function (buf : PAnsiChar; size : integer; rwflag : integer; userdata : Pointer) : integer; cdecl;

  // Error functions
function ERR_get_error: Cardinal; cdecl;
function ERR_error_string(e: Cardinal; buf: PAnsiChar): PAnsiChar; cdecl;
function ERR_GetErrorMessage: String;

// BIO functions
function BIO_new(_type: pBIO_METHOD): pBIO; cdecl;
function BIO_new_file(const aFileName: PAnsiChar; const aMode: PAnsiChar): pBIO; cdecl;
function BIO_free(a: pBIO): Integer; cdecl;
function BIO_s_file: pBIO_METHOD; cdecl;
function BIO_f_md: pBIO_METHOD; cdecl;
function BIO_ctrl(bp: pBIO; cmd: Integer; larg: Longint; parg: Pointer): Longint; cdecl;
function BIO_read(b: pBIO; buf: Pointer; len: Integer): Integer; cdecl;
function BIO_write(b: PBIO; buf: Pointer; len: Integer): Integer; cdecl;
function BIO_get_md_ctx(bp: pBIO; mdcp: Pointer): Longint;
function BIO_read_filename(bp: pBIO; filename: PAnsiChar): Integer;
function BIO_s_mem: PBIO_METHOD cdecl;
function BIO_pending(b : PBIO): integer;

function PEM_read_bio_PrivateKey(bp: pBIO; x: pEVP_PKEY; cb: TPWCallbackFunction; u: Pointer): pEVP_PKEY; cdecl;
function PEM_read_bio_PUBKEY(bp: pBIO; x: pEVP_PKEY; cb: TPWCallbackFunction; u: Pointer): pEVP_PKEY; cdecl;
function PEM_read_bio_RSAPrivateKey(bp : PBIO; x : PPRSA; cb : ppem_password_cb; u: Pointer) : PRSA cdecl;
function PEM_write_bio_RSAPrivateKey(bp : PBIO; x : PRSA; const enc : PEVP_CIPHER; kstr : PAnsiChar; klen : integer; cb : ppem_password_cb; u : Pointer) : integer cdecl;
function PEM_read_bio_RSAPublicKey(bp : PBIO; x : PPRSA; cb : ppem_password_cb; u: Pointer) : PRSA cdecl;
function PEM_write_bio_RSAPublicKey(bp : PBIO; x : PRSA) : integer cdecl;
function PEM_read_bio_X509(bp: PBIO; x: PPX509; cb: ppem_password_cb; u: Pointer): PX509 cdecl;

// EVP functions
function EVP_MD_CTX_new: pEVP_MD_CTX; cdecl;
procedure EVP_MD_CTX_free(ctx: pEVP_MD_CTX); cdecl;
function EVP_sha256: pEVP_MD; cdecl;
function EVP_PKEY_new: PEVP_PKEY cdecl;
function EVP_PKEY_assign(pkey: PEVP_PKEY; _type: integer; key: Pointer): integer cdecl;
function EVP_PKEY_assign_RSA(pkey: PEVP_PKEY; rsa: PRSA): integer;
procedure EVP_PKEY_free(pkey: PEVP_PKEY) cdecl;

function EVP_PKEY_size(key: pEVP_PKEY): Integer; cdecl;
function EVP_DigestSignInit(aCtx: pEVP_MD_CTX; aPCtx: pEVP_PKEY_CTX; aType: pEVP_MD; aEngine: PENGINE; aKey: pEVP_PKEY): Integer; cdecl;
function EVP_DigestSignUpdate(ctx: pEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; cdecl;
function EVP_DigestSignFinal(ctx: pEVP_MD_CTX; const d: PByte; var cnt: Cardinal): Integer; cdecl;

function EVP_DigestVerifyInit(aCtx: pEVP_MD_CTX; aPCtx: pEVP_PKEY_CTX; aType: pEVP_MD; aEngine: PENGINE; aKey: pEVP_PKEY): Integer; cdecl;
function EVP_DigestVerifyUpdate(ctx: pEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; cdecl;
function EVP_DigestVerifyFinal(ctx: pEVP_MD_CTX; const d: PByte; cnt: Cardinal): Integer; cdecl;
function EVP_sha1: PEVP_MD cdecl;

function RSA_new: PRSA cdecl;
function RSA_generate_key_ex(rsa: PRSA; bits: integer; e: PBIGNUM; cb: PBN_GENCB) : integer cdecl;
function RSA_set0_key(const rsa: PRSA; n: PBIGNUM; e: PBIGNUM; d: PBIGNUM): integer cdecl;
function RSA_get0_n(const rsa: PRSA): PBIGNUM cdecl;
function RSA_get0_e(const rsa: PRSA): PBIGNUM cdecl;
function RSA_size(const rsa: PRSA): integer cdecl;

procedure RSA_free(rsa: PRSA) cdecl;
function RSA_sign(type_: integer; m: pointer; m_len: cardinal; sigret: pointer; var siglen: cardinal; rsa: PRSA): integer cdecl;
function RSA_verify(type_: integer; m: pointer; m_len: cardinal; sigbuf: pointer; siglen: cardinal; rsa: PRSA): integer cdecl;

function BN_new: PBIGNUM cdecl;
function BN_set_word(a: PBIGNUM; w: integer): integer cdecl;
function BN_bn2hex(const a: PBIGNUM): PAnsiChar cdecl;
function BN_bin2bn(data: pointer; len: integer; ret: PBIGNUM): PBIGNUM cdecl;
function BN_bn2bin(const a: PBIGNUM; _to: pointer): integer cdecl;
function BN_num_bits(const a: PBIGNUM): integer cdecl;
procedure BN_free(a: PBIGNUM) cdecl;

function X509_REQ_new: PX509_REQ cdecl;
function X509_REQ_get_subject_name(x: PX509_REQ):PX509_NAME; cdecl;
function X509_REQ_set_pubkey(x: PX509_REQ; pkey: PEVP_PKEY): integer cdecl;
function X509_REQ_sign(x: PX509_REQ; pkey: PEVP_PKEY; const md: PEVP_MD): integer cdecl;
function X509_NAME_add_entry_by_txt(name: PX509_NAME; const field: PAnsiChar; _type: integer; const bytes: PAnsiChar; len, loc, _set: integer): integer cdecl;
function X509_get0_notAfter(x: PX509): PASN1_TIME cdecl;
procedure X509_free(x: PX509) cdecl;

function SHA256(const d: pointer; n: NativeUInt; md: pointer): pointer cdecl;
function SHA512(const d: pointer; n: NativeUInt; md: pointer): pointer cdecl;
function PKCS5_PBKDF2_HMAC_SHA1(pass : PAnsiChar; passlen: integer; salt: PAnsiChar; saltlen: integer; iter: integer; keylen: integer; _out : PAnsiChar): integer cdecl;
function PKCS5_PBKDF2_HMAC(pass : PAnsiChar; passlen: integer; salt: PAnsiChar; saltlen: integer; iter: integer; const digest: PEVP_MD; keylen: integer; _out : PAnsiChar): integer cdecl;
function RAND_bytes(buf: PAnsiChar; num: integer): integer cdecl;

function i2d_X509_REQ_bio(bp: PBIO;x: PX509_REQ): integer cdecl;

implementation

uses
  System.AnsiStrings;

const
  {$IFDEF WIN64}
  LIBEAY_DLL_NAME = 'libcrypto-3-x64.dll';
  {$ELSE}
  LIBEAY_DLL_NAME = 'libcrypto-3.dll';
  {$ENDIF}

function ERR_get_error: Cardinal; external LIBEAY_DLL_NAME delayed;
function ERR_error_string; external LIBEAY_DLL_NAME delayed;

function BIO_new; external LIBEAY_DLL_NAME delayed;
function BIO_new_file; external LIBEAY_DLL_NAME delayed;
function BIO_free; external LIBEAY_DLL_NAME delayed;
function BIO_ctrl; external LIBEAY_DLL_NAME delayed;
function BIO_s_file; external LIBEAY_DLL_NAME delayed;
function BIO_f_md; external LIBEAY_DLL_NAME delayed;
function BIO_read; external LIBEAY_DLL_NAME delayed;
function BIO_write; external LIBEAY_DLL_NAME delayed;
function BIO_s_mem; external LIBEAY_DLL_NAME delayed;

function PEM_read_bio_PrivateKey; external LIBEAY_DLL_NAME delayed;
function PEM_read_bio_PUBKEY; external LIBEAY_DLL_NAME delayed;
function PEM_read_bio_RSAPrivateKey; external LIBEAY_DLL_NAME delayed;
function PEM_write_bio_RSAPrivateKey; external LIBEAY_DLL_NAME delayed;
function PEM_read_bio_RSAPublicKey; external LIBEAY_DLL_NAME delayed;
function PEM_write_bio_RSAPublicKey; external LIBEAY_DLL_NAME delayed;
function PEM_read_bio_X509; external LIBEAY_DLL_NAME delayed;

function EVP_MD_CTX_new; external LIBEAY_DLL_NAME delayed;
procedure EVP_MD_CTX_free; external LIBEAY_DLL_NAME delayed;
function EVP_sha256; external LIBEAY_DLL_NAME delayed;
function EVP_PKEY_new; external LIBEAY_DLL_NAME delayed;
function EVP_PKEY_assign; external LIBEAY_DLL_NAME delayed;
procedure EVP_PKEY_free; external LIBEAY_DLL_NAME delayed;

function EVP_PKEY_size; external LIBEAY_DLL_NAME delayed;
function EVP_DigestSignInit; external LIBEAY_DLL_NAME delayed;
function EVP_DigestSignUpdate; external LIBEAY_DLL_NAME name 'EVP_DigestUpdate' delayed;
function EVP_DigestSignFinal; external LIBEAY_DLL_NAME delayed;

function EVP_DigestVerifyInit; external LIBEAY_DLL_NAME delayed;
function EVP_DigestVerifyUpdate; external LIBEAY_DLL_NAME name 'EVP_DigestUpdate' delayed;
function EVP_DigestVerifyFinal; external LIBEAY_DLL_NAME delayed;

function EVP_sha1; external LIBEAY_DLL_NAME delayed;

function RSA_New: PRSA; external LIBEAY_DLL_NAME delayed;
function RSA_generate_key_ex; external LIBEAY_DLL_NAME delayed;
function RSA_set0_key; external LIBEAY_DLL_NAME delayed;
function RSA_get0_n; external LIBEAY_DLL_NAME delayed;
function RSA_get0_e; external LIBEAY_DLL_NAME delayed;
function RSA_size; external LIBEAY_DLL_NAME delayed;

procedure RSA_free; external LIBEAY_DLL_NAME delayed;
function RSA_sign; external LIBEAY_DLL_NAME delayed;
function RSA_verify; external LIBEAY_DLL_NAME delayed;
//
function BN_New; external LIBEAY_DLL_NAME delayed;
function BN_set_word; external LIBEAY_DLL_NAME delayed;
function BN_bn2hex; external LIBEAY_DLL_NAME delayed;
function BN_bin2bn; external LIBEAY_DLL_NAME delayed;
function BN_bn2bin; external LIBEAY_DLL_NAME delayed;
function BN_num_bits; external LIBEAY_DLL_NAME delayed;
procedure BN_free; external LIBEAY_DLL_NAME delayed;

function X509_NAME_add_entry_by_txt; external LIBEAY_DLL_NAME delayed;
function X509_REQ_new; external LIBEAY_DLL_NAME delayed;
function X509_REQ_get_subject_name; external LIBEAY_DLL_NAME delayed;
function X509_REQ_set_pubkey; external LIBEAY_DLL_NAME delayed;
function X509_REQ_sign; external LIBEAY_DLL_NAME delayed;
function X509_get0_notAfter; external LIBEAY_DLL_NAME delayed;
procedure X509_free; external LIBEAY_DLL_NAME delayed;

function SHA256; external LIBEAY_DLL_NAME delayed;
function SHA512; external LIBEAY_DLL_NAME delayed;
function PKCS5_PBKDF2_HMAC_SHA1; external LIBEAY_DLL_NAME delayed;
function PKCS5_PBKDF2_HMAC; external LIBEAY_DLL_NAME delayed;
function RAND_bytes; external LIBEAY_DLL_NAME delayed;

function i2d_X509_REQ_bio; external LIBEAY_DLL_NAME delayed;

function ERR_GetErrorMessage: String;
var
  locErrMsg: array [0 .. 160] of Char;
begin
  ERR_error_string(ERR_get_error, @locErrMsg);
  Result := String(System.AnsiStrings.StrPas(PAnsiChar(@locErrMsg)));
end;

function BIO_get_md_ctx(bp: pBIO; mdcp: Pointer): Longint;
begin
  Result := BIO_ctrl(bp, BIO_C_GET_MD_CTX, 0, mdcp);
end;

function BIO_read_filename(bp: pBIO; filename: PAnsiChar): Integer;
begin
  Result := BIO_ctrl(bp, BIO_C_SET_FILENAME, BIO_CLOSE or BIO_FP_READ, filename);
end;

function BIO_pending(b : PBIO): integer;
begin
  Result := BIO_ctrl(b, BIO_CTRL_PENDING,0,nil);
end;

function EVP_PKEY_assign_RSA(pkey: PEVP_PKEY; rsa: PRSA): integer;
begin
  Result := EVP_PKEY_assign(pkey, EVP_PKEY_RSA, rsa);
end;

end.
