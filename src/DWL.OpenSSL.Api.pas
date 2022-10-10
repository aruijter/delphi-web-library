unit DWL.OpenSSL.Api;

interface

uses
  Winapi.Windows;

{$I dwl.inc}

const
  BIO_CTRL_PENDING = 10;

  MBSTRING_FLAG = $1000;
  MBSTRING_ASC = MBSTRING_FLAG or 1;

  RSA_F4 = $10001;

  SHA256_DIGEST_LENGTH = 32;
  SHA512_DIGEST_LENGTH = 64;

  OSSL_KEYMGMT_SELECT_PRIVATE_KEY = $01;
  OSSL_KEYMGMT_SELECT_PUBLIC_KEY = $02;
  OSSL_KEYMGMT_SELECT_DOMAIN_PARAMETERS = $04;
  OSSL_KEYMGMT_SELECT_OTHER_PARAMETERS = $80;
  OSSL_KEYMGMT_SELECT_ALL_PARAMETERS = OSSL_KEYMGMT_SELECT_DOMAIN_PARAMETERS OR OSSL_KEYMGMT_SELECT_OTHER_PARAMETERS;
  EVP_PKEY_KEY_PARAMETERS = OSSL_KEYMGMT_SELECT_ALL_PARAMETERS;
  EVP_PKEY_PUBLIC_KEY = EVP_PKEY_KEY_PARAMETERS OR OSSL_KEYMGMT_SELECT_PUBLIC_KEY;
  EVP_PKEY_KEYPAIR = EVP_PKEY_PUBLIC_KEY OR OSSL_KEYMGMT_SELECT_PRIVATE_KEY;

type
  int = integer;

  pASN1_STRING = pointer;
  pASN1_TIME = pASN1_STRING;

  pBIGNUM = pointer;

  pBIO_METHOD = pointer;
  pBIO = pointer;

  pENGINE = pointer;

  pEVP_MD_CTX = pointer;
  pEVP_MD = pointer;
  ppEVP_PKEY_CTX = ^pEVP_PKEY_CTX;
  pEVP_PKEY_CTX = Pointer;
  pEVP_CIPHER = pointer;
  ppEVP_PKEY = ^pEVP_PKEY;
  pEVP_PKEY = pointer;

  pOSSL_LIB_CTX = pointer;
  pOSSL_PARAM = pointer;
  pOSSL_PARAM_BLD = pointer;

  ppem_password_cb = pointer;

  ppX509 = ^pX509;
  pX509 = pointer;
  pX509_REQ = pointer;
  pX509_NAME = pointer;

// Error functions
function ERR_get_error: long; cdecl;
function ERR_error_string(e: ulong32; buf: PAnsiChar): PAnsiChar; cdecl;
function ERR_GetErrorMessage: String;
// ASN1 functions
function ASN1_STRING_get0_data(const x: pASN1_STRING): PAnsiChar;
function ASN1_STRING_length(const x: pASN1_STRING): int;
// BIO functions
function BIO_new(_type: pBIO_METHOD): pBIO; cdecl;
function BIO_new_file(const filename: PAnsiChar; const mode: PAnsiChar): pBIO; cdecl;
function BIO_free(a: pBIO): int; cdecl;
function BIO_ctrl(bp: pBIO; cmd: int; larg: long; parg: pointer): long; cdecl;
function BIO_read(b: pBIO; data: pointer; dlen: int): int; cdecl;
function BIO_write(b: pBIO; data: pointer; dlen: int): int; cdecl;
function BIO_s_mem: PBIO_METHOD cdecl;
function BIO_pending(b : pBIO): int;
// BN functions
function BN_new: pBIGNUM cdecl;
function BN_bn2hex(const a: pBIGNUM): PAnsiChar cdecl;
function BN_bin2bn(s: PAnsiChar; len: int; ret: pBIGNUM): pBIGNUM cdecl;
function BN_bn2bin(const a: pBIGNUM; _to: PAnsiChar): int cdecl;
function BN_num_bits(const a: PBIGNUM): int cdecl;
function BN_num_bytes(const a: pBIGNUM): integer;
procedure BN_free(a: pBIGNUM) cdecl;
// OSSL functions
function OSSL_PARAM_BLD_new: pOSSL_PARAM_BLD;
procedure OSSL_PARAM_BLD_free(bld: pOSSL_PARAM_BLD);
function OSSL_PARAM_BLD_push_BN(bld: pOSSL_PARAM_BLD; key: PAnsiChar; const bn: pBIGNUM): int;
function OSSL_PARAM_BLD_to_param(bld: pOSSL_PARAM_BLD): pOSSL_PARAM;
procedure OSSL_PARAM_free(params: pOSSL_PARAM);
// EVP functions
function EVP_MD_CTX_new: pEVP_MD_CTX; cdecl;
procedure EVP_MD_CTX_free(ctx: pEVP_MD_CTX); cdecl;
function EVP_sha256: pEVP_MD; cdecl;
function EVP_PKEY_new: pEVP_PKEY cdecl;
procedure EVP_PKEY_free(pkey: PEVP_PKEY) cdecl;
function EVP_PKEY_Q_keygen(libctx: pOSSL_LIB_CTX; const propq: PAnsiChar; const _type: PAnsiChar; bits: size_t): pEVP_PKEY;
function EVP_PKEY_get_bn_param(pkey: pEVP_PKEY; key_name: PAnsiChar; var bn: pBIGNUM): int;
function EVP_PKEY_set_bn_param(pkey: pEVP_PKEY; key_name: PAnsiChar; var bn: pBIGNUM): int;
function EVP_PKEY_CTX_new_from_name(libctx: pOSSL_LIB_CTX; const name: PAnsiChar; const propquery: PAnsiChar): pEVP_PKEY_CTX;
procedure EVP_PKEY_CTX_free(ctx: pEVP_PKEY_CTX);
function EVP_PKEY_fromdata_init(ctx: pEVP_PKEY_CTX): int;
function EVP_PKEY_fromdata(ctx: pEVP_PKEY_CTX; var key: pEVP_PKEY; selection: int; params: pOSSL_PARAM): int;

function EVP_DigestSignInit(ctx: pEVP_MD_CTX; pctx: ppEVP_PKEY_CTX; _type: pEVP_MD; e: pENGINE; pkey: pEVP_PKEY): int; cdecl;
function EVP_DigestSignUpdate(ctx: pEVP_MD_CTX; const data: pointer; cnt: size_t): int; cdecl;
function EVP_DigestSignFinal(ctx: pEVP_MD_CTX; const sig: PAnsiChar; var siglen: size_t): int; cdecl;
function EVP_DigestVerifyInit(ctx: pEVP_MD_CTX; pctx: ppEVP_PKEY_CTX; _type: pEVP_MD; e: pENGINE; pkey: pEVP_PKEY): int; cdecl;
function EVP_DigestVerifyUpdate(ctx: pEVP_MD_CTX; const d: pointer; cnt: size_t): int; cdecl;
function EVP_DigestVerifyFinal(ctx: pEVP_MD_CTX; const sig: PAnsiChar; siglen: size_t): Integer; cdecl;
// PEM functions
function PEM_read_bio_PrivateKey(bp: pBIO; x: ppEVP_PKEY; cb: ppem_password_cb; u: pointer): pEVP_PKEY; cdecl;
function PEM_write_bio_PrivateKey(bp : PBIO; x : pEVP_PKEY; const enc : pEVP_CIPHER; kstr: pointer; klen: int; cb: ppem_password_cb; u: pointer) : int cdecl;
function PEM_read_bio_X509(bp: pBIO; x: ppX509; cb: ppem_password_cb; u: pointer): pX509 cdecl;
// X509 functions
function X509_REQ_new: pX509_REQ cdecl;
function X509_REQ_get_subject_name(x: pX509_REQ):pX509_NAME; cdecl;
function X509_REQ_set_pubkey(x: pX509_REQ; pkey: pEVP_PKEY): int cdecl;
function X509_REQ_sign(x: pX509_REQ; pkey: pEVP_PKEY; const md: pEVP_MD): integer cdecl;
function X509_NAME_add_entry_by_txt(name: pX509_NAME; const field: PAnsiChar; _type: int; const bytes: PAnsiChar; len, loc, _set: int): int cdecl;
function X509_get0_notAfter(x: pX509): pASN1_TIME cdecl;
procedure X509_free(x: pX509) cdecl;
function i2d_X509_REQ_bio(bp: pBIO; x: pX509_REQ): int cdecl;
//
function SHA256(const d: PAnsiChar; n: size_t; md: PAnsiChar): pointer cdecl;
function SHA512(const d: PAnsiChar; n: size_t; md: PAnsiChar): pointer cdecl;
function PKCS5_PBKDF2_HMAC_SHA1(const pass: PAnsiChar; passlen: int; const salt: PAnsiChar; saltlen: int; iter: int; keylen: int; _out: PAnsiChar): int cdecl;
function PKCS5_PBKDF2_HMAC(const pass: PAnsiChar; passlen: int; const salt: PAnsiChar; saltlen: int; iter: int; const digest: pEVP_MD; keylen: int; _out: PAnsiChar): int cdecl;
procedure OPENSSL_free(addr: pointer);
procedure CRYPTO_free(ptr: pointer; const _file: PAnsiChar; line: int);
function RAND_bytes(buf: PAnsiChar; num: int): int cdecl;

procedure CheckOpenSSL(Res: int);

implementation

uses
  System.AnsiStrings, System.SysUtils;

const
  {$IFDEF WIN64}
  LIBEAY_DLL_NAME = 'libcrypto-3-x64.dll';
  {$ELSE}
  LIBEAY_DLL_NAME = 'libcrypto-3.dll';
  {$ENDIF}

function ERR_get_error; external LIBEAY_DLL_NAME delayed;
function ERR_error_string; external LIBEAY_DLL_NAME delayed;
//
function ASN1_STRING_get0_data; external LIBEAY_DLL_NAME delayed;
function ASN1_STRING_length; external LIBEAY_DLL_NAME delayed;
//
function BIO_new; external LIBEAY_DLL_NAME delayed;
function BIO_new_file; external LIBEAY_DLL_NAME delayed;
function BIO_free; external LIBEAY_DLL_NAME delayed;
function BIO_ctrl; external LIBEAY_DLL_NAME delayed;
function BIO_read; external LIBEAY_DLL_NAME delayed;
function BIO_write; external LIBEAY_DLL_NAME delayed;
function BIO_s_mem; external LIBEAY_DLL_NAME delayed;
//
function BN_new; external LIBEAY_DLL_NAME delayed;
function BN_bn2hex; external LIBEAY_DLL_NAME delayed;
function BN_bin2bn; external LIBEAY_DLL_NAME delayed;
function BN_bn2bin; external LIBEAY_DLL_NAME delayed;
function BN_num_bits; external LIBEAY_DLL_NAME delayed;
procedure BN_free; external LIBEAY_DLL_NAME delayed;
//
function OSSL_PARAM_BLD_new; external LIBEAY_DLL_NAME delayed;
procedure OSSL_PARAM_BLD_free; external LIBEAY_DLL_NAME delayed;
function OSSL_PARAM_BLD_push_BN; external LIBEAY_DLL_NAME delayed;
function OSSL_PARAM_BLD_to_param; external LIBEAY_DLL_NAME delayed;
procedure OSSL_PARAM_free; external LIBEAY_DLL_NAME delayed;
//
function EVP_MD_CTX_new; external LIBEAY_DLL_NAME delayed;
procedure EVP_MD_CTX_free; external LIBEAY_DLL_NAME delayed;
function EVP_sha256; external LIBEAY_DLL_NAME delayed;
function EVP_PKEY_new; external LIBEAY_DLL_NAME delayed;
procedure EVP_PKEY_free; external LIBEAY_DLL_NAME delayed;
function EVP_PKEY_Q_keygen; external LIBEAY_DLL_NAME delayed;
function EVP_PKEY_get_bn_param; external LIBEAY_DLL_NAME delayed;
function EVP_PKEY_set_bn_param; external LIBEAY_DLL_NAME delayed;
function EVP_PKEY_CTX_new_from_name; external LIBEAY_DLL_NAME delayed;
procedure EVP_PKEY_CTX_free; external LIBEAY_DLL_NAME delayed;
function EVP_PKEY_fromdata_init; external LIBEAY_DLL_NAME delayed;
function EVP_PKEY_fromdata; external LIBEAY_DLL_NAME delayed;
function EVP_DigestSignInit; external LIBEAY_DLL_NAME delayed;
function EVP_DigestSignUpdate; external LIBEAY_DLL_NAME delayed;
function EVP_DigestSignFinal; external LIBEAY_DLL_NAME delayed;
function EVP_DigestVerifyInit; external LIBEAY_DLL_NAME delayed;
function EVP_DigestVerifyUpdate; external LIBEAY_DLL_NAME delayed;
function EVP_DigestVerifyFinal; external LIBEAY_DLL_NAME delayed;
//
function PEM_read_bio_PrivateKey; external LIBEAY_DLL_NAME delayed;
function PEM_write_bio_PrivateKey; external LIBEAY_DLL_NAME delayed;
function PEM_read_bio_X509; external LIBEAY_DLL_NAME delayed;
//
function X509_NAME_add_entry_by_txt; external LIBEAY_DLL_NAME delayed;
function X509_REQ_new; external LIBEAY_DLL_NAME delayed;
function X509_REQ_get_subject_name; external LIBEAY_DLL_NAME delayed;
function X509_REQ_set_pubkey; external LIBEAY_DLL_NAME delayed;
function X509_REQ_sign; external LIBEAY_DLL_NAME delayed;
function X509_get0_notAfter; external LIBEAY_DLL_NAME delayed;
procedure X509_free; external LIBEAY_DLL_NAME delayed;
function i2d_X509_REQ_bio; external LIBEAY_DLL_NAME delayed;
//
function SHA256; external LIBEAY_DLL_NAME delayed;
function SHA512; external LIBEAY_DLL_NAME delayed;
function PKCS5_PBKDF2_HMAC_SHA1; external LIBEAY_DLL_NAME delayed;
function PKCS5_PBKDF2_HMAC; external LIBEAY_DLL_NAME delayed;
procedure CRYPTO_free; external LIBEAY_DLL_NAME delayed;
function RAND_bytes; external LIBEAY_DLL_NAME delayed;

// The following functions are the OpenSSL macro definitions
// which are not exported in the DLL
function ERR_GetErrorMessage: String;
var
  locErrMsg: array [0 .. 160] of Char;
begin
  ERR_error_string(ERR_get_error, @locErrMsg);
  Result := String(System.AnsiStrings.StrPas(PAnsiChar(@locErrMsg)));
end;

function BIO_pending(b : pBIO): int;
begin
  Result := BIO_ctrl(b, BIO_CTRL_PENDING,0,nil);
end;

procedure OPENSSL_free(addr: pointer);
begin
  CRYPTO_free(Addr, nil, 0);
end;

function BN_num_bytes(const a: pBIGNUM): integer;
begin
  Result := (BN_num_bits(a)+7) div 8;
end;

procedure CheckOpenSSL(Res: int);
begin
  {$IFDEF DEBUG}
  if Res<>1 then
    raise Exception.Create(ERR_GetErrorMessage);
  {$ENDIF}
end;


end.

