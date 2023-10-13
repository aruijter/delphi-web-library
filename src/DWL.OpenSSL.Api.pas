unit DWL.OpenSSL.Api;

{$I DWL.inc}

interface

uses
  Winapi.Windows;

{$DEFINE DELAYED}

// Please note: no library init is required anymore starting with OpenSSL version 1.1.0

const
  BIO_CTRL_PENDING = 10;

  MBSTRING_FLAG = $1000;
  MBSTRING_ASC = MBSTRING_FLAG or 1;

  RSA_F4 = $10001;

  SHA256_DIGEST_LENGTH = 32;
  SHA512_DIGEST_LENGTH = 64;

  OSSL_KEYMGMT_SELECT_PRIVATE_KEY = 1;
  OSSL_KEYMGMT_SELECT_PUBLIC_KEY = 2;
  OSSL_KEYMGMT_SELECT_DOMAIN_PARAMETERS = 4;
  OSSL_KEYMGMT_SELECT_OTHER_PARAMETERS = $80;
  OSSL_KEYMGMT_SELECT_ALL_PARAMETERS = OSSL_KEYMGMT_SELECT_DOMAIN_PARAMETERS OR OSSL_KEYMGMT_SELECT_OTHER_PARAMETERS;
  EVP_PKEY_KEY_PARAMETERS = OSSL_KEYMGMT_SELECT_ALL_PARAMETERS;
  EVP_PKEY_PUBLIC_KEY = EVP_PKEY_KEY_PARAMETERS OR OSSL_KEYMGMT_SELECT_PUBLIC_KEY;
  EVP_PKEY_KEYPAIR = EVP_PKEY_PUBLIC_KEY OR OSSL_KEYMGMT_SELECT_PRIVATE_KEY;

  SSL_CLIENT_HELLO_ERROR = 0;
  SSL_CLIENT_HELLO_SUCCESS = 1;
  SSL_CLIENT_HELLO_RETRY = -1;

  SSL_TLSEXT_ERR_OK = 0;
  SSL_TLSEXT_ERR_NOACK = 3;

  SSL_CTRL_SET_TLSEXT_SERVERNAME_CB = 53;

  TLSEXT_TYPE_server_name = 0;
  TLSEXT_TYPE_application_layer_protocol_negotiation = 16;

  SSL_FILETYPE_PEM = 1;

  SSL_CTRL_EXTRA_CHAIN_CERT = 14;

  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_WANT_CONNECT = 7;
  SSL_ERROR_WANT_ACCEPT = 8;

  SSL_VERIFY_NONE = 0;

  BIO_FLAGS_SHOULD_RETRY = 8;

  NID_X9_62_id_ecPublicKey = 408;
  NID_X9_62_prime256v1 = 415;
  NID_rsassaPss = 912;

  EVP_PKEY_ALG_CTRL = $1000;
  EVP_PKEY_OP_PARAMGEN = 2;
  EVP_PKEY_OP_KEYGEN = 4;
  EVP_PKEY_CTRL_EC_PARAMGEN_CURVE_NID = EVP_PKEY_ALG_CTRL + 1;
  EVP_PKEY_RSA_PSS = NID_rsassaPss;
  EVP_PKEY_EC = NID_X9_62_id_ecPublicKey;

  NID_key_usage = 83;
  NID_subject_key_identifier = 82;
  NID_subject_alt_name = 85;
  NID_basic_constraints = 87;
  NID_authority_key_identifier = 90;
  NID_ext_key_usage = 126;

type
  int = integer;

  pASN1_STRING = pointer;
  pASN1_TIME = pASN1_STRING;
  pASN1_INTEGER = pointer;

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
  pX509V3_CTX = pointer;
  pLHASH = pointer;
  pX509_EXTENSION = pointer;

  pSSL_METHOD = pointer;
  pSSL_CTX = pointer;
  pSSL = pointer;

  ppInteger = ^PInteger;

function ASN1_INTEGER_set_int64(a: pASN1_INTEGER; r: int64): int; cdecl;
function ASN1_STRING_get0_data(const x: pASN1_STRING): PAnsiChar; cdecl;
function ASN1_STRING_length(const x: pASN1_STRING): int; cdecl;

function BIO_ctrl(bp: pBIO; cmd: int; larg: long; parg: pointer): long; cdecl;
function BIO_free(a: pBIO): int; cdecl;
function BIO_new(_type: pBIO_METHOD): pBIO; cdecl;
function BIO_new_file(const filename: PAnsiChar; const mode: PAnsiChar): pBIO; cdecl;
function BIO_new_mem_buf(buf: pointer; len: int): pBIO; cdecl;
function BIO_pending(b : pBIO): int;
function BIO_read(b: pBIO; data: pointer; dlen: int): int; cdecl;
function BIO_s_mem: PBIO_METHOD cdecl;
function BIO_should_retry(b: pBIO): int;
function BIO_test_flags(b: pBIO; flags: int): int; cdecl;
function BIO_write(b: pBIO; data: pointer; dlen: int): int; cdecl;

function BN_bin2bn(s: PAnsiChar; len: int; ret: pBIGNUM): pBIGNUM cdecl;
function BN_bn2bin(const a: pBIGNUM; _to: PAnsiChar): int cdecl;
function BN_bn2hex(const a: pBIGNUM): PAnsiChar cdecl;
procedure BN_free(a: pBIGNUM) cdecl;
function BN_new: pBIGNUM cdecl;
function BN_num_bits(const a: PBIGNUM): int cdecl;
function BN_num_bytes(const a: pBIGNUM): int;

procedure CheckOpenSSL(Res: int);

procedure CRYPTO_free(ptr: pointer; const _file: PAnsiChar; line: int); cdecl;

function d2i_X509_bio(bp: pBIO; x: ppX509): pX509; cdecl;

function ERR_error_string(e: ulong32; buf: PAnsiChar): PAnsiChar; cdecl;
function ERR_get_error: long; cdecl;
function ERR_GetErrorMessage: string;

function EVP_DigestSignFinal(ctx: pEVP_MD_CTX; const sig: PAnsiChar; var siglen: size_t): int; cdecl;
function EVP_DigestSignInit(ctx: pEVP_MD_CTX; pctx: ppEVP_PKEY_CTX; _type: pEVP_MD; e: pENGINE; pkey: pEVP_PKEY): int; cdecl;
function EVP_DigestSignUpdate(ctx: pEVP_MD_CTX; const data: pointer; cnt: size_t): int; cdecl;
function EVP_DigestVerifyFinal(ctx: pEVP_MD_CTX; const sig: PAnsiChar; siglen: size_t): int; cdecl;
function EVP_DigestVerifyInit(ctx: pEVP_MD_CTX; pctx: ppEVP_PKEY_CTX; _type: pEVP_MD; e: pENGINE; pkey: pEVP_PKEY): int; cdecl;
function EVP_DigestVerifyUpdate(ctx: pEVP_MD_CTX; const d: pointer; cnt: size_t): int; cdecl;

procedure EVP_MD_CTX_free(ctx: pEVP_MD_CTX); cdecl;
function EVP_MD_CTX_new: pEVP_MD_CTX; cdecl;

function EVP_PKEY_CTX_ctrl(pctx: PEVP_PKEY_CTX; keytype, optype, cmd, p1: int; p2: Pointer): int; cdecl;
procedure EVP_PKEY_CTX_free(ctx: pEVP_PKEY_CTX); cdecl;
function EVP_PKEY_CTX_new_from_name(libctx: pOSSL_LIB_CTX; const name: PAnsiChar; const propquery: PAnsiChar): pEVP_PKEY_CTX; cdecl;
function EVP_PKEY_CTX_new_id(Id: int; eng: PEngine): PEVP_PKEY_CTX; cdecl;
function EVP_PKEY_CTX_set_ec_paramgen_curve_nid(pctx: PEVP_PKEY_CTX; nid: int): int;
procedure EVP_PKEY_free(pkey: PEVP_PKEY) cdecl;
function EVP_PKEY_fromdata(ctx: pEVP_PKEY_CTX; var key: pEVP_PKEY; selection: int; params: pOSSL_PARAM): int; cdecl;
function EVP_PKEY_fromdata_init(ctx: pEVP_PKEY_CTX): int; cdecl;
function EVP_PKEY_get_bn_param(pkey: pEVP_PKEY; key_name: PAnsiChar; var bn: pBIGNUM): int; cdecl;
function EVP_PKEY_keygen(pctx: PEVP_PKEY_CTX; pkey: PPEVP_PKEY): int; cdecl;
function EVP_PKEY_keygen_init(pctx: PEVP_PKEY_CTX): Integer; cdecl;
function EVP_PKEY_new: pEVP_PKEY cdecl;
function EVP_PKEY_Q_keygen(libctx: pOSSL_LIB_CTX; const propq: PAnsiChar; const _type: PAnsiChar; bits: size_t): pEVP_PKEY; cdecl;
function EVP_PKEY_set_bn_param(pkey: pEVP_PKEY; key_name: PAnsiChar; var bn: pBIGNUM): int; cdecl;

function EVP_sha256: pEVP_MD; cdecl;

function i2d_X509_REQ_bio(bp: pBIO; x: pX509_REQ): int cdecl;

function OBJ_create(oid, sn, ln:PAnsiChar): int;

procedure OPENSSL_free(addr: pointer);

procedure OSSL_PARAM_BLD_free(bld: pOSSL_PARAM_BLD); cdecl;
function OSSL_PARAM_BLD_new: pOSSL_PARAM_BLD; cdecl;
function OSSL_PARAM_BLD_push_BN(bld: pOSSL_PARAM_BLD; key: PAnsiChar; const bn: pBIGNUM): int; cdecl;
function OSSL_PARAM_BLD_to_param(bld: pOSSL_PARAM_BLD): pOSSL_PARAM; cdecl;
procedure OSSL_PARAM_free(params: pOSSL_PARAM); cdecl;

function PEM_read_bio_PrivateKey(bp: pBIO; x: ppEVP_PKEY; cb: ppem_password_cb; u: pointer): pEVP_PKEY; cdecl;
function PEM_read_bio_X509(bp: pBIO; x: ppX509; cb: ppem_password_cb; u: pointer): pX509 cdecl;
function PEM_write_bio_PrivateKey(bp : PBIO; x : pEVP_PKEY; const enc : pEVP_CIPHER; kstr: pointer; klen: int; cb: ppem_password_cb; u: pointer) : int cdecl;
function PEM_write_bio_X509(bp : PBIO; x : pX509) : int cdecl;

function PKCS5_PBKDF2_HMAC(const pass: PAnsiChar; passlen: int; const salt: PAnsiChar; saltlen: int; iter: int; const digest: pEVP_MD; keylen: int; _out: PAnsiChar): int cdecl;
function PKCS5_PBKDF2_HMAC_SHA1(const pass: PAnsiChar; passlen: int; const salt: PAnsiChar; saltlen: int; iter: int; keylen: int; _out: PAnsiChar): int cdecl;

function RAND_bytes(buf: PAnsiChar; num: int): int cdecl;

function SHA256(const d: PAnsiChar; n: size_t; md: PAnsiChar): pointer cdecl;
function SHA512(const d: PAnsiChar; n: size_t; md: PAnsiChar): pointer cdecl;


function SSL_accept(s: pSSL): int; cdecl;
function SSL_client_hello_get0_ext(s: pSSL; type_: int; out: PPAnsiChar; outlen: PSIZE_T): int; cdecl;
function SSL_client_hello_get1_extensions_present(s: pSSL; _out: PPInteger; outlen: PSIZE_T): int; cdecl;
function SSL_CTX_add_extra_chain_cert(ctx: pSSL_CTX; x509: pX509): long;
function SSL_CTX_callback_ctrl(ctx: PSSL_CTX; cb_id: int; fp: Pointer): int; cdecl;
function SSL_CTX_ctrl(ctx: pSSL_CTX; cmd: int; larg: long; parg: pointer): long; cdecl;
procedure SSL_CTX_free(ctx: pSSL_CTX); cdecl;
function SSL_CTX_new(method: pSSL_METHOD): pSSL_CTX; cdecl;
procedure SSL_CTX_set_alpn_select_cb(ctx: pSSL_CTX; cb: pointer; arg: Pointer); cdecl;
procedure SSL_CTX_set_client_hello_cb(ctx: pSSL_CTX; cb: pointer; arg: Pointer); cdecl;
procedure SSL_CTX_set_security_level(ctx: pSSL_CTX; level: int); cdecl;
function SSL_CTX_set_tlsext_servername_callback(ctx: PSSL_CTX; cb: pointer): int;
procedure SSL_CTX_set_verify(ctx: pSSL_CTX; mode: int; callback: pointer); cdecl;
function SSL_CTX_use_certificate(ctx: pSSL_CTX; cert: pX509): int; cdecl;
function SSL_CTX_use_certificate_file(ctx: pSSL_CTX; const file_: PAnsiChar; type_: int): int; cdecl;
function SSL_CTX_use_PrivateKey(ctx: pSSL_CTX; pkey: pEVP_PKEY): int; cdecl;
function SSL_CTX_use_PrivateKey_file(ctx: pSSL_CTX; const file_: PAnsiChar; type_: int): int; cdecl;
procedure SSL_free(ssl: pSSL); cdecl;
function SSL_get_error(ssl: pSSL; ret: int): int; cdecl;
function SSL_get_ex_data(d: pSSL; idx: int): pointer; cdecl;
function SSL_is_init_finished(s: pSSL): int; cdecl;
function SSL_new(ctx: pSSL_CTX): pSSL; cdecl;
function SSL_peek(ssl: pSSL; buf: pointer; num: int): int; cdecl;
function SSL_read(ssl: pSSL; buf: pointer; num: int): int; cdecl;
procedure SSL_set_accept_state(ssl: pSSL); cdecl;
procedure SSL_set_bio(ssl: pSSL; rboi: pBIO; wbio: pBIO);
function SSL_set_ex_data(d: pSSL; idx: int; arg: pointer): int; cdecl;
function SSL_set_fd(s: pSSL; fd: int): int; cdecl;
function SSL_set_SSL_CTX(s: pSSL; ctx: pSSL_CTX): pSSL_CTX; cdecl;
function SSL_shutdown(s: pSSL): int; cdecl;
function SSL_write(ssl: pSSL; buf: pointer; num: int): int; cdecl;

function TLS_method: PSSL_METHOD; cdecl;

function X509_add_ext(x: pX509; ex: pX509_EXTENSION; loc: int): int;
procedure X509_EXTENSION_free(ext: pX509_EXTENSION); cdecl;
procedure X509_free(x: pX509) cdecl;
function X509_get_serialNumber(x: pX509): pASN1_INTEGER; cdecl;
function X509_get_subject_name(x: pX509): pX509_NAME; cdecl;
function X509_get0_notAfter(x: pX509): pASN1_TIME; cdecl;
function X509_get0_notBefore(x: pX509): pASN1_TIME; cdecl;
function X509_gmtime_adj(asn1_time: pASN1_TIME; offset_sec: long): pASN1_TIME; cdecl;
function X509_NAME_add_entry_by_txt(name: pX509_NAME; const field: PAnsiChar; _type: int; const bytes: PAnsiChar; len, loc, _set: int): int; cdecl;
function X509_new: pX509; cdecl;
function X509_REQ_get_subject_name(x: pX509_REQ):pX509_NAME; cdecl;
function X509_REQ_new: pX509_REQ; cdecl;
function X509_REQ_set_pubkey(x: pX509_REQ; pkey: pEVP_PKEY): int; cdecl;
function X509_REQ_sign(x: pX509_REQ; pkey: pEVP_PKEY; const md: pEVP_MD): int; cdecl;
function X509_set_issuer_name(x: pX509; name: pX509_NAME): int;
function X509_set_pubkey(x: pX509; pkey: pEVP_PKEY): int; cdecl;
function X509_set_version(x: pX509; version: long): int; cdecl;
function X509_sign(x: pX509; pkey: pEVP_PKEY; md: pEVP_MD): int; cdecl;
function X509_verify(x: pX509; pkey: pEVP_PKEY): int; cdecl;
function X509V3_EXT_conf_nid(conf: pLHASH; ctx: pX509V3_CTX; ext_nid: int; value: PAnsiChar): pX509_EXTENSION; cdecl;

implementation

uses
  System.AnsiStrings, System.SysUtils;

const
  {$IFDEF WIN64}
  LIBCRYPTO_DLL_NAME = 'libcrypto-3-x64.dll';
  LIBSSL_DLL_NAME = 'libssl-3-x64.dll';
  {$ELSE}
  LIBCRYPTO_DLL_NAME = 'libcrypto-3.dll';
  LIBSSL_DLL_NAME = 'libssl-3.dll';
  {$ENDIF}

function ASN1_INTEGER_set_int64; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function ASN1_STRING_get0_data; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function ASN1_STRING_length; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BIO_ctrl; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BIO_free; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BIO_new; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BIO_new_file; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BIO_new_mem_buf; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BIO_read; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BIO_s_mem; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BIO_test_flags; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BIO_write; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BN_bin2bn; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BN_bn2bin; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BN_bn2hex; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure BN_free; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BN_new; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function BN_num_bits; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure CRYPTO_free; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function d2i_X509_bio; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function ERR_error_string; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function ERR_get_error; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_DigestSignFinal; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_DigestSignInit; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_DigestSignUpdate; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_DigestVerifyFinal; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_DigestVerifyInit; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_DigestVerifyUpdate; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure EVP_MD_CTX_free; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_MD_CTX_new; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_PKEY_CTX_ctrl; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure EVP_PKEY_CTX_free; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_PKEY_CTX_new_from_name; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_PKEY_CTX_new_id; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure EVP_PKEY_free; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_PKEY_fromdata; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_PKEY_fromdata_init; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_PKEY_get_bn_param; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_PKEY_keygen; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_PKEY_keygen_init; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_PKEY_new; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_PKEY_Q_keygen; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_PKEY_set_bn_param; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function EVP_sha256; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function i2d_X509_REQ_bio; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function OBJ_create; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure OSSL_PARAM_BLD_free; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function OSSL_PARAM_BLD_new; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function OSSL_PARAM_BLD_push_BN; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function OSSL_PARAM_BLD_to_param; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure OSSL_PARAM_free; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function PEM_read_bio_PrivateKey; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function PEM_read_bio_X509; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function PEM_write_bio_PrivateKey; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function PEM_write_bio_X509; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function PKCS5_PBKDF2_HMAC; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function PKCS5_PBKDF2_HMAC_SHA1; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function RAND_bytes; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SHA256; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SHA512; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_accept; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_client_hello_get0_ext; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_client_hello_get1_extensions_present; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_CTX_callback_ctrl; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_CTX_ctrl; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure SSL_CTX_free; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_CTX_new; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure SSL_CTX_set_alpn_select_cb; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure SSL_CTX_set_client_hello_cb; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure SSL_CTX_set_security_level; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure SSL_CTX_set_verify; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_CTX_use_certificate; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_CTX_use_certificate_file; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_CTX_use_PrivateKey; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_CTX_use_PrivateKey_file; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure SSL_free; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_get_error; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_get_ex_data; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_is_init_finished; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_new; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_peek; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_read; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure SSL_set_accept_state; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure SSL_set_bio; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_set_ex_data; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_set_fd; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_set_SSL_CTX; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_shutdown; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function SSL_write; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function TLS_method; external LIBSSL_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_add_ext; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure X509_EXTENSION_free; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
procedure X509_free; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_get_serialNumber; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_get_subject_name; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_get0_notAfter; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_get0_notBefore; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_gmtime_adj; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_NAME_add_entry_by_txt; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_new; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_REQ_get_subject_name; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_REQ_new; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_REQ_set_pubkey; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_REQ_sign; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_set_issuer_name; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_set_pubkey; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_set_version; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_sign; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509_verify; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}
function X509V3_EXT_conf_nid; external LIBCRYPTO_DLL_NAME {$IFDEF DELAYED} delayed;{$ENDIF}

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

function BN_num_bytes(const a: pBIGNUM): int;
begin
  Result := (BN_num_bits(a)+7) div 8;
end;

function SSL_CTX_add_extra_chain_cert(ctx: pSSL_CTX; x509: pX509): long;
begin
  Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_EXTRA_CHAIN_CERT, 0, x509)
end;

procedure CheckOpenSSL(Res: int);
begin
  {$IFDEF DEBUG}
  if Res<>1 then
    raise Exception.Create(ERR_GetErrorMessage);
  {$ENDIF}
end;

function BIO_should_retry(b: pBIO): int;
begin
  Result := BIO_test_flags(b, BIO_FLAGS_SHOULD_RETRY);
end;

function EVP_PKEY_CTX_set_ec_paramgen_curve_nid(pctx: PEVP_PKEY_CTX; nid: int): int;
begin
  Result := EVP_PKEY_CTX_ctrl(pctx, EVP_PKEY_EC, EVP_PKEY_OP_PARAMGEN OR EVP_PKEY_OP_KEYGEN, EVP_PKEY_CTRL_EC_PARAMGEN_CURVE_NID, nid, Nil)
end;

function SSL_CTX_set_tlsext_servername_callback(ctx: PSSL_CTX; cb: Pointer): int;
begin
  Result := SSL_CTX_callback_ctrl(ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_CB, cb);
end;

end.



