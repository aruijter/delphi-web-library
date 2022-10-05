unit DWL.Crypt.API;

interface

uses
  WinApi.Windows;

const
  PROV_RSA_FULL = 1;
  PROV_RSA_AES = 24;
  CRYPT_VERIFYCONTEXT = $F0000000;
  CRYPT_NEWKEYSET = 8;
  CALG_MD5 = $8003;
  HP_HASHVAL = 2;
  CALG_AES_128 = $660e;
  CALG_AES_192 = $660f;
  CALG_AES_256 = $6610;
  CALG_RSA_KEYX = $a400;
  RSA1024BIT_KEY =  $04000000;
  CRYPT_EXPORTABLE = 1;
  CUR_BLOB_VERSION = 2;
  KP_IV = 1;
  KP_MODE = 4;
  CRYPT_MODE_CBC = 1;
  AT_KEYEXCHANGE = 1;
  AT_SIGNATURE = 2;

  PUBLICKEYBLOB = $6;
  PRIVATEKEYBLOB    = $7;
  PLAINTEXTKEYBLOB = $8;

type
  HCRYPTPROV = ULONG_PTR;
  PHCRYPTPROV = ^HCRYPTPROV;
  HCRYPTKEY = ULONG_PTR;
  PHCRYPTKEY = ^HCRYPTKEY;
  HCRYPTHASH = ULONG_PTR;
  PHCRYPTHASH = ^HCRYPTHASH;
  ALG_ID = ULONG;

type
  PPUBLICKEYSTRUC = ^PUBLICKEYSTRUC;
  PUBLICKEYSTRUC = record
    bType    :BYTE;
    bVersion :BYTE;
    reserved :WORD;
    aiKeyAlg :ALG_ID;
  end;

function CryptAcquireContext(phProv: PHCRYPTPROV; pszContainer, pszProvider: LPCTSTR; dwProvType, dwFlags: DWORD): BOOL; stdcall;
function CryptCreateHash(hProv: HCRYPTPROV; Algid: ALG_ID; hKey: HCRYPTKEY; dwFlags: DWORD; phHash: PHCRYPTHASH): BOOL; stdcall;
function CryptHashData(hHash: HCRYPTHASH; pbData: PBYTE; dwDataLen, dwFlags: DWORD): BOOL; stdcall;
function CryptGetHashParam(hHash: HCRYPTHASH; dwParam: DWORD; pbData: PBYTE; pdwDataLen: PDWORD; dwFlags: DWORD): BOOL; stdcall;
function CryptDestroyHash(hHash: HCRYPTHASH): BOOL; stdcall;
function CryptReleaseContext(hProv: HCRYPTPROV; dwFlags: DWORD): BOOL; stdcall;
function CryptImportKey(hProv: HCRYPTPROV; pbData: PBYTE; dwDataLen: DWORD; hPubKey: HCRYPTKEY; dwFlags: DWORD; phKey: PHCRYPTKEY): BOOL; stdcall;
function CryptGenKey(hProv: HCRYPTPROV; Algid: ALG_ID; dwFlags: DWORD; phKey: PHCRYPTKEY): BOOL; stdcall;
function CryptDestroyKey(hKey: HCRYPTKEY): BOOL; stdcall;
function CryptSetKeyParam(hKey: HCRYPTKEY; dwParam: DWORD; pbData: PBYTE; dwFlags: DWORD): BOOL; stdcall;
function CryptEncrypt(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL; dwFlags: DWORD; pbData: PBYTE; pdwDataLen: PDWORD; dwBufLen: DWORD): BOOL; stdcall;
function CryptDecrypt(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL; dwFlags: DWORD; pbData: PBYTE; pdwDataLen: PDWORD): BOOL; stdcall;
function CryptExportKey(hKey: HCRYPTKEY; hExpKey: HCRYPTKEY; dwBlobType :DWORD; dwFlags: DWORD; pbData: PBYTE; pdwDataLen: PDWORD): BOOL; stdcall;
function CryptSignHash(hHash: HCRYPTHASH; dwKeySpec: DWORD; sDescription: PChar; dwFlags: DWORD; pbSignature: PByte; pdsSigLen: PDWORD): BOOL; stdcall;
function CryptVerifySignature(hHash: HCRYPTHASH; pbSignature: PByte; dwSigLen: DWORD; hKey: HCRYPTKEY; sDescription: PChar; dsFlags: DWORD): BOOL; stdcall;

implementation

function CryptAcquireContext; external advapi32 name 'CryptAcquireContextW';
function CryptCreateHash; external advapi32 name 'CryptCreateHash';
function CryptHashData; external advapi32 name 'CryptHashData';
function CryptGetHashParam; external advapi32 name 'CryptGetHashParam';
function CryptDestroyHash; external advapi32 name 'CryptDestroyHash';
function CryptReleaseContext; external advapi32 name 'CryptReleaseContext';
function CryptImportKey; external advapi32 name 'CryptImportKey';
function CryptGenKey; external advapi32 name 'CryptGenKey';
function CryptDestroyKey; external advapi32 name 'CryptDestroyKey';
function CryptSetKeyParam; external advapi32 name 'CryptSetKeyParam';
function CryptEncrypt; external advapi32 name 'CryptEncrypt';
function CryptDecrypt; external advapi32 name 'CryptDecrypt';
function CryptExportKey; external advapi32 name 'CryptExportKey';
function CryptSignHash; external advapi32 name 'CryptSignHashW';
function CryptVerifySignature; external advapi32 name 'CryptVerifySignatureW';

end.

