// <summary>
///   Support unit for JSON Object Signing and Encryption (JOSE) as described
///   in RFC7165 <br />The applicable JSON Web Algoritms are described in
///   RFC7518 <br />
/// </summary>
unit DWL.JOSE;

interface

uses
  DWL.OpenSSL, System.SysUtils, System.JSON;

const
  // header names from RFC7515
  joseheaderALGORITHM = 'alg';
  joseheaderJWKSETURL = 'jku';
  joseheaderJSONWEBKEY = 'jwk';
  joseheaderKEYID = 'kid';
  joseheaderX509URL = 'x5u';
  joseheaderX509CERTIFICATECHAIN = 'x5c';
  joseheaderX509CERTIFICATESHA1THUMBPRINT = 'x5t';
  joseheaderX509CERTIFICATESHA256THUMBPRINT = 'x5t#256';
  joseheaderTYPE = 'typ';
  joseheaderCONTENTTYPE = 'cty';
  joseheaderCRITICAL = 'crit';
  // claim names from RFC7519
  jwtclaimISSUER = 'iss';
  jwtclaimSUBJECT = 'sub';
  jwtclaimEMAIL = 'email';
  jwtclaimNAME = 'name';
  jwtclaimAUDIENCE = 'aud';
  jwtclaimEXPIRATION_TIME = 'exp';
  jwtclaimNOT_BEFORE = 'nbf';
  jwtclaimISSUED_AT = 'iat';
  jwtclaimJWT_ID = 'jti';
  // my own defined header key names
  joseheader_type_JSONWEBTOKEN = 'jwt';
  // my own defined payload key names
  jwt_key_NONCE = 'nonce';
  jwt_key_GRanT = 'grt';
  jwt_key_ORDer = 'ord';
  jwt_key_SCOPE = 'scope';
  jwt_key_URL = 'url';
  jwt_key_USagE = 'use';
  jwt_Key_algoriTmfamilY = 'kty';

  // known values for keys
  algorithm_RS256 = 'RS256';
  algorithmfamily_RSA = 'RSA';
  usage_SIGnature = 'sig';
  algoparam_modulusN ='n';
  algoparam_exponentE ='e';

type
  TdwlJWSSerializationKind= (jskCompact, jskFlattened);

  IdwlJSONWebPart = interface
    function GetIntValues(const Name: string): integer;
    procedure SetIntValues(const Name: string; Value: integer);
    function GetValues(const Name: string): string;
    procedure SetValues(const Name, Value: string);
    property Values[const Name: string]: string read GetValues write SetValues; default;
    property IntValues[const Name: string]: integer read GetIntValues write SetIntValues;
    procedure AddJSONValue(const Name: string; JSONValue: TJSONValue);
    function ToJSON: string;
    function ToEncodedJSON: string;
  end;

  /// <summary>
  ///   A JSON Web Signature (as described in RFC7515)
  /// </summary>
  IdwlJWS = interface
    function CheckSignature(PublicKey: IdwlOpenSSLKey): boolean;
    function ProtectedHeader: IdwlJSONWebPart;
    function UnprotectedHeader: IdwlJSONWebPart;
    function GetPayload: TBytes;
    procedure SetPayload(Payload: TBytes);
    property Payload: Tbytes read GetPayload write SetPayload;
    procedure SetPayloadString(PayloadStr: string);
    function Serialize(PrivateKey: IdwlOpenSSLKey; SerializationKind: TdwlJWSSerializationKind=jskCompact): string;
  end;


  /// <summary>
  ///   a JSON Web Token (as described in RFC7519) <br />
  /// </summary>
  IdwlJWT = interface
    function CheckSignature(PublicKey: IdwlOpenSSLKey): boolean;
    function Header: IdwlJSONWebPart;
    function Payload: IdwlJSONWebPart;
    function Serialize(PrivateKey: IdwlOpenSSLKey): string;
  end;

  /// <summary>
  ///   // A JSON Web Key (as described in RFC7517}
  /// </summary>
  IdwlJWK = interface
    function OpenSSLKey: IdwlOpenSSLKey;
  end;

  /// <summary>
  ///   A dictionary of JSON Web Keys
  /// </summary>
  IdwlJWKs = interface
    function TryGetKey(const kid: string; out JWK: IdwlJWK): boolean;
  end;

function New_JWS: IdwlJWS;
function New_JWS_FromSerialization(const SerializationValue: string): IdwlJWS;
function New_JWT: IdwlJWT;
function New_JWT_FromSerialization(const SerializationValue: string): IdwlJWT;
function New_JWKs_FromJSONString(const JSONString: string): IdwlJWKs;

implementation

uses
  System.Generics.Collections, System.NetEncoding;

type
  TdwlJSONWebPart = class(TInterfacedObject, IdwlJSONWebPart)
    FJSON: TJSONObject;
  private
    function GetIntValues(const Name: string): integer;
    procedure SetIntValues(const Name: string; Value: integer);
    function GetValues(const Name: string): string;
    procedure SetValues(const Name, Value: string);
    procedure AddJSONValue(const Name: string; JSONValue: TJSONValue);
    function ToJSON: string;
    function ToEncodedJSON: string;
  public
    constructor Create(const JSONString: string='');
    destructor Destroy; override;
  end;

  TdwlJWS = class(TInterfacedObject, IdwlJWS)
  strict private
    FPartsForSignatureCheck: TArray<string>;
    FPayload: TBytes;
    FUnprotectedHeader: IdwlJSONWebPart;
  private
    function CheckSignature(PublicKey: IdwlOpenSSLKey): boolean;
    function ProtectedHeader: IdwlJSONWebPart;
    function UnprotectedHeader: IdwlJSONWebPart;
    function Serialize(PrivateKey: IdwlOpenSSLKey; SerializationKind: TdwlJWSSerializationKind=jskCompact): string;
  protected
    FProtectedHeader: IdwlJSONWebPart;
    function GetPayload: TBytes; virtual;
    procedure SetPayload(Payload: TBytes); virtual;
    procedure SetPayloadString(PayloadStr: string); virtual;
  public
    constructor Create(const CompactSerializationValue: string=''); virtual;
  end;

  TdwlJWT = class(TdwlJWS, IdwlJWT)
  strict private
    FPayload: IdwlJSONWebPart;
  private
    function Header: IdwlJSONWebPart;
    function Payload: IdwlJSONWebPart;
    function Serialize(PrivateKey: IdwlOpenSSLKey): string;
  protected
    function GetPayload: TBytes; override;
    procedure SetPayload(Payload: TBytes); override;
    procedure SetPayloadString(PayloadStr: string); override;
  public
    constructor Create(const CompactSerializationValue: string=''); override;
  end;

  TdwlJWK = class(TInterfacedObject, IdwlJWK)
  strict private
    FContent: IdwlJSONWebPart;
    FOpenSSLKey: IdwlOpenSSLKey;
  private
    function OpenSSLKey: IdwlOpenSSLKey;
    constructor Create(const ContentAsJSONString: string);
  end;

  TdwlJWKs = class(TInterfacedObject, IdwlJWKs)
  strict private
    FJWKs: TDictionary<string, IdwlJWK>;
  private
    function TryGetKey(const kid: string; out JWK: IdwlJWK): boolean;
public
    constructor Create(const JSONString: string);
    destructor Destroy; override;
  end;

{ TdwlJWS }

constructor TdwlJWS.Create(const CompactSerializationValue: string='');
begin
  inherited Create;
  if CompactSerializationValue='' then
    FProtectedHeader := TdwlJSONWebPart.Create('')
  else
  begin
    FPartsForSignatureCheck := CompactSerializationValue.Split(['.']);
    if Length(FPartsForSignatureCheck)<>3 then
    begin
      SetLength(FPartsForSignatureCheck, 0);
      raise Exception.Create('Invalid JWS Serialization');
    end;
    SetPayloadString(TNetencoding.Base64URL.Decode(FPartsForSignatureCheck[1]));
    FProtectedHeader := TdwlJSONWebPart.Create(TNetencoding.Base64URL.Decode(FPartsForSignatureCheck[0]));
  end;
end;

function TdwlJWS.GetPayload: TBytes;
begin
  Result := FPayload;
end;

function TdwlJWS.CheckSignature(PublicKey: IdwlOpenSSLKey): boolean;
begin
  Result := Length(FPartsForSignatureCheck)=3;
  if Result then
  begin
    var DataForSignature := ansistring(FPartsForSignatureCheck[0]+'.'+FPartsForSignatureCheck[1]);
    var GivenSignature := TNetencoding.Base64URL.DecodeStringToBytes(FPartsForSignatureCheck[2]);
    SetLength(FPartsForSignatureCheck, 0); // finished checking. Can only be done once
    Result := TdwlOpenSSL.VerifySignature(DataForSignature, GivenSignature, PublicKey);
  end;
end;

function TdwlJWS.ProtectedHeader: IdwlJSONWebPart;
begin
  Result := FProtectedHeader;
end;

function TdwlJWS.Serialize(PrivateKey: IdwlOpenSSLKey; SerializationKind: TdwlJWSSerializationKind=jskCompact): string;
begin
  var EncodedPayload := TNetEncoding.Base64URL.EncodeBytesToString(GetPayload);
  var EncodedProtectedHeader := FProtectedHeader.ToEncodedJSON;
  var ValueToSign := EncodedProtectedHeader+'.'+EncodedPayload;
  var Signature := TNetEncoding.Base64URL.EncodeBytesToString(TdwlOpenSSL.CalculateSignature(ansistring(ValueToSign), PrivateKey));
  case SerializationKind of
  jskCompact: Result := ValueToSign+'.'+Signature;
  jskFlattened:
    begin
      Result := '{"payload":"'+EncodedPayload+'","protected":"'+EncodedProtectedHeader+'",';
      if FUnprotectedHeader<>nil then
        Result := Result+'"header":'+FUnprotectedHeader.ToJSON+',';
      Result := Result+'"signature":"'+Signature+'"}';
    end;
  end;
end;

procedure TdwlJWS.SetPayload(Payload: TBytes);
begin
  FPayload := Payload;
end;

procedure TdwlJWS.SetPayloadString(PayloadStr: string);
begin
  FPayload := TEncoding.UTF8.GetBytes(PayloadStr);
end;

function TdwlJWS.UnprotectedHeader: IdwlJSONWebPart;
begin
  if FUnprotectedHeader=nil then
    FUnprotectedHeader := TdwlJSONWebPart.Create;
  Result := FUnprotectedHeader;
end;

{ TdwlJSONWebPart }

function TdwlJSONWebPart.ToEncodedJSON: string;
begin
  Result := TNetEncoding.Base64URL.Encode(ToJSON);
end;

function TdwlJSONWebPart.ToJSON: string;
begin
  Result := FJSON.ToJSON;
end;

constructor TdwlJSONWebPart.Create(const JSONString: string='');
begin
  inherited Create;
  if JSONString<>'' then
    FJSON := TJSONObject.ParseJSONValue(JSONString) as TJSONObject
  else
    FJSON := TJSONObject.Create;
end;

destructor TdwlJSONWebPart.Destroy;
begin
  FJSON.Free;
  inherited Destroy;
end;

function TdwlJSONWebPart.GetIntValues(const Name: string): integer;
begin
  if not FJSON.TryGetValue<integer>(Name, Result) then
    Result := 0;
end;

procedure TdwlJSONWebPart.AddJSONValue(const Name: string; JSONValue: TJSONValue);
begin
  FJSON.RemovePair(Name).Free;
  FJSON.AddPair(Name, JSONValue);
end;

function TdwlJSONWebPart.GetValues(const Name: string): string;
begin
  if not FJSON.TryGetValue<string>(Name, Result) then
    Result := '';
end;

procedure TdwlJSONWebPart.SetIntValues(const Name: string; Value: integer);
begin
  FJSON.RemovePair(Name).Free;
  FJSON.AddPair(Name, TJSONNumber.Create(Value));
end;

procedure TdwlJSONWebPart.SetValues(const Name, Value: string);
begin
  FJSON.RemovePair(Name).Free;
  FJSON.AddPair(Name, Value);
end;

{ TdwlJWKs }

constructor TdwlJWKs.Create(const JSONString: string);
begin
  inherited Create;
  FJWKs := TDictionary<string, IdwlJWK>.Create;
  var JSON := TJSONObject.ParseJSONValue(JSONString);
  try
    var Keys: TJSONArray;
    if not JSON.TryGetValue<TJSONArray>('keys', Keys) then
      raise Exception.Create('Error reading Keys from  JWKs');
    var kid: string;
    var ENum := Keys.GetEnumerator;
    try
      while ENum.MoveNext do
      begin
        if not ENum.Current.TryGetValue<string>(joseheaderKEYID, kid) then
          raise Exception.Create('Error getting kid from key');
        FJWKs.Add(kid, TdwlJWK.Create(TJSONObject(ENum.Current).ToJSON));
      end;
    finally
      ENum.Free;
    end;
  finally
    JSON.Free;
  end;
end;

destructor TdwlJWKs.Destroy;
begin
  FJWKs.Free;
  inherited Destroy;
end;

function TdwlJWKs.TryGetKey(const kid: string; out JWK: IdwlJWK): boolean;
begin
  Result := FJWKs.TryGetValue(kid, JWK);
end;

{ TdwlJWK }

constructor TdwlJWK.Create(const ContentAsJSONString: string);
begin
  inherited Create;
  FContent := TdwlJSONWebPart.Create(ContentAsJSONString);
end;

function TdwlJWK.OpenSSLKey: IdwlOpenSSLKey;
begin
  if FOpenSSLKey=nil then
    FOpenSSLKey := TdwlOpenSSL.New_PublicKey_FromModulusExponent(
      TNetencoding.Base64URL.DecodeStringToBytes(FContent.Values['n']),
      TNetencoding.Base64URL.DecodeStringToBytes(FContent.Values['e']));
  Result := FOpenSSLKey;
end;

{ TdwlJWT }

constructor TdwlJWT.Create(const CompactSerializationValue: string);
begin
  inherited Create(CompactSerializationValue);
end;

function TdwlJWT.GetPayload: TBytes;
begin
  if FPayload<>nil then
    Result :=  TEncoding.UTF8.GetBytes(FPayload.ToJSON);
end;

function TdwlJWT.Header: IdwlJSONWebPart;
begin
  Result := FProtectedHeader;
end;

function TdwlJWT.Payload: IdwlJSONWebPart;
begin
  if FPayload=nil then
    FPayload := TdwlJSONWebPart.Create;
  Result := FPayload;
end;

function TdwlJWT.Serialize(PrivateKey: IdwlOpenSSLKey): string;
begin
  Result := inherited Serialize(PrivateKey);
end;

procedure TdwlJWT.SetPayload(Payload: TBytes);
begin
  FPayload := TdwlJSONWebPart.Create(TEncoding.UTF8.GetString(Payload));
end;

procedure TdwlJWT.SetPayloadString(PayloadStr: string);
begin
  FPayload := TdwlJSONWebPart.Create(PayLoadStr);
end;

// Generic creation functions

function New_JWS: IdwlJWS;
begin
  Result := TdwlJWS.Create('');
end;

function New_JWS_FromSerialization(const SerializationValue: string): IdwlJWS;
begin
  Result := TdwlJWS.Create(SerializationValue);
end;

function New_JWT: IdwlJWT;
begin
  Result := TdwlJWT.Create('');
end;

function New_JWT_FromSerialization(const SerializationValue: string): IdwlJWT;
begin
  Result := TdwlJWT.Create(SerializationValue);
end;

function New_JWKs_FromJSONString(const JSONString: string): IdwlJWKs;
begin
  Result := TdwlJWKs.Create(JSONString);
end;

end.
