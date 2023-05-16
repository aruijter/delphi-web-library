/// <summary>
///   This is an ACME client with all functionality to request a certificate
///   from LetsEncrypt. It complies to RFC8555
/// </summary>
unit DWL.ACME;

interface

uses
  DWL.OpenSSL.Api, DWL.OpenSSL, System.SysUtils, DWL.HTTP.Consts,
  DWL.HTTP.Client, DWL.Logging, DWL.JOSE;

type
  TCertificateStatus = (certstatUnknown, certstatNotFound, certstatExpired, certstatAboutToExpire, certstatOk);

  /// <summary>
  ///   The combined State of an ACME request, only used internally
  /// </summary>
  TACMECheckState = record
    URLNewAccount: string;
    URLNewOrder: string;
    URLNewNonce: string;
    URLAccount: string;
    URLFinalize: string;
    URLCertificate: string;
    ReplayNonce: string;
    JSONWebKey: string;
  end;

  /// <summary>
  ///   <para>
  ///     This is the client to use for ACME request. It is obligated to set
  ///     the properties Domain, ProfileCountryCode, ProfileState,
  ///     ProfileCity to be able to generate a valid CSR.
  ///   </para>
  ///   <para>
  ///     After setting these properties, just call GetOrRenewCertificate.
  ///     The requested certificate and the support files (in the form of PEM files) will be placed in the appointed Directory
  ///     The whole process is logged via the DWL.Logging functionality.
  ///     Only the http challenge is supported
  ///   </para>
  /// </summary>
  TdwlACMEClient = class
  strict private
    FAccountPrivateKey: IdwlOpenSSLKey;
    FCertificate: string;
    FCertificateStatus: TCertificateStatus;
    FDomain: string;
    FCallBackPortNumber: integer;
    FPrivateKey: IdwlOpenSSLKey;
    FProfileCountryCode: string;
    FProfileCity: string;
    FProfileState: string;
    FRenewalDays: byte;
    FAPIEndpoint: string;
    FHTTPRequestSeen: boolean;
    FDaysLeft: integer;
    FChallengeIP: string;
    function DoRequest(var State: TACMECheckState; const URL: string; Method: string=HTTP_METHOD_GET; const Payload: string=''): IdwlHTTPResponse;
    function GetReplayNonce(var State:TACMECheckState): string;
    procedure Log(const Msg: string; SeverityLevel: TdwlLogSeverityLevel);
    function PrepareKey(var State:TACMECheckState): boolean;
    function InitializeACMEDirectory(var State:TACMECheckState): boolean;
    function InitAccount(var State:TACMECheckState): boolean;
    function SubmitOrderAndDoChallenges(var State: TACMECheckState): boolean;
    function CreateCSRandFinalizeOrder(var State: TACMECheckState): boolean;
    function RetrieveCertificate(var State: TACMECheckState): boolean;
  public
    /// <summary>
    ///   The Private Key of the ACME account, must be stored and set for renewals, will be created is not set
    /// </summary>
    property AccountPrivateKey: IdwlOpenSSLKey read FAccountPrivateKey write FAccountPrivateKey;
    /// <summary>
    ///   The API endpoint of the ACME service, you can set this, the default
    ///   points to the LetsEncrypt Production API
    /// </summary>
    property APIEndpoint: string read FAPIEndpoint write FAPIEndpoint;
    /// <summary>
    ///   The Callback port on which the challenge request will be received, normally no need to change is from the default value of 80
    /// </summary>
    property CallBackPortNumber: integer read FCallBackPortNumber write FCallBackPortNumber;
    /// <summary>
    ///   The actual certificate. Will be generated
    ///   automatically, but you can provide it if needed
    /// </summary>
    property Certificate: string read FCertificate write FCertificate;
    /// <summary>
    ///   The minimum remaining days when te certificate will be renewed,
    ///   defaults to 30 (The LetsEncrypt advised period)
    /// </summary>
    property RenewalDays: byte read FRenewalDays write FRenewalDays;
    /// <summary>
    ///   The full path/filename of file with the private key. Will be generated
    ///   automatically, but you can override it is needed
    /// </summary>
    property PrivateKey: IdwlOpenSSLKey read FPrivateKey write FPrivateKey;
    /// <summary>
    ///   The domain for which the certificate is requested. Please note that
    ///   the DNS of this domain must point to the interface on which the application is reachable (CN value)
    /// </summary>
    property Domain: string read FDomain write FDomain;
    /// <summary>
    ///   the value of the C entry in the certificate
    /// </summary>
    property ProfileCountryCode: string read FProfileCountryCode write FProfileCountryCode;
    /// <summary>
    ///   the value of the ST entry in the certificate
    /// </summary>
    property ProfileState: string read FProfileState write FProfileState;
    /// <summary>
    ///   the value of the L entry in the certificate
    /// </summary>
    property ProfileCity: string read FProfileCity write FProfileCity;
    /// <summary>
    ///   After checking or retrieving this property holds the remaining
    ///   validity in days
    /// </summary>
    property DaysLeft: integer read FDaysLeft;
    /// <summary>
    ///   The status of the certificate
    /// </summary>
    property CertificateStatus: TCertificateStatus read FCertificateStatus;
    /// <summary>
    ///   The IP address of the interface where the server listens for a
    ///   challenge , if omitted it will be the default interface
    /// </summary>
    property ChallengeIP: string read FChallengeIP write FChallengeIP;
    constructor Create;
    /// <summary>
    ///   Call this function to check the status of the current certificate
    /// </summary>
    function CheckCertificate: TCertificateStatus;
    /// <summary>
    ///   Logs the current status through TdwlLogger
    /// </summary>
    procedure LogCertificateStatus;
    /// <summary>
    ///   This function Gets or Renews the certificate if needed. Progress will
    ///   be logged through TdwlLogger
    /// </summary>
    function CheckAndRetrieveCertificate: boolean;
  end;

implementation

uses
  System.Classes, System.AnsiStrings, System.StrUtils,
  System.DateUtils, System.IOUtils, System.Math, System.JSON, Winapi.WinInet,
  System.Hash, Winapi.Windows,
  System.Generics.Collections, System.NetEncoding, DWL.TCP.Consts, DWL.TCP.HTTP;

const
  ProductionAPIEndpoint ='https://acme-v02.api.letsencrypt.org/directory';
  StagingAPIEndpoint = 'https://acme-staging-v02.api.letsencrypt.org/directory';
  // Please use staging endpoint if you're working on ACME implementation itself
  {$IFDEF DEBUG}
//  DefaultAPIEndpoint = StagingAPIEndpoint;
  DefaultAPIEndpoint = ProductionAPIEndpoint;
  {$ELSE}
  DefaultAPIEndpoint = ProductionAPIEndpoint;
  {$ENDIF}
  PrivateAccountKeyFileName='account.key';
  TOPIC_ACME = 'acme';

type
  TCallBackServer = class(TdwlCustomHTTPServer)
  private
    FCurrentChallengeResponse: ansistring;
  protected
    function HandleRequest(Request: TdwlHTTPSocket): boolean; override;
  end;

{ TdwlACMEClient }

function TdwlACMEClient.CheckCertificate: TCertificateStatus;
var
  pTime: pASN1_STRING;
begin
  try
    FCertificateStatus := certstatUnknown;
    FDaysLeft := -MaxInt;
    try
      if Certificate<>'' then
      begin
        var Cert := TdwlOpenSSL.New_Cert_FromPEMStr(Certificate);
        pTime := X509_get0_notAfter(Cert.X509);
        FDaysLeft := Floor(TdwlOpenSSL.ASN1_StringToDateTime(pTime)-Now);
        if FDaysLeft>RenewalDays then
          FCertificateStatus := certstatOk
        else
        begin
          if FDaysLeft>=0 then
            FCertificateStatus := certstatAboutToExpire
          else
            FCertificateStatus := certstatExpired
        end;
      end
      else
        FCertificateStatus := certstatNotFound;
    except
      On E: Exception do
        Log('Error in checking current certificate: '+E.Message, lsError);
    end;
  finally
    Result := CertificateStatus;
  end;
end;

constructor TdwlACMEClient.Create;
begin
  inherited Create;
  FCertificateStatus := certstatUnknown;
  FDaysLeft := -1;
  FRenewalDays := 30; //Let's encrypt recommendation
  FAPIEndpoint := DefaultAPIEndpoint;
  FCallBackPortNumber := PORT_HTTP;
end;

function TdwlACMEClient.DoRequest(var State: TACMECheckState; const URL: string; Method: string=HTTP_METHOD_GET; const Payload: string=''): IdwlHTTPResponse;
var
  Request: IdwlHTTPRequest;
  Nonce: string;
begin
  Request := New_HTTPRequest(URL);
  Request.Method := Method;
  if (Method=HTTP_METHOD_POST) then
  begin
    Request.Header[HTTP_FIELD_CONTENT_TYPE] := 'application/jose+json';
    var JWS := New_JWS;
    JWS.SetPayloadString(PayLoad);
    // Payload is always provided as a JWS object if Account URL is not yet available
    JWS.ProtectedHeader[joseheaderALGORITHM] := algorithm_RS256;
    if State.URLAccount='' then
      JWS.ProtectedHeader.AddJSONValue(joseheaderJSONWEBKEY, TJSONObject.ParseJSONValue(State.JSONWebKey))
    else
      JWS.ProtectedHeader[joseheaderKEYID] := State.URLAccount;
    JWS.ProtectedHeader[jwt_key_NONCE] := GetReplayNonce(State);
    JWS.ProtectedHeader[jwt_key_URL] := url;
    Request.WritePostData(JWS.Serialize(FAccountPrivateKey, jskFlattened));
  end;
  Result := Request.Execute;
  // Check for ReplayNonce;
  Nonce := Result.Header[HTTP_FIELD_REPLAY_NONCE];
  if Nonce<>'' then
    State.ReplayNonce := Nonce;
end;

function TdwlACMEClient.CreateCSRandFinalizeOrder(var State: TACMECheckState): boolean;
var
  BIO: PBIO;
  X509_Req: pX509_REQ;
  X509_Name: pX509_NAME;
  csr_LEN: integer;
  csr_TXT: ansiString;
  csr_BUF: TBytes;
  i: integer;
  Response: IdwlHTTPResponse;
  Status: string;
begin
  Result := false;
  Log('Creating CSR and Finalizing order.', lsTrace);
  // generate private key for domain
  FPrivateKey := TdwlOpenSSL.New_PrivateKey;
  // create CSR
  X509_Req := X509_REQ_new;
  X509_Name := X509_REQ_get_subject_name(X509_Req);
  X509_NAME_add_entry_by_txt(X509_Name, 'C', MBSTRING_ASC, PAnsiChar(AnsiString(ProfileCountryCode)), -1, -1, 0);
  X509_NAME_add_entry_by_txt(X509_Name, 'ST', MBSTRING_ASC, PAnsiChar(AnsiString(ProfileState)), -1, -1, 0);
  X509_NAME_add_entry_by_txt(X509_Name, 'L', MBSTRING_ASC, PAnsiChar(AnsiString(ProfileCity)), -1, -1, 0);
  X509_NAME_add_entry_by_txt(X509_Name, 'O', MBSTRING_ASC, PAnsiChar(AnsiString('')), -1, -1, 0);
  X509_NAME_add_entry_by_txt(X509_Name, 'OU', MBSTRING_ASC, PAnsiChar(AnsiString('.')), -1, -1, 0);
  X509_NAME_add_entry_by_txt(X509_Name, 'CN', MBSTRING_ASC, PAnsiChar(AnsiString(Domain)), -1, -1, 0);

  // get Certificate Signing Request.
  X509_REQ_set_pubkey(x509_req, FPrivateKey.key);
  X509_REQ_sign(X509_Req, FPrivateKey.key, EVP_sha256);
  BIO := BIO_new(BIO_s_mem);
  try
    i2d_X509_REQ_bio(BIO, x509_req);
    csr_LEN := BIO_pending(BIO);
    SetLength(csr_TXT, csr_LEN + 1);
    BIO_read(BIO, PAnsiChar(csr_TXT), csr_LEN);
  finally
    BIO_Free(BIO);
  end;
  SetLength(csr_BUF, csr_LEN);
  for i := 0 to csr_LEN - 1 do
    csr_BUF[i] := ord(csr_TXT[i+1]);

  Response := DoRequest(State, State.URLFinalize, HTTP_METHOD_POST, '{"csr":"'+TNetEncoding.Base64URL.EncodeBytesToString(csr_BUF)+'"}');
  if Response.StatusCode<>HTTP_STATUS_OK then
  begin
    Log('Error in finalize request: '+Response.AsString, lsError);
    Exit;
  end;
  var JSON := TJSONObject.ParseJSONValue(Response.AsString);
  try
    Status := JSON.GetValue<string>('status');
    if Status<>'valid' then
    begin
      Log('Wrong status in finalize request: '+Response.AsString, lsError);
      Exit;
    end;
    State.URLCertificate := JSON.GetValue<string>('certificate');
  finally
    JSON.Free;
  end;
  Result := true;
end;

function TdwlACMEClient.RetrieveCertificate(var State: TACMECheckState): boolean;
var
  Response: IdwlHTTPResponse;
begin
  Result := false;
  Response := DoRequest(State, State.URLCertificate, HTTP_METHOD_POST);
  if Response.StatusCode<>HTTP_STATUS_OK then
  begin
    Log('Error downloading Certificate: '+Response.AsString, lsError);
    Exit;
  end;
  Certificate := Response.AsString;
  Log('Certificate retrieval succeeded', lsNotice);
  Result := true;
end;

function TdwlACMEClient.CheckAndRetrieveCertificate: boolean;
begin
  if FAPIEndpoint=StagingAPIEndpoint then
    Log('Using Staging environment', lsWarning);
  try
    if CertificateStatus=certstatUnknown then
      CheckCertificate;
    if CertificateStatus in [certstatUnknown, certstatOk] then
      Exit;
    var State: TACMECheckState;
    FillChar(State, SizeOf(State), 0);
    if not PrepareKey(State) then
      Exit;
    if not InitializeACMEDirectory(State) then
      Exit;
    if not InitAccount(State) then
      Exit;
    if not SubmitOrderAndDoChallenges(State) then
      Exit;
    if not CreateCSRandFinalizeOrder(State) then
      Exit;
    if not RetrieveCertificate(State) then
      Exit;
    CheckCertificate; {To get status and date etc}
  finally
    Result := CertificateStatus in [certstatAboutToExpire, certstatOk];
  end;
end;

function TdwlACMEClient.GetReplayNonce(var State:TACMECheckState): string;
begin
  if State.ReplayNonce='' then
  begin // Get one
    try
      DoRequest(State, State.URLNewNonce, 'HEAD');
    except
      on E: Exception do
      begin
        Log('Error retrieving Nonce: '+E.Message, lsError);
      end;
    end;
  end;
  Result := State.ReplayNonce;
  State.ReplayNonce := '';
end;

function TdwlACMEClient.InitAccount(var State:TACMECheckState): boolean;
begin
  // contact information is not required, only agree to terms of service
  var Response := DoRequest(State, State.URLNewAccount, HTTP_METHOD_POST, '{"termsOfServiceAgreed":true}');
  Result := (Response.StatusCode in [HTTP_STATUS_OK, HTTP_STATUS_CREATED]);
  if Result then
    State.URLAccount := Response.Header[HTTP_FIELD_LOCATION]
  else
    Log('Account initialization failed: '+Response.AsString, lsError);
end;

function TdwlACMEClient.InitializeACMEDirectory(var State:TACMECheckState): boolean;
begin
  Result := false;
  try
    var Response := DoRequest(State, APIEndpoint);
    if Response.StatusCode<>HTTP_STATUS_OK then
    begin
      Log('Error getting API endpoint directory: '+Response.AsString, lsError);
      Exit;
    end;
    var JSON := TJSONObject.ParseJSONValue(Response.AsString);
    try
      State.URLNewAccount := JSON.GetValue<string>('newAccount');
      State.URLNewNonce := JSON.GetValue<string>('newNonce');
      State.URLNewOrder := JSON.GetValue<string>('newOrder');
    finally
      JSON.Free;
    end;
    Result := true;
  except
    on E: Exception do
    begin
      Log('Error in InitializeACMEDirectory: '+E.Message, lsError);
      Result := false;
    end;
  end;
end;

function TdwlACMEClient.SubmitOrderAndDoChallenges(var State: TACMECheckState): boolean;
var
  URLAuthorizations: TJSONArray;
  Challenges: TJSONArray;
  AuthNo: integer;
  ChallNo: integer;
  ChallType: string;
  ChallUrl: string;
  ChallToken: string;
  TickCount: UInt64;
  TimeOutTickCount: UInt64;
  CheckStatusTick: UInt64;
  Status: string;
  ChallPayLoad: string;
begin
  Result := false;
  try
    // Submit order
    var Response := DoRequest(State, State.URLNewOrder, HTTP_METHOD_POST, '{"identifiers":[{"type":"dns","value":"'+Domain+'"}]}');
    if Response.StatusCode=HTTP_STATUS_CREATED then
    begin
      //fetch order details
      var OrderJSON := TJSONObject.ParseJSONValue(Response.AsString);
      try
        State.URLFinalize := OrderJSON.GetValue<string>('finalize');
        URLAuthorizations := OrderJSON.GetValue<TJsonArray>('authorizations');
        // start authorizations
        for AuthNo := 0 to URLAuthorizations.Count-1 do
        begin
          Response := DoRequest(State, URLAuthorizations.Items[AuthNo].Value, HTTP_METHOD_POST);
          if Response.StatusCode<>HTTP_STATUS_OK then
          begin
            Log('Error getting authorization: '+Response.AsString, lsError);
            Exit;
          end;
          // fetch authorization details
          var AuthJSON := TJSONObject.ParseJSONValue(Response.AsString);
          try
            Challenges := AuthJSON.GetValue<TJsonArray>('challenges');
            // do challenges
            for ChallNo := 0 to Challenges.Count-1 do
            begin
              // fetch challenge details
              ChallType := Challenges.Items[ChallNo].GetValue<string>('type');
              // perform challenge if it s a http challenge
              if ChallType.StartsWith('http') then
              begin
                ChallToken := Challenges.Items[ChallNo].GetValue<string>('token');
                ChallUrl := Challenges.Items[ChallNo].GetValue<string>('url');
                Log('Starting HTTP challenge '+(ChallNo+1).Tostring+ ' (listening on port '+CallBackPortNumber.ToString+')', lsTrace);
                Status := '';
                FHTTPRequestSeen := false;
                var CallBackServer := TCallBackServer.Create;
                try
                  // Set Response combined token and JSONWebkey (Thumbprint)
                  CallbackServer.FCurrentChallengeResponse := ansistring(ChallToken+'.'+ TNetEncoding.Base64URL.EncodeBytesToString(THashSHA2.GetHashBytes(State.JSONWebKey.Replace(' ', ''), SHA256)));
                  CallbackServer.Bindings.Add(ChallengeIP, CallBackPortNumber);
                  // start internal server
                  CallBackServer.Active := true;
                  // start the challenge
                  TickCount := GetTickCount64;
                  TimeoutTickCount := TickCount+10000;
                  CheckStatusTick := 0;
                  // To start challenge send payload {}
                  ChallPayLoad := '{}';
                  while TickCount<TimeoutTickCount do
                  begin
                    if CheckStatusTick<TickCount then
                    begin
                      // Get the status, the first time this will start the challenge
                      var JWS := New_JWS;
                      Response := DoRequest(State, ChallURL, HTTP_METHOD_POST, ChallPayLoad);
                      // the next time we don't start the challende, just polling, so change payload to Post-As-Get
                      ChallPayLoad := '';
                      if Response.StatusCode=HTTP_STATUS_OK then
                      begin
                        var ChallJSON := TJSONObject.ParseJsonValue(Response.AsString);
                        try
                          Status := ChallJSON.GetValue<String>('status');
                        finally
                          ChallJSON.Free;
                        end;
                        if Status='valid' then // yes, we completed the challenge
                        begin
                          Log('Challenge succeeded', lsTrace);
                          Break;
                        end
                        else
                          Log('challenge status: '+Status, lsTrace);
                        if Status<>'pending' then
                          Break; // something went wrong
                      end
                      else
                      begin
                        Log('Error getting challenge status: '+Response.AsString, lsError);
                        Exit;
                      end;
                      CheckStatusTick := TickCount+1000;
                    end;
                    Sleep(100);
                    TickCount := GetTickCount64;
                  end;
                finally
                  CallBackServer.Free;
                end;
                if Status<>'valid' then
                begin
                  if FHTTPRequestSeen then
                    Log('Challenge failed (But I saw an incoming HTTP request)', lsError)
                  else
                    Log('Challenge failed, most probably we''re not publicly reachable on HTTP port 80', lsError);
                  Exit;
                end;
              end;
            end;
          finally
            AuthJSON.Free;
          end;
      end;
      finally
        OrderJSON.Free;
      end;
      Result := true;
    end
    else
      Log('Creation of order failed: '+Response.AsString, lsError);
  except
    on E: Exception do
      Log('Error in SubmitOrderAndDoChallenges: '+E.Message, lsError);
  end;
end;

procedure TdwlACMEClient.Log(const Msg: string; SeverityLevel: TdwlLogSeverityLevel);
begin
  TdwlLogger.Log(Msg, SeverityLevel, TOPIC_ACME);
 end;

procedure TdwlACMEClient.LogCertificateStatus;
begin
  case CertificateStatus of
  certstatUnknown: Log('Certificate status for domain '+Domain+' unknown', lsError);
  certstatNotFound: Log('No certificate found for domain '+Domain, lsError);
  certstatExpired: Log('Current certificate for domain '+Domain+' is expired.', lsError);
  certstatAboutToExpire: Log('Current certificate for domain '+Domain+' is about to expire: '+DaysLeft.ToString+' days left.', lsNotice);
  certstatOk: Log('Current certificate found for domain '+Domain+', still valid for '+DaysLeft.ToString+' days.', lsTrace);
  end;
end;

function TdwlACMEClient.PrepareKey(var State:TACMECheckState): boolean;
begin
  // new approach
  Result := false;
  try
    if AccountPrivateKey=nil then
    begin
      FAccountPrivateKey := TdwlOpenSSL.New_PrivateKey;
      Log('Created new account Key.', lsTrace);
    end;
    // Create the JWK to be used
    var e := FAccountPrivateKey.PublicExponent_e;
    var n := FAccountPrivateKey.Modulus_n;
    State.JSONWebKey := '{"e":"'+TNetEncoding.Base64URL.EncodeBytesToString(e)+'","kty":"RSA","n":"'+TNetEncoding.Base64URL.EncodeBytesToString(n)+'"}';
    Result := true;
  except
    on E: Exception do
    begin
      Log('Error in PrepareKey: '+E.Message, lsError);
    end;
  end;
end;

{ TCallBackServer }

function TCallBackServer.HandleRequest(Request: TdwlHTTPSocket): boolean;
begin
  Request.ResponseHeaders.WriteValue(HTTP_FIELD_CONTENT_TYPE, CONTENT_TYPE_PLAIN);
  Request.ResponseDataStream.WriteBuffer(FCurrentChallengeResponse[1], Length(FCurrentChallengeResponse));
  Result := true;
end;

end.
