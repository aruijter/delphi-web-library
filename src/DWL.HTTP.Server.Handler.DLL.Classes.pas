/// <summary>
///   A base class with a lot of handling support for functionality developed
///   in a Server DLL. See the bootstrap DLL project for details. The handling
///   in the bootstrap project needs to be derived from TdwlDDLHandling, or if
///   pure OAuth2 authntication is used, form TdwlDLLHandling_OAuth2
/// </summary>
unit DWL.HTTP.Server.Handler.DLL.Classes;

interface

uses
  DWL.HTTP.Server.Types, DWL.Params, System.Generics.Collections,
  System.Classes, System.JSON, DWL.MySQL, Winapi.WinInet, DWL.OpenID;

type
  TEndpoint_HandleProc = function(const State: PdwlHTTPHandlingState): boolean of object;

  THandlingEndpoint_Props = record
    HandleProc: TEndpoint_HandleProc;
    AllowedScopes: TArray<string>;
    Params: IdwlParams;
    function ScopesAllowed(const State: PdwlHTTPHandlingState): boolean;
  end;

  THandlingEndpoints_Dictionary = TDictionary<byte, THandlingEndpoint_Props>;

  /// <summary>
  ///   This a class as base functionality for internally handling HTTP requests (in a DLL)
  /// </summary>
  TdwlDLLHandling = class
  strict private
    class var FHandlingEndpoints: TObjectDictionary<string, THandlingEndpoints_Dictionary>;
    class procedure ProcessOptions(const State: PdwlHTTPHandlingState; var Success: boolean; Cmds: THandlingEndpoints_Dictionary);
    class function Body_JSON(const State: PdwlHTTPHandlingState): TJSONObject;
  private
    class function StateParams_Scopes(const State: PdwlHTTPHandlingState): TStringList;
  protected
    class var FConfigParams: IdwlParams;
    class var FCallBackProcs: TdwlCallBackProcs;
    class procedure RegisterHandling(const Command: byte; const URI: string; const handleProc: TEndpoint_HandleProc; const AllowedScopes: TArray<string>; Params: IdwlParams=nil);
    class function StateParams(const State: PdwlHTTPHandlingState): IdwlParams;
    class function ScopeOverlap(const State: PdwlHTTPHandlingState; const Scopes: TArray<string>): boolean; overload;
    class function Get_UserId(const State: PdwlHTTPHandlingState): integer;
    /// <summary>
    ///   Gets a MySQL Session SQL access, configuration needs to be in the
    ///   configparams. Only use if really needed, the session is cached in the
    ///   state, directly calling MySQL Command is more convenient
    /// </summary>
    class function MySQLSession(const State: PdwlHTTPHandlingState): IdwlMySQLSession;
    /// <summary>
    ///   Gets MYSQLCommand for SQL access, configuration needs to be in the
    ///   configparams
    /// </summary>
    class function MySQLCommand(const State: PdwlHTTPHandlingState; const Query: string): IdwlMySQLCommand;
    /// <summary>
    ///   Tries to get a param embedded in a JSON Post request
    /// </summary>
    class function TryGetJSONParam<T>(const State: PdwlHTTPHandlingState; JSON: TJSONObject; const Key: string; var Value: T; AddJSONErrorOnFailure: boolean=false; const NewStatusCodeOnError: word=HTTP_STATUS_BAD_REQUEST): boolean;
    /// <summary>
    ///   JSON support, only use JSON directly for specific purposes, normally
    ///   use Data, Success and AddError variants, to comply with the chosen
    ///   REST result JSON structure <br />
    /// </summary>
    class function Response_JSON(const State: PdwlHTTPHandlingState): TJSONObject;
    /// <summary>
    ///   The JSON Node to fill with result Data
    /// </summary>
    class function JSON_Data(const State: PdwlHTTPHandlingState): TJSONObject;
    /// <summary>
    ///   Sets then JSON content regarding success true/false <br />
    /// </summary>
    class procedure JSON_Set_Success(const State: PdwlHTTPHandlingState; IsSuccess: boolean=true);
    /// <summary>
    ///   Add error information to the the JSON Node meant to containe error
    ///   information <br />
    /// </summary>
    class procedure JSON_AddError(const State: PdwlHTTPHandlingState; ErrorCode: integer; const ErrorStr: string; NewStatusCode: word=0);
    /// <summary>
    ///   tries to get a header value
    /// </summary>
    class function TryGetHeaderValue(const State: PdwlHTTPHandlingState; const HeaderKey: string; var Value: string; AddJSONErrorOnFailure: boolean=false; const NewStatusCodeOnError: word=HTTP_STATUS_BAD_REQUEST): boolean;
    /// <summary>
    ///   try get a request param string
    /// </summary>
    class function TryGetRequestParamStr(const State: PdwlHTTPHandlingState; const Key: string; var Value: string; AddJSONErrorOnFailure: boolean=false; const NewStatusCodeOnError: word=HTTP_STATUS_BAD_REQUEST): boolean;
    /// <summary>
    ///   try get a request param as integer
    /// </summary>
    class function TryGetRequestParamInt(const State: PdwlHTTPHandlingState; const Key: string; var Value: integer; AddJSONErrorOnFailure: boolean=false; const NewStatusCodeOnError: word=HTTP_STATUS_BAD_REQUEST): boolean;
    /// <summary>
    ///   try get a request param as Int64
    /// </summary>
    class function TryGetRequestParamInt64(const State: PdwlHTTPHandlingState; const Key: string; var Value: Int64; AddJSONErrorOnFailure: boolean=false; const NewStatusCodeOnError: word=HTTP_STATUS_BAD_REQUEST): boolean;
    /// <summary>
    ///   try get a request param as UInt64
    /// </summary>
    class function TryGetRequestParamUInt16(const State: PdwlHTTPHandlingState; const Key: string; var Value: word; AddJSONErrorOnFailure: boolean=false; const NewStatusCodeOnError: word=HTTP_STATUS_BAD_REQUEST): boolean;
    /// <summary>
    ///   try get a request param as cardinal <br />
    /// </summary>
    class function TryGetRequestParamCardinal(const State: PdwlHTTPHandlingState; const Key: string; var Value: cardinal; AddJSONErrorOnFailure: boolean=false; const NewStatusCodeOnError: word=HTTP_STATUS_BAD_REQUEST): boolean;
    /// <summary>
    ///   try get a request param as boolean
    /// </summary>
    class function TryGetRequestParamBool(const State: PdwlHTTPHandlingState; const Key: string; var Value: boolean; AddJSONErrorOnFailure: boolean=false; const NewStatusCodeOnError: word=HTTP_STATUS_BAD_REQUEST): boolean;
    /// <summary>
    ///   try get a request param as datetime
    /// </summary>
    class function TryGetRequestParamDateTime(const State: PdwlHTTPHandlingState; const Key: string; var Value: TDateTime; AddJSONErrorOnFailure: boolean=false; const NewStatusCodeOnError: word=HTTP_STATUS_BAD_REQUEST): boolean;
    /// <summary>
    ///   Gets the pointer to the payload. You can consume binary post data by
    ///   using this pointer. Please note, you're not the owner of this memory,
    ///   don't write to it!
    /// </summary>
    class function TryGetPayloadPtr(const State: PdwlHTTPHandlingState; out Data: pointer; out DataSize: Int64): boolean;
  public
    class function Authorize(const State: PdwlHTTPHandlingState): boolean; virtual;
    class procedure Configure(const Params: string); virtual;
    class procedure ProcessRequest(const State: PdwlHTTPHandlingState; var Success: boolean); virtual;
    class procedure WrapUp(const State: PdwlHTTPHandlingState); virtual;
  end;

  TdwlDLLHandling_OpenID = class(TdwlDLLHandling)
  strict private
    class var FOIDC_Client: TdwlOIDC_Client;
  public
    class function Authorize(const State: PdwlHTTPHandlingState): boolean; override;
    class procedure Configure(const Params: string); override;
    class procedure WrapUp(const State: PdwlHTTPHandlingState); override;
  end;

implementation

uses
  DWL.HTTP.Consts, System.SysUtils, DWL.JOSE, DWL.Types, DWL.Params.Consts,
  System.Rtti, System.DateUtils, DWL.Logging, DWL.Logging.API,
  DWL.HTTP.Server.Utils, DWL.HTTP.Server.Globals;

const
  Param_Body_JSON='body_json';
  Param_Scopes='scopes';

type
  PBaseInternalHandlingStructure = ^TBaseInternalHandlingStructure;
  TBaseInternalHandlingStructure = record
    Response_JSON: TJSONObject;
    Params: IdwlParams;
  end;

{ THandler_Base_Internal }

class function TdwlDLLHandling.Authorize(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := State.Command=dwlhttpOPTIONS;
end;

class function TdwlDLLHandling.Body_JSON(const State: PdwlHTTPHandlingState): TJSONObject;
begin
  var V: TValue;
  if not (StateParams(State).TryGetValue(Param_Body_JSON, V) and V.TryAsType(Result, false)) then
  begin
    Result := nil;
    var CType: string;
    if TryGetHeaderValue(State, HTTP_HEADER_CONTENT_TYPE, CType) and SameText(Copy(CType, 1, CONTENT_TYPE_JSON.Length), CONTENT_TYPE_JSON) then
    begin
      var PostData: pointer;
      var PostDataSize: Int64;
      if TryGetPayloadPtr(State, PostData, PostDataSize) then
      begin
        try
          var Res := TJSONObject.ParseJSONValue(PostData, 0, PostDataSize, [TJSONObject.TJSONParseOption.IsUTF8]);
          if Res is TJSONObject then
            Result := TJSONObject(Res)
          else
            Res.Free;
        except
          Result := nil;
        end;
      end;
    end;
    // always return an JSONObject, even an emtpy one if needed
    if Result=nil then
      Result := TJSONObject.Create;
    StateParams(State).WriteValue(Param_Body_JSON, TValue.From(Result));
  end;
end;

class procedure TdwlDLLHandling.Configure(const Params: string);
begin
  FHandlingEndpoints := TObjectDictionary<string, THandlingEndpoints_Dictionary>.Create([doOwnsValues]);
  FConfigParams := New_Params;
  FConfigParams.WriteNameValueText(Params);
  EnableLogDispatchingToAPI(FConfigParams.StrValue(Param_BaseURI)+EndpointURI_Log, FConfigParams.StrValue(Param_LogSecret), 'general');
end;

class procedure TdwlDLLHandling.ProcessOptions(const State: PdwlHTTPHandlingState; var Success: boolean; Cmds: THandlingEndpoints_Dictionary);
begin
  var Val := 'OPTIONS';
  var Enum := Cmds.GetEnumerator;
  try
    while Enum.MoveNext do
      Val := Val+', '+dwlhttpCommandToString[Enum.Current.Key];
  finally
    Enum.Free;
  end;
  var Hdrs: string;
  State.SetHeaderValue('Access-Control-Allow-Methods', Val);
  if State.TryGetHeaderValue('Access-Control-Request-Headers', Hdrs) then
    State.SetHeaderValue('Access-Control-Allow-Headers', Hdrs);
  Success := true;
end;

class procedure TdwlDLLHandling.ProcessRequest(const State: PdwlHTTPHandlingState; var Success: boolean);
begin
  if not Success then
  begin
    var Cmds: THandlingEndpoints_Dictionary;
    if FHandlingEndpoints.TryGetValue(State.URI, Cmds) then
    begin
      if State.Command=dwlhttpOPTIONS then
        ProcessOptions(State, Success, Cmds)
      else
      begin
        var Props: THandlingEndpoint_Props;
        if Cmds.TryGetValue(State.Command, Props) then
        begin
          if not Props.ScopesAllowed(State) then
          begin
            Success := true;
            State.StatusCode := HTTP_STATUS_DENIED;
          end
          else
          begin
            if Props.Params<>nil then
              Props.Params.AssignTo(StateParams(State));
            Success := Props.HandleProc(State);
          end;
        end;
      end;
    end;
  end;
  if State._InternalHandlingStructure<>nil then
  begin
    if PBaseInternalHandlingStructure(State._InternalHandlingStructure).Response_JSON<>nil then
    begin
      State.SetContentText(PBaseInternalHandlingStructure(State._InternalHandlingStructure).Response_JSON.ToJSON, CONTENT_TYPE_JSON);
      PBaseInternalHandlingStructure(State._InternalHandlingStructure).Response_JSON.Free;
    end;
    if PBaseInternalHandlingStructure(State._InternalHandlingStructure).Params<>nil then
    begin
      var V: TValue;
      var BodyJSON: TJSONObject;
      if (StateParams(State).TryGetValue(Param_Body_JSON, V) and V.TryAsType(BodyJSON, false)) then
        BodyJSON.Free;
      var Scopes: TStringList;
      if (StateParams(State).TryGetValue(Param_Scopes, V) and V.TryAsType(Scopes, false)) then
        Scopes.Free;
      PBaseInternalHandlingStructure(State._InternalHandlingStructure).Params := nil;
    end;
    FreeMem(State._InternalHandlingStructure);
  end;
end;

class procedure TdwlDLLHandling.RegisterHandling(const Command: byte; const URI: string; const HandleProc: TEndpoint_HandleProc; const AllowedScopes: TArray<string>; Params: IdwlParams=nil);
begin
  var Cmds: THandlingEndpoints_Dictionary;
  if not FHandlingEndpoints.TryGetValue(URI, Cmds) then
  begin
    Cmds := THandlingEndpoints_Dictionary.Create;
    FHandlingEndpoints.Add(URI, Cmds);
  end;
  var Props: THandlingEndpoint_Props;
  Props.HandleProc := HandleProc;
  Props.AllowedScopes := AllowedScopes;
  Props.Params := Params;
  Cmds.Add(Command, Props);
end;

class function TdwlDLLHandling.StateParams_Scopes(const State: PdwlHTTPHandlingState): TStringList;
begin
  var Params := StateParams(State);
  var V: TValue;
  if not (Params.TryGetValue(Param_Scopes, V) and V.TryAsType(Result, false)) then
  begin
    Result := TStringList.Create;
    Result.CaseSensitive := false;
    Params.WriteValue(Param_Scopes, TValue.From(Result));
  end;
end;

class function TdwlDLLHandling.TryGetHeaderValue(const State: PdwlHTTPHandlingState; const HeaderKey: string; var Value: string; AddJSONErrorOnFailure: boolean; const NewStatusCodeOnError: word): boolean;
begin
  Result := State.TryGetHeaderValue(HeaderKey, Value);
  if AddJSONErrorOnFailure and not Result then
    JSON_AddError(State, 99, 'invalid or missing header '+HeaderKey, NewStatusCodeOnError);
end;

class function TdwlDLLHandling.TryGetJSONParam<T>(const State: PdwlHTTPHandlingState; JSON: TJSONObject; const Key: string; var Value: T; AddJSONErrorOnFailure: boolean; const NewStatusCodeOnError: word): boolean;
begin
  if JSON=nil then
    JSON := Body_JSON(State);
  Result := JSON.TryGetValue<T>(Key, Value);
  if AddJSONErrorOnFailure and not Result then
    JSON_AddError(State, 99, 'invalid or missing parameter '+Key, NewStatusCodeOnError);
end;

class function TdwlDLLHandling.TryGetPayloadPtr(const State: PdwlHTTPHandlingState; out Data: pointer; out DataSize: Int64): boolean;
begin
  Result := serverProcs.GetPayloadPtrProc(State, Data, DataSize);
end;

class function TdwlDLLHandling.TryGetRequestParamBool(const State: PdwlHTTPHandlingState; const Key: string; var Value: boolean; AddJSONErrorOnFailure: boolean; const NewStatusCodeOnError: word): boolean;
begin
  var StrVal: string;
  Result := State.TryGetRequestParamStr(Key, StrVal) and boolean.TryToParse(StrVal, Value);
  if AddJSONErrorOnFailure and not Result then
    JSON_AddError(State, 99, 'invalid or missing parameter '+Key, NewStatusCodeOnError);
end;

class function TdwlDLLHandling.TryGetRequestParamCardinal(const State: PdwlHTTPHandlingState; const Key: string; var Value: cardinal; AddJSONErrorOnFailure: boolean; const NewStatusCodeOnError: word): boolean;
begin
  var StrVal: string;
  Result := State.TryGetRequestParamStr(Key, StrVal) and cardinal.TryParse(StrVal, Value);
  if AddJSONErrorOnFailure and not Result then
    JSON_AddError(State, 99, 'invalid or missing parameter '+Key, NewStatusCodeOnError);
end;

class function TdwlDLLHandling.TryGetRequestParamDateTime(const State: PdwlHTTPHandlingState; const Key: string; var Value: TDateTime; AddJSONErrorOnFailure: boolean; const NewStatusCodeOnError: word): boolean;
begin
  try
    var StrVal: string;
    Result := State.TryGetRequestParamStr(Key, StrVal);
    if Result then
      Value := ISO8601ToDate(StrVal);
  except
    Result := false;
  end;
  if AddJSONErrorOnFailure and not Result then
    JSON_AddError(State, 99, 'invalid or missing parameter '+Key, NewStatusCodeOnError);
end;

class function TdwlDLLHandling.TryGetRequestParamInt(const State: PdwlHTTPHandlingState; const Key: string; var Value: integer; AddJSONErrorOnFailure: boolean; const NewStatusCodeOnError: word): boolean;
begin
  var StrVal: string;
  Result := State.TryGetRequestParamStr(Key, StrVal) and integer.TryParse(StrVal, Value);
  if AddJSONErrorOnFailure and not Result then
    JSON_AddError(State, 99, 'invalid or missing parameter '+Key, NewStatusCodeOnError);
end;

class function TdwlDLLHandling.TryGetRequestParamInt64(const State: PdwlHTTPHandlingState; const Key: string; var Value: Int64; AddJSONErrorOnFailure: boolean; const NewStatusCodeOnError: word): boolean;
begin
  var StrVal: string;
  Result := State.TryGetRequestParamStr(Key, StrVal) and Int64.TryParse(StrVal, Value);
  if AddJSONErrorOnFailure and not Result then
    JSON_AddError(State, 99, 'invalid or missing parameter '+Key, NewStatusCodeOnError);
end;

class function TdwlDLLHandling.TryGetRequestParamStr(const State: PdwlHTTPHandlingState; const Key: string; var Value: string; AddJSONErrorOnFailure: boolean; const NewStatusCodeOnError: word): boolean;
begin
  Result := State.TryGetRequestParamStr(Key, Value);
  if AddJSONErrorOnFailure and not Result then
    JSON_AddError(State, 99, 'invalid or missing parameter '+Key, NewStatusCodeOnError);
end;

class function TdwlDLLHandling.TryGetRequestParamUInt16(const State: PdwlHTTPHandlingState; const Key: string; var Value: word; AddJSONErrorOnFailure: boolean; const NewStatusCodeOnError: word): boolean;
begin
  var StrVal: string;
  Result := State.TryGetRequestParamStr(Key, StrVal) and word.TryParse(StrVal, Value);
  if AddJSONErrorOnFailure and not Result then
    JSON_AddError(State, 99, 'invalid or missing parameter '+Key, NewStatusCodeOnError);
end;

class function TdwlDLLHandling.Get_UserId(const State: PdwlHTTPHandlingState): integer;
begin
  Result := StrToIntDef(StateParams(State).StrValue(Param_Subject), -1);
end;

class function TdwlDLLHandling.Response_JSON(const State: PdwlHTTPHandlingState): TJSONObject;
begin
  if State._InternalHandlingStructure=nil then
    State._InternalHandlingStructure := AllocMem(SizeOf(TBaseInternalHandlingStructure));
  if PBaseInternalHandlingStructure(State._InternalHandlingStructure).Response_JSON=nil then
    PBaseInternalHandlingStructure(State._InternalHandlingStructure).Response_JSON := TJSONObject.Create;;
  Result := PBaseInternalHandlingStructure(State._InternalHandlingStructure).Response_JSON;
end;

class procedure TdwlDLLHandling.JSON_AddError(const State: PdwlHTTPHandlingState; ErrorCode: integer; const ErrorStr: string; NewStatusCode: word);
begin
  var Errors := TJSONArray(Response_JSON(State).GetValue('errors'));
  if Errors=nil then
  begin
    Errors := TJSONArray.Create;
    Response_JSON(State).AddPair('errors', Errors)
  end
  else
    Assert(Errors is TJSONArray);
  var Error := TJSONObject.Create;
  Error.AddPair('code', TJSONNumber.Create(ErrorCode));
  Error.AddPair('message', ErrorStr);
  Errors.Add(Error);
  JSON_Set_Success(State, false);
  if NewStatusCode<>0 then
    State.StatusCode := NewStatusCode;
end;

class function TdwlDLLHandling.JSON_Data(const State: PdwlHTTPHandlingState): TJSONObject;
begin
  Result := TJSONObject(Response_JSON(State).GetValue('data'));
  if Result=nil then
  begin
    Result := TJSONObject.Create;
    Response_JSON(State).AddPair('data', Result)
  end
  else
    Assert(Result is TJSONObject);
end;

class procedure TdwlDLLHandling.JSON_Set_Success(const State: PdwlHTTPHandlingState; IsSuccess: boolean);
begin
  var Pair := Response_JSON(State).Get('success');
  if Pair<>nil then
    Pair.JSONValue := TJSONBool.Create(IsSuccess)
  else
    Response_JSON(State).AddPair('success', TJSONBool.Create(IsSuccess));
end;

class function TdwlDLLHandling.MySQLCommand(const State: PdwlHTTPHandlingState; const Query: string): IdwlMySQLCommand;
begin
  Result := MySQLSession(State).CreateCommand(Query);
end;

class function TdwlDLLHandling.MySQLSession(const State: PdwlHTTPHandlingState): IdwlMySQLSession;
begin
  var V: TValue;
  var Params := StateParams(State);
  if not (Params.TryGetValue(Param_MySQLSession, V) and V.TryAsType(Result, false)) then
  begin
    Result := New_MySQLSession(FConfigParams);
    Params.WriteValue(Param_MySQLSession, TValue.From(Result));
  end;
end;

class function TdwlDLLHandling.ScopeOverlap(const State: PdwlHTTPHandlingState; const Scopes: TArray<string>): boolean;
begin
  Result := false;
  var StateScopes := StateParams_Scopes(State);
  if StateScopes=nil then
    Exit;
  for var i := 0 to High(Scopes) do
  begin
    Result := StateScopes.IndexOf(Scopes[i])>=0;
    if Result then
      Break;
  end;
end;

class function TdwlDLLHandling.StateParams(const State: PdwlHTTPHandlingState): IdwlParams;
begin
  if State._InternalHandlingStructure=nil then
    State._InternalHandlingStructure := AllocMem(SizeOf(TBaseInternalHandlingStructure));
  if PBaseInternalHandlingStructure(State._InternalHandlingStructure).Params=nil then
    PBaseInternalHandlingStructure(State._InternalHandlingStructure).Params := New_Params;
  Result := PBaseInternalHandlingStructure(State._InternalHandlingStructure).Params;
end;

class procedure TdwlDLLHandling.WrapUp(const State: PdwlHTTPHandlingState);
begin
  if (State=nil) then
    FHandlingEndpoints.Free;
end;

function THandlingEndpoint_Props.ScopesAllowed(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := (High(AllowedScopes)<0) or TdwlDLLHandling.ScopeOverlap(State, AllowedScopes);
end;

{ TdwlDLLHandling_OpenID }

class function TdwlDLLHandling_OpenID.Authorize(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := inherited Authorize(State);
  if Result then
    Exit;
  var AccessToken := '';
  var AuthStr: string;
  if TryGetHeaderValue(State, 'Authorization', AuthStr) then
  begin
    AuthStr := trim(AuthStr);
    if SameText(Copy(AuthStr, 1, 7), 'Bearer ') then
      AccessToken := trim(Copy(AuthStr, 8, MaxInt));
  end
  else
  begin  // for backward compatibility
    if not (TryGetRequestParamStr(State, 'accesstoken', AccessToken) or
      TryGetHeaderValue(State, 'accesstoken', AccessToken)) then
      AccessToken := '';
  end;
  if AccessToken='' then
    Exit;
  try
    var JWT := New_JWT_FromSerialization(AccessToken);
    if not FOIDC_Client.CheckJWT(JWT).Success then
      Exit;
    if not  (JWT.Payload.IntValues[jwtclaimEXPIRATION_TIME]>TUnixEpoch.Now) and
      SameText(JWT.Payload.Values[jwtclaimISSUER], FOIDC_Client.Issuer_Uri) then
      Exit;
    // all ok store authentication information
    var JWTScopes := JWT.Payload.Values[jwt_key_SCOPE].Split([' ']);
    var Scopes := StateParams_Scopes(State);
    for var Scope in  JWTScopes do
      Scopes.Add(Scope);
    // write userid in state var, to be deprecated at some point
    // add logging info to state
    var Subject := JWT.Payload.Values[jwtclaimSUBJECT];
    StateParams(State).WriteValue(Param_Subject, Subject);
    Result := true;
  except
  end;
end;

class procedure TdwlDLLHandling_OpenID.Configure(const Params: string);
begin
  inherited Configure(Params);
  // get the Issuer,
  var Issuer := FConfigParams.StrValue(Param_Issuer);
  if Issuer='' then
  begin
    Issuer :=  FConfigParams.StrValue(Param_BaseURI)+Default_EndpointURI_OAuth2;
    TdwlLogger.Log('No issuer configured, default applied: '+Issuer, lsNotice);
  end;
  // we only need this client for JWT checking purposes
  FOIDC_Client := TdwlOIDC_Client.Create(Issuer, '', '');
end;

class procedure TdwlDLLHandling_OpenID.WrapUp(const State: PdwlHTTPHandlingState);
begin
  if State=nil then
    FOIDC_Client.Free;
  inherited WrapUp(State);
end;

end.
