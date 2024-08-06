unit DWL.HTTP.APIClient.UserPw;

interface

uses
  DWL.HTTP.APIClient, System.JSON;

type
  TdwlAPIUserNamePasswordAuthorizerCallBackAction = (acapwNone, acapwGetUserNamePassword, acapwGetRefreshtoken, acapwNewAccesstoken, acapwNewRefreshtoken, acapwInvalidateAuthorization);
  TdwlAPIUserNamePasswordAuthorizerCallBackProc = procedure(var Username, Password, Token: string; Action: TdwlAPIUserNamePasswordAuthorizerCallBackAction; JSONResponse: TJSONValue) of object;

function New_UserPwAuthorizer(const Authorization_Endpoint: string; CallBackProc: TdwlAPIUserNamePasswordAuthorizerCallBackProc): IdwlAPIAuthorizer;

implementation

uses
  DWL.HTTP.Client, DWL.HTTP.Consts, System.SysUtils, System.NetEncoding,
  Winapi.WinInet, Winapi.Windows, DWL.MediaTypes;

type
  TdwlAPIUserNamePasswordAuthorizer = class(TdwlAPIAuthorizerWithRefreshToken)
  strict private
    FCallBackProc: TdwlAPIUserNamePasswordAuthorizerCallBackProc;
    FAuthorization_Endpoint: string;
    procedure ParentCallBack(var Token: string; Action: TdwlAPIAuthorizerCallBackAction);
    procedure ProcessAccessTokenFromJSON(JsonVal: TJSONValue);
  protected
    procedure AcquireAccessToken; override;
    procedure AcquireRefreshtoken; override;
  public
    constructor Create(const Authorization_Endpoint: string; CallbackProc: TdwlAPIUserNamePasswordAuthorizerCallBackProc);
  end;

function New_UserPwAuthorizer(const Authorization_Endpoint: string; CallBackProc: TdwlAPIUserNamePasswordAuthorizerCallBackProc): IdwlAPIAuthorizer;
begin
  Result := TdwlAPIUserNamePasswordAuthorizer.Create(Authorization_Endpoint, CallBackproc);
end;

{ TdwlAPIUserNamePasswordAuthorizer }

procedure TdwlAPIUserNamePasswordAuthorizer.AcquireRefreshtoken;
begin
  var Username := '';
  var Password := '';
  if Assigned(FCallBackProc) then
  begin
    var Dummy: string;
    FCallBackProc(Username, Password, Dummy, acapwGetUserNamePassword, nil);
  end;
  if Username<>'' then
  begin
    var lRequest := New_HTTPRequest(FAuthorization_Endpoint+'/refreshtoken');
    lRequest.Method := HTTP_METHOD_POST;
    lRequest.WritePostData(Format('username=%s&password=%s', [TNetEncoding.URL.Encode(Username), TNetEncoding.URL.Encode(Password)]));
    lRequest.Header[HTTP_FIELD_CONTENT_TYPE] := MEDIA_TYPE_X_WWW_FORM_URLENCODED;
    var lResponse := lRequest.Execute;
    if lResponse.StatusCode=200 then
    begin
      var lTmp := lResponse.AsString;
      if lTmp='' then
        Exit;
      var JsonVal := TJsonObject.ParseJSONValue(lTmp);
      try
        var Refr_Token: string;
        if JsonVal.TryGetValue<string>('refresh_token', Refr_Token) then
        begin
          NewRefreshtoken(Refr_Token);
          // an accesstoken is also delivered as service
          ProcessAccessTokenFromJSON(JsonVal);
        end;
      finally
        JsonVal.Free;
      end;
    end;
  end;
end;

procedure TdwlAPIUserNamePasswordAuthorizer.AcquireAccessToken;
begin
  var RefreshToken := GetRefreshToken;
  // maybe getting refreshtoken already filled accesstoken
  if AccessTokenPresent then
    Exit;
  var lRequest := New_HTTPRequest(FAuthorization_Endpoint+'/accesstoken');
  lRequest.Method := HTTP_METHOD_POST;
  lRequest.WritePostData(Format('refreshtoken=%s', [GetRefreshToken]));
  lRequest.Header[HTTP_FIELD_CONTENT_TYPE] := MEDIA_TYPE_X_WWW_FORM_URLENCODED;
  var lResponse := lRequest.Execute;
  case lResponse.StatusCode of
  HTTP_STATUS_OK:
    begin
      var lTmp := lResponse.AsString;
      if lTmp='' then
        Exit;
      var JsonVal := TJsonObject.ParseJSONValue(lTmp);
      try
        ProcessAccessTokenFromJSON(JsonVal);
      finally
        JsonVal.Free;
      end;
    end;
  HTTP_STATUS_DENIED:
    InvalidateAuthorization;
  end;
end;

constructor TdwlAPIUserNamePasswordAuthorizer.Create(const Authorization_Endpoint: string; CallbackProc: TdwlAPIUserNamePasswordAuthorizerCallBackProc);
begin
  inherited Create(ParentCallBack);
  FCallBackProc := CallbackProc;
  FAuthorization_Endpoint := Authorization_Endpoint;
end;

procedure TdwlAPIUserNamePasswordAuthorizer.ParentCallBack(var Token: string; Action: TdwlAPIAuthorizerCallBackAction);
begin
  var NewAction := acapwNone;
  case Action  of
    acaGetRefreshtoken: NewAction := acapwGetRefreshtoken;
    acaNewRefreshtoken: NewAction := acapwNewRefreshtoken;
    acaInvalidateAuthorization: NewAction := acapwInvalidateAuthorization;
  end;
  if Assigned(FCallBackProc) then
  begin
    var Dummy: string;
    FCallBackProc(Dummy, Dummy, Token, NewAction, nil);
  end;
end;

procedure TdwlAPIUserNamePasswordAuthorizer.ProcessAccessTokenFromJSON(JsonVal: TJSONValue);
begin
  var Acc_Token: string;
  if JsonVal.TryGetValue<string>('access_token', Acc_Token) then
  begin
    var ExpireTick: UInt64;
    if JsonVal.TryGetValue<UInt64>('expires_in', ExpireTick) then
    begin
      ExpireTick := GetTickCount64+ExpireTick*1000;
      NewAccessToken(Acc_Token, ExpireTick);
      if Assigned(FCallBackProc) then
      begin
        var Dummy: string;
        FCallBackProc(Dummy, Dummy, Acc_Token, acapwNewAccesstoken, JsonVal);
      end;
    end;
  end;
end;

end.
