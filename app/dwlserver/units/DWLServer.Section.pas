unit DWLServer.Section;

{$I DWL.inc}

interface

uses
  DWL.Server, DWL.Params, DWL.MySQL, DWL.TCP.SSL, DWL.TCP.Server,
  DWL.SyncObjs, DWL.Server.Handler.Log, DWL.Logging, DWL.TCP.HTTP,
  System.SyncObjs;

type
  TDWLServerSection = class
  strict private
    FLogLevel: byte;
    FACMECheckThread: TdwlThread;
    FLogHandler: TdwlHTTPHandler_Log;
    FCallBackLogDispatcher: IdwlLogDispatcher;
    FServerStarted: boolean;
    FServerStarting: boolean;
    FRequestLoggingParams: IdwlParams;
    class procedure InsertOrUpdateDbParameter(Session: IdwlMySQLSession; const Key, Value: string);
    procedure DoLog(LogItem: PdwlLogItem);
    procedure LogRequest(Request: TdwlHTTPSocket);
    function Start_InitDataBase(ConfigParams: IdwlParams): IdwlMySQLSession;
    procedure Start_ReadParameters_CommandLine_IniFile(ConfigParams: IdwlParams);
    procedure Start_ReadParameters_MySQL(Session: IdwlMySQLSession; ConfigParams: IdwlParams);
    procedure Start_ProcessBindings(ConfigParams: IdwlParams);
    procedure Start_LoadDLLHandlers(Session: IdwlMySQLSession; ConfigParams: IdwlParams);
    procedure Start_Enable_Logging(ConfigParams: IdwlParams);
    procedure Start_LoadURIAliases(Session: IdwlMySQLSession);
  private
    FServer: TDWLServer;
    class procedure CheckACMEConfiguration(Server: TdwlTCPServer; ConfigParams: IdwlParams);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure StartServer;
    procedure StopServer;
  end;

implementation

uses
  DWL.Params.Utils, System.SysUtils, Winapi.ActiveX,
  System.Win.ComObj, DWL.Server.Consts, System.Rtti, System.Classes,
  DWL.Params.Consts, DWL.HTTP.Consts, DWL.ACME, DWL.OpenSSL,
  System.Math, DWL.TCP.Consts, System.StrUtils, Winapi.Windows,
  System.Threading, DWL.Logging.Callback, Winapi.ShLwApi, DWL.Mail.Queue,
  DWL.Server.Handler.Mail, DWL.Server.Handler.DLL, Winapi.WinInet,
  System.NetEncoding;

const
  TOPIC_BOOTSTRAP = 'bootstrap';
  TOPIC_WRAPUP = 'wrapup';
  TOPIC_DLL = 'dll';
  TOPIC_ACME = 'acme';

  httpLogLevelEmergency = 0;
  httplogLevelFailedRequests=3;
  httplogLevelWarning=4;
  httplogLevelAllRequests=6;
  httplogLevelDebug=6;
  httplogLevelEverything=9;

  Param_LogLevel = 'loglevel'; ParamDef_LogLevel = httplogLevelWarning;

type
  TACMECheckThread = class(TdwlThread)
  strict private
    FSection: TDWLServerSection;
    FConfigParams: IdwlParams;
  protected
    procedure Execute; override;
  public
    constructor Create(Section: TDWLServerSection; ConfigParams: IdwlParams);
  end;

{ TDWLServerSection }

procedure TDWLServerSection.AfterConstruction;
begin
  inherited AfterConstruction;
  FServer := TDWLServer.Create;
  FServer.OnLog := LogRequest;
end;

procedure TDWLServerSection.BeforeDestruction;
begin
  StopServer;
  FServer.Free;
  inherited BeforeDestruction;
end;

class procedure TDWLServerSection.CheckACMEConfiguration(Server: TdwlTCPServer; ConfigParams: IdwlParams);
const
  SQL_GetHostNames =
    'SELECT HostName, Cert, PrivateKey, CountryCode, State, City, BindingIp, Id FROM dwl_hostnames';
  GetHostNames_Idx_HostName=0; GetHostNames_Idx_Cert=1; GetHostNames_Idx_PrivateKey=2;
  GetHostNames_Idx_CountyCode=3; GetHostNames_Idx_State=4; GetHostNames_Idx_City=5; GetHostNames_Idx_BindingIp=6; GetHostNames_Idx_Id=7;
  SQL_Update_Cert =
    'UPDATE dwl_hostnames SET Cert=?, PrivateKey=? WHERE Id=?';
  Update_Cert_Idx_Cert=0; Update_Cert_Idx_PrivateKey=1; Update_Cert_Idx_Id=2;
begin
  var SslIoHandler: IdwlSslIoHandler;
  var HostNames := '';
  if not Supports(Server.IOHandler, IdwlSslIoHandler, SslIoHandler)  then
    SslIoHandler := nil;
  var ACMECLient := TdwlACMEClient.Create;
  try
    var AccountKey := ConfigParams.StrValue(Param_ACME_Account_Key);
    if AccountKey<>'' then
      ACMEClient.AccountPrivateKey := TdwlOpenSSL.New_PrivateKey_FromPEMStr(AccountKey);
    var Session := New_MySQLSession(ConfigParams);
    var Cmd := Session.CreateCommand(SQL_GetHostNames);
    Cmd.Execute;
    while Cmd.Reader.Read do
    begin
      var HostName := Cmd.Reader.GetString(GetHostNames_Idx_HostName);
      if HostNames<>'' then
        Hostnames := HostNames+',';
      HostNames := Hostnames+HostName;
      if SslIoHandler=nil then // we need one!
      begin
        Server.IOHandler := TdwlSslIoHandler.Create;
        SslIoHandler := Server.IOHandler as IdwlSslIoHandler;
      end;
      ACMECLient.Domain := HostName;
      ACMECLient.ChallengeIP := Cmd.Reader.GetString(GetHostNames_Idx_BindingIp, true);
      // First Check ACME Certificate
      var Certificate := Cmd.Reader.GetString(GetHostNames_Idx_Cert, true);
      var PrivateKey := Cmd.Reader.GetString(GetHostNames_Idx_PrivateKey, true);
      if Certificate<>'' then
      begin
        ACMEClient.Certificate := Certificate;
        ACMEClient.PrivateKey := TdwlOpenSSL.New_PrivateKey_FromPEMStr(PrivateKey);
      end
      else
      begin
        ACMEClient.Certificate := '';
        ACMECLient.PrivateKey := nil;
      end;
      ACMEClient.CheckCertificate;
      ACMECLient.LogCertificateStatus;
      if ACMECLient.CertificateStatus=certstatOk then
      begin
        if SslIoHandler.Environment.GetContext(ACMECLient.Domain)=nil then
          SslIoHandler.Environment.AddContext(ACMECLient.Domain, Certificate, PrivateKey);
        Continue;
      end;
      // Hostname found without or with an old certificate: do a retrieve
      ACMEClient.ProfileCountryCode := Cmd.Reader.GetString(GetHostNames_Idx_CountyCode);
      ACMEClient.ProfileState := Cmd.Reader.GetString(GetHostNames_Idx_State);
      ACMEClient.ProfileCity := Cmd.Reader.GetString(GetHostNames_Idx_City);
      ACMECLient.CheckAndRetrieveCertificate;
      if (AccountKey='') and (ACMECLient.AccountPrivateKey<>nil) then
      begin
        AccountKey := ACMECLient.AccountPrivateKey.PEMString;
        InsertOrUpdateDbParameter(Session, Param_ACME_Account_Key, AccountKey);
        ConfigParams.WriteValue(Param_ACME_Account_Key, AccountKey);
      end;
      if ACMEClient.CertificateStatus=certstatOk then
      begin // process newly retrieved certificate
        Certificate := ACMECLient.Certificate;
        PrivateKey := ACMECLient.PrivateKey.PEMString;
        var CmdUpdate := Session.CreateCommand(SQL_Update_Cert);
        CmdUpdate.Parameters.SetTextDataBinding(Update_Cert_Idx_Cert, Certificate);
        CmdUpdate.Parameters.SetTextDataBinding(Update_Cert_Idx_PrivateKey, PrivateKey);
        CmdUpdate.Parameters.SetIntegerDataBinding(Update_Cert_Idx_Id, Cmd.Reader.GetInteger(GetHostNames_Idx_Id));
        CmdUpdate.Execute;
      end;
      if ACMEClient.CertificateStatus in [certstatAboutToExpire, certstatOk] then
        SslIoHandler.Environment.AddContext(ACMECLient.Domain, Certificate, PrivateKey);
    end;
  finally
    ACMEClient.Free;
  end;
  ConfigParams.WriteValue(Param_Hostnames, HostNames);
end;

procedure TDWLServerSection.DoLog(LogItem: PdwlLogItem);
begin
  if FLogHandler<>nil then
    FLogHandler.SubmitLog('', integer(LogItem.SeverityLevel), LogItem.Source, LogItem.Channel,
      LogItem.Topic, LogItem.Msg, LogItem.ContentType, LogItem.Content);
end;

class procedure TDWLServerSection.InsertOrUpdateDbParameter(Session: IdwlMySQLSession; const Key, Value: string);
const
  SQL_InsertOrUpdateParameter=
    'INSERT INTO dwl_parameters (`Key`, `Value`) VALUES (?, ?) ON DUPLICATE KEY UPDATE `Value`=VALUES(`Value`)';
  InsertOrUpdateParameter_Idx_Key=0; InsertOrUpdateParameter_Idx_Value=1;
begin
  var Cmd := Session.CreateCommand(SQL_InsertOrUpdateParameter);
  Cmd.Parameters.SetTextDataBinding(InsertOrUpdateParameter_Idx_Key, Key);
  Cmd.Parameters.SetTextDataBinding(InsertOrUpdateParameter_Idx_Value, Value);
  Cmd.Execute;
end;

procedure TDWLServerSection.LogRequest(Request: TdwlHTTPSocket);
const
  SQL_InsertRequest =
    'INSERT INTO dwl_log_requests (Method, StatusCode, IP_Remote, Uri, ProcessingTime, RequestHeader, RequestParams) VALUES (?,?,?,?,?,?,?)';
  InsertRequest_Idx_Method=0;  InsertRequest_Idx_StatusCode=1;  InsertRequest_Idx_IP_Remote=2;  InsertRequest_Idx_Uri=3;
  InsertRequest_Idx_ProcessingTime=4; InsertRequest_Idx_Header=5; InsertRequest_Idx_Params=6;
begin
  try
    // in debugging always log everything
    {$IFNDEF DEBUG}
    if FLogLevel<httplogLevelFailedRequests then
      Exit;
    if (FLogLevel<httplogLevelAllRequests) and
      ((Request.StatusCode=HTTP_STATUS_OK) or (Request.StatusCode=HTTP_STATUS_REDIRECT)) then
      Exit;
    {$ENDIF}
    var Ticks := GetTickCount64-Request.TickStart;
    var RequestMethodStr := dwlhttpMethodToString[Request.RequestMethod];
    // in debugging log to Server Console
    {$IFDEF DEBUG}
    var LogItem := TdwlLogger.PrepareLogitem;
    LogItem.Msg :=  Request.IP_Remote+':'+Request.Port_Local.ToString+' '+
      RequestMethodStr+' '+Request.Uri+' '+Request.StatusCode.ToString+
        ' ('+Ticks.ToString+'ms)';
    Logitem.Topic := 'requests';
    LogItem.SeverityLevel := lsDebug;
    LogItem.Destination := logdestinationServerConsole;
    TdwlLogger.Log(LogItem);
    {$ENDIF}
    // clear sensitive information before logging
    Request.RequestParams.Values['password'] := '';
    // log request to table
    var RequestParamsText: string := '';
    var Prms := Request.RequestParams;
    for var i := 0 to Prms.Count-1 do
      RequestParamsText := RequestParamsText+Prms.Names[i]+'='+TNetEncoding.URL.Decode(Prms.ValueFromIndex[i])+#13#10;
    var Cmd := New_MySQLSession(FRequestLoggingParams).CreateCommand(SQL_InsertRequest);
    Cmd.Parameters.SetTextDataBinding(InsertRequest_Idx_Method, RequestMethodStr);
    Cmd.Parameters.SetIntegerDataBinding(InsertRequest_Idx_StatusCode, Request.StatusCode);
    Cmd.Parameters.SetTextDataBinding(InsertRequest_Idx_IP_Remote, Request.Ip_Remote);
    Cmd.Parameters.SetTextDataBinding(InsertRequest_Idx_Uri, Request.Uri);
    Cmd.Parameters.SetIntegerDataBinding(InsertRequest_Idx_ProcessingTime, Min(High(word), Ticks));
    Cmd.Parameters.SetTextDataBinding(InsertRequest_Idx_Header, Request.RequestHeaders.GetAsNameValueText(false));
    Cmd.Parameters.SetTextDataBinding(InsertRequest_Idx_Params, RequestParamsText);
    Cmd.Execute;
  except
    on E: Exception do
      TdwlLogger.Log(E);
  end;
end;

procedure TDWLServerSection.Start_LoadDLLHandlers(Session: IdwlMySQLSession; ConfigParams: IdwlParams);
const
  SQL_GetHandlers =
    'SELECT id, endpoint, handler_uri, params FROM dwl_handlers ORDER BY open_order';
  GetHandlers_Idx_id=0; GetHandlers_Idx_endpoint=1; GetHandlers_Idx_handler_uri=2; GetHandlers_Idx_params=3;
  SQL_GetHandlerParameters =
    'SELECT `key`, `value` FROM dwl_handler_parameters WHERE handler_id=?';
  GetHandlerParameters_Idx_key=0; GetHandlerParameters_Idx_value=1;
  GetHandlerParameters_Binding_Handler_ID=0;
begin
  try
    var DLLBasePath := ConfigParams.StrValue(Param_DLLBasePath, ExtractFileDir(ParamStr(0)));
    var Cmd := Session.CreateCommand(SQL_GetHandlers);
    Cmd.Execute;
    while Cmd.Reader.Read do
    begin
      var EndPoint := '';
      try
        var Handler_Id := Cmd.Reader.GetInteger(GetHandlers_Idx_id);
        EndPoint := Cmd.Reader.GetString(GetHandlers_Idx_endpoint);
        var URI := Cmd.Reader.GetString(GetHandlers_Idx_handler_uri);
        var HandlerParams := New_Params;
        ConfigParams.AssignTo(HandlerParams);
        // get parameters from the simple row based NameValue memo
        HandlerParams.WriteNameValueText(Cmd.Reader.GetString(GetHandlers_Idx_params ,true));
        // get parameters from the newer dwl_handler_parameters table
        var CmdPrm := Session.CreateCommand(SQL_GetHandlerParameters);
        CmdPrm.Parameters.SetIntegerDataBinding(GetHandlerParameters_Binding_Handler_ID, Handler_Id);
        CmdPrm.Execute;
        while CmdPrm.Reader.Read do
        begin
          HandlerParams.WriteValue(
            CmdPrm.Reader.GetString(GetHandlerParameters_Idx_key),
            CmdPrm.Reader.GetString(GetHandlerParameters_Idx_value));
        end;
        // if not enabled then skip loading
        if not HandlerParams.BoolValue(Param_Enabled, true) then
          Continue;
        HandlerParams.WriteValue(Param_Endpoint, Endpoint);
        var FileName := '';
        try
          if FServer.UnRegisterHandler(EndPoint) then
            TdwlLogger.Log('Unregistered DLL Handler at endpoint '+Endpoint, lsNotice, TOPIC_DLL);
          if SameText(Copy(URI, 1, 17), 'file://localhost/') then
            FileName := DLLBasePath+ReplaceStr(Copy(URI, 17, MaxInt), '/', '\')
          else
          begin
            var Len := MAX_PATH;
            SetLength(FileName, Len);
            if PathCreateFromUrl(PChar(URI), PChar(FileName), @Len, 0)<>S_OK then
              raise Exception.Create('Invalid URI');
            SetLength(FileName, Len);
          end;
          if not FileExists(FileName) then
          begin
            TdwlLogger.Log('Missing DLL '+URI+' ('+FileName+') for endpoint '+Endpoint, lsError, TOPIC_DLL);
            Continue;
          end;
          var DLLHandle := LoadLibrary(PChar(FileName));
          if DLLHandle=0 then
            raise Exception.Create('LoadLibrary failed');
          var ProcessProc := GetProcAddress(DLLHandle, 'ProcessRequest');
          var AuthorizeProc := GetProcAddress(DLLHandle, 'Authorize');
          if Assigned(ProcessProc) and Assigned(AuthorizeProc) then
          begin
            var Handler := TdwlHTTPHandler_DLL.Create(DLLHandle, ProcessProc, AuthorizeProc, EndPoint, HandlerParams);
            FServer.RegisterHandler(EndPoint, Handler);
            TdwlLogger.Log('Registered DLL Handler '+ExtractFileName(FileName)+' at endpoint '+Endpoint, lsNotice, TOPIC_DLL);
          end
          else
          begin
            FreeLibrary(DLLHandle);
            raise Exception.Create('No ProcessRequest or Authorize function found.');
          end;
        except
          on E: Exception do
            TdwlLogger.Log('Failed loading DLL '+URI+'('+FileName+') on endpoint '+Endpoint+': '+E.Message, lsError, TOPIC_DLL);
        end;
      except
        on E: Exception do
          TdwlLogger.Log('Error loading DLL handler at '+EndPoint+': '+E.Message, lsError, TOPIC_DLL);
      end;
    end;
  except
    on E: Exception do
      TdwlLogger.Log('Error loading DLL handlers: '+E.Message, lsError,TOPIC_DLL);
  end;
end;

procedure TDWLServerSection.Start_LoadURIAliases(Session: IdwlMySQLSession);
const
  SQL_Get_UriAliases =
    'SELECT alias, uri FROM dwl_urialiases';
  Get_UriAliases_Idx_alias=0; Get_UriAliases_Idx_uri=1;
begin
  try
    FServer.ClearURIAliases;
    var Cmd := Session.CreateCommand(SQL_Get_UriAliases);
    Cmd.Execute;
    while Cmd.Reader.Read do
      FServer.AddURIAlias(Cmd.Reader.GetString(Get_UriAliases_Idx_alias), Cmd.Reader.GetString(Get_UriAliases_Idx_uri));
  except
    on E: Exception do
      TdwlLogger.Log('Error loading URI Aliases: '+E.Message, lsError, TOPIC_BOOTSTRAP);
  end;
end;

procedure TDWLServerSection.StartServer;
begin
  if FServerStarted or FServerStarting then
    Exit;
  FServerStarting := true;
  TTask.Run(procedure
  begin
    try
      var ConfigParams := New_Params;
      Start_ReadParameters_CommandLine_IniFile(ConfigParams);
      var Session := Start_InitDataBase(ConfigParams);
      Start_ReadParameters_MySQL(Session, ConfigParams);
      FServer.GlobalIssuer := ConfigParams.StrValue(Param_Issuer);
      FRequestLoggingParams := New_Params;
      ConfigParams.AssignTo(FRequestLoggingParams);
      TdwlMailQueue.Configure(ConfigParams, true);
      Start_Enable_Logging(ConfigParams);
      TdwlLogger.Log('DWL Server starting', lsTrace, TOPIC_BOOTSTRAP);
      DWL.Server.AssignServerProcs;
      CheckACMEConfiguration(FServer, ConfigParams);
      Start_ProcessBindings(ConfigParams);
      Start_LoadURIAliases(Session);

      if not FServer.IsSecure then
      begin
        FServer.OnlyLocalConnections := ConfigParams.BoolValue(Param_TestMode);
        if FServer.OnlyLocalConnections then
          TdwlLogger.Log('SERVER IS NOT SECURE, only allowing local connections', lsWarning, TOPIC_BOOTSTRAP)
        else
          TdwlLogger.Log('SERVER IS NOT SECURE, please configure or review ACME parameters', lsWarning, TOPIC_BOOTSTRAP);
      end
      else
        FServer.OnlyLocalConnections := false;
      // Time to start the server
      FServer.Active := true;
      TdwlLogger.Log('Enabled Server listening', lsTrace, TOPIC_BOOTSTRAP);
      if FServer.IsSecure or FServer.OnlyLocalConnections then
      begin
        FServer.RegisterHandler(EndpointURI_Mail,  TdwlHTTPHandler_Mail.Create(ConfigParams));
        Start_LoadDLLHandlers(Session, ConfigParams)
      end
      else
        TdwlLogger.Log('Skipped loading of handlers because server is not secure', lsWarning, TOPIC_DLL);
      FACMECheckThread := TACMECheckThread.Create(Self, ConfigParams);
      TdwlLogger.Log('DWL Server started', lsTrace, TOPIC_BOOTSTRAP);
      FServerStarted := true;
    except
      FServerStarted := false;
      FServer.Active := false;
    end;
    FServerStarting := false;
  end);
end;

procedure TDWLServerSection.Start_Enable_Logging(ConfigParams: IdwlParams);
begin
  TdwlLogger.SetDefaultOrigins('', 'dwlserver', '');
  FCallBackLogDispatcher := EnableLogDispatchingToCallback(false, DoLog);
  FLogHandler := TdwlHTTPHandler_Log.Create(ConfigParams); // init before activating DoLog!
  FServer.RegisterHandler(EndpointURI_Log,  FLogHandler);
  FLogLevel := ConfigParams.IntValue(Param_LogLevel, httplogLevelWarning);
  TdwlLogger.Log('Enabled Request logging (level '+FLogLevel.ToString+')', lsTrace, TOPIC_BOOTSTRAP);
end;

function TDWLServerSection.Start_InitDataBase(ConfigParams: IdwlParams): IdwlMySQLSession;
const
  SQL_CheckTable_Handlers =
    'CREATE TABLE IF NOT EXISTS dwl_handlers (id INT UNSIGNED AUTO_INCREMENT, open_order SMALLINT, endpoint VARCHAR(50), handler_uri VARCHAR(255), `params` TEXT NULL, INDEX `primaryindex` (`id`))';
  SQL_CheckTable_Handler_Parameters =
    'CREATE TABLE IF NOT EXISTS dwl_handler_parameters (Id SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT, handler_id SMALLINT UNSIGNED NOT NULL, `Key` VARCHAR(50) NOT NULL, `Value` TEXT, PRIMARY KEY(Id), UNIQUE INDEX HandlerKeyIndex (`handler_id`, `Key`))';
  SQL_CheckTable_UriAliases =
    'CREATE TABLE IF NOT EXISTS dwl_urialiases (id INT UNSIGNED AUTO_INCREMENT, alias VARCHAR(255), uri VARCHAR(255), PRIMARY KEY (id))';
  SQL_CheckTable_Parameters =
    'CREATE TABLE IF NOT EXISTS dwl_parameters (Id SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT, `Key` VARCHAR(50) NOT NULL, `Value` TEXT, PRIMARY KEY(Id), UNIQUE INDEX KeyIndex (`Key`))';
  SQL_CheckTable_HostNames =
    'CREATE TABLE IF NOT EXISTS dwl_hostnames (Id SMALLINT UNSIGNED NOT NULL AUTO_INCREMENT, HostName VARCHAR(50) NOT NULL, CountryCode CHAR(2) NOT NULL, State VARCHAR(50) NOT NULL, '+
    'City VARCHAR(50) NOT NULL, BindingIp VARCHAR(39), Cert TEXT, PrivateKey TEXT, PRIMARY KEY(Id), UNIQUE INDEX HostName (HostName))';
  SQL_CheckTable_Log_Requests =
    'CREATE TABLE IF NOT EXISTS dwl_log_requests (Id INT UNSIGNED NOT NULL AUTO_INCREMENT, TimeStamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, Method CHAR(7) NOT NULL, StatusCode SMALLINT UNSIGNED NOT NULL, IP_Remote CHAR(15) NOT NULL,'+
    'Uri VARCHAR(250) NOT NULL, ProcessingTime SMALLINT UNSIGNED NOT NULL, RequestHeader TEXT NOT NULL, RequestParams TEXT NOT NULL, PRIMARY KEY (Id))';
begin
  if ConfigParams.StrValue(Param_Db)='' then
    ConfigParams.WriteValue(Param_Db, 'dwl');
  ConfigParams.WriteValue(Param_CreateDatabase, true);
  ConfigParams.WriteValue(Param_TestConnection, true);
  Result := New_MySQLSession(ConfigParams);
  ConfigParams.ClearKey(Param_CreateDatabase);
  ConfigParams.ClearKey(Param_TestConnection);
  // create tables (if needed)
  Result.CreateCommand(SQL_CheckTable_Handlers).Execute;
  Result.CreateCommand(SQL_CheckTable_Handler_Parameters).Execute;
  Result.CreateCommand(SQL_CheckTable_UriAliases).Execute;
  Result.CreateCommand(SQL_CheckTable_Parameters).Execute;
  Result.CreateCommand(SQL_CheckTable_HostNames).Execute;
  Result.CreateCommand(SQL_CheckTable_Log_Requests).Execute;
end;

procedure TDWLServerSection.Start_ReadParameters_CommandLine_IniFile(ConfigParams: IdwlParams);
begin
  // at first take parameters from the commandline
  TdwlParamsUtils.Import_CommandLine(ConfigParams);
  // secondary pick parameters from inifile
  TdwlParamsUtils.Import_IniFile_Section(ConfigParams, ChangeFileExt(ParamStr(0), '.ini'),
    ConfigParams.StrValue(Param_Section_Dwl_Db, ParamDef_Section_Dwl_Db));
  // and later after Init of database parameters from the dwl database are added
end;

procedure TDWLServerSection.Start_ReadParameters_MySQL(Session: IdwlMySQLSession; ConfigParams: IdwlParams);
const
  SQL_GetParameters =
    'SELECT `Key`, `Value` FROM dwl_parameters';
  GetParameters_Idx_Key=0; GetParameters_Idx_Value=1;
begin
  // get additional parameters from database configuration
  var Cmd := Session.CreateCommand(SQL_GetParameters);
  Cmd.Execute;
  var Reader := Cmd.Reader;
  while Reader.Read do
    ConfigParams.WriteValue(Reader.GetString(GetParameters_Idx_Key), Reader.GetString(GetParameters_Idx_Value, true));
  // Set LogSecret if needed
  if not ConfigParams.ContainsKey(param_LogSecret) then
  begin
    var LogSecret := Random(MaxInt).ToHexString;
    ConfigParams.WriteValue(param_LogSecret, LogSecret);
    InsertOrUpdateDbParameter(Session, Param_LogSecret, LogSecret);
  end;
end;

procedure TDWLServerSection.Start_ProcessBindings(ConfigParams: IdwlParams);
begin
  // Apply binding information
  var BindingIP := ConfigParams.StrValue(Param_Binding_IP);
  var BindingPort := ConfigParams.IntValue(Param_Binding_Port, IfThen(Supports(FServer.IOHandler, IdwlSslIoHandler), PORT_HTTPS, PORT_HTTP));
  FServer.Bindings.Add(BindingIP, BindingPort);
  TdwlLogger.Log('Bound to '+IfThen(BindingIP='', '*', BindingIP)+':'+BindingPort.ToString, lsNotice, TOPIC_BOOTSTRAP);
end;

procedure TDWLServerSection.StopServer;
begin
  if not FServerStarted then
    Exit;
  TdwlLogger.Log('Stopping DWL Server', lsNotice, TOPIC_WRAPUP);
  FLogHandler := nil; // do not try to log when server goes down
  FreeAndNil(FACMECheckThread);
  FServer.Active := false;
  TdwlLogger.UnregisterDispatcher(FCallBackLogDispatcher);
  TdwlMailQueue.Configure(nil); // to stop sending
  FServer.Bindings.Clear;
  TdwlLogger.Log('Stopped DWL Server', lsNotice, TOPIC_WRAPUP);
  TdwlLogger.FinalizeDispatching;
  FServerStarted := false;
end;

{ TACMECheckThread }

constructor TACMECheckThread.Create(Section: TDWLServerSection; ConfigParams: IdwlParams);
begin
  FSection := Section;
  FConfigParams := New_Params;
  ConfigParams.AssignTo(FConfigParams);
  inherited Create(false);
end;

procedure TACMECheckThread.Execute;
const
  CheckDelay=24*60*60*1000{one day};
begin
  while not Terminated do
  begin
    WaitForSingleObject(FWorkToDoEventHandle, CheckDelay);
    if Terminated then
      Break;
    try
      TDWLServerSection.CheckACMEConfiguration(FSection.FServer, FConfigParams);
    except
      on E:Exception do
        TdwlLogger.Log(E, lsError, TOPIC_ACME);
    end;
  end;
end;

end.
