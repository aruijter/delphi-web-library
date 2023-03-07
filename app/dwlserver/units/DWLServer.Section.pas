unit DWLServer.Section;

{$I DWL.inc}

interface

uses
  DWL.Server, DWL.Params, DWL.MySQL, DWL.TCP.SSL, DWL.TCP.Server,
  DWL.SyncObjs, DWL.Server.Handler.Log, DWL.Logging;

type
  TDWLServerSection = class
  strict private
    FACMECheckThread: TdwlThread;
    FLogHandler: TdwlHTTPHandler_Log;
    FCallBackLogDispatcher: IdwlLogDispatcher;
    FServerStarted: boolean;
    FServerStarting: boolean;
    class procedure InsertOrUpdateDbParameter(Session: IdwlMySQLSession; const Key, Value: string);
    class function TryHostNameMigration(ConfigParams: IdwlParams): boolean; // 20230403: can be removed next year or so
    procedure DoLog(LogItem: PdwlLogItem);
    function Start_InitDataBase(ConfigParams: IdwlParams): IdwlMySQLSession;
    procedure Start_PrepareParameters_Phase1(ConfigParams: IdwlParams);
    procedure Start_PrepareParameters_Phase2(Session: IdwlMySQLSession; ConfigParams: IdwlParams);
    procedure Start_ProcessBindings(ConfigParams: IdwlParams);
    procedure Start_DetermineServerParams(ConfigParams: IdwlParams);
    procedure Start_LoadDLLHandlers(ConfigParams: IdwlParams);
    procedure Start_Enable_Logging(ConfigParams: IdwlParams);
    procedure Start_LoadURIAliases(ConfigParams: IdwlParams);
  private
    FServer: TDWLServer;
    class procedure CheckACMEConfiguration(Server: TdwlTCPServer; ConfigParams: IdwlParams; TryUpgrade: boolean=false);
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
  DWL.Server.Handler.Mail, DWL.Server.Handler.DLL;

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
end;

procedure TDWLServerSection.BeforeDestruction;
begin
  StopServer;
  FServer.Free;
  inherited BeforeDestruction;
end;

class procedure TDWLServerSection.CheckACMEConfiguration(Server: TdwlTCPServer; ConfigParams: IdwlParams; TryUpgrade: boolean=false);
const
  SQL_GetHostNames =
    'SELECT HostName, RootCert, Cert, PrivateKey, CountryCode, State, City, BindingIp, Id FROM dwl_hostnames';
  GetHostNames_Idx_HostName=0; GetHostNames_Idx_RootCert=1; GetHostNames_Idx_Cert=2; GetHostNames_Idx_PrivateKey=3;
  GetHostNames_Idx_CountyCode=4; GetHostNames_Idx_State=5; GetHostNames_Idx_City=6; GetHostNames_Idx_BindingIp=7; GetHostNames_Idx_Id=8;
  SQL_Update_Cert =
    'UPDATE dwl_hostnames SET RootCert=?, Cert=?, PrivateKey=? WHERE Id=?';
  Update_Cert_Idx_RootCert=0; Update_Cert_Idx_Cert=1; Update_Cert_Idx_PrivateKey=2; Update_Cert_Idx_Id=3;
begin
  var SslIoHandler: IdwlSslIoHandler;
  var HostnameSeen := false;
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
      HostnameSeen := true;
      if SslIoHandler=nil then // we need one!
      begin
        Server.IOHandler := TdwlSslIoHandler.Create;
        SslIoHandler := Server.IOHandler as IdwlSslIoHandler;
      end;
      ACMECLient.Domain := Cmd.Reader.GetString(GetHostNames_Idx_HostName);
      ACMECLient.ChallengeIP := Cmd.Reader.GetString(GetHostNames_Idx_BindingIp, true);
      // First Check ACME Certificate
      var RootCertificate := Cmd.Reader.GetString(GetHostNames_Idx_RootCert, true);
      var Certificate := Cmd.Reader.GetString(GetHostNames_Idx_Cert, true);
      var PrivateKey := Cmd.Reader.GetString(GetHostNames_Idx_PrivateKey, true);
      if Certificate<>'' then
      begin
        ACMEClient.RootCertificate := TdwlOpenSSL.New_Cert_FromPEMStr(RootCertificate);
        ACMEClient.Certificate := TdwlOpenSSL.New_Cert_FromPEMStr(Certificate);
        ACMEClient.PrivateKey := TdwlOpenSSL.New_PrivateKey_FromPEMStr(PrivateKey);
      end
      else
      begin
        ACMECLient.RootCertificate := nil;
        ACMEClient.Certificate := nil;
        ACMECLient.PrivateKey := nil;
      end;
      ACMEClient.CheckCertificate;
      ACMECLient.LogCertificateStatus;
      if ACMECLient.CertificateStatus=certstatOk then
      begin
        if SslIoHandler.Environment.GetContext(ACMECLient.Domain)=nil then
          SslIoHandler.Environment.AddContext(ACMECLient.Domain, RootCertificate, Certificate, PrivateKey);
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
        RootCertificate := ACMECLient.RootCertificate.PEMString;
        Certificate := ACMECLient.Certificate.PEMString;
        PrivateKey := ACMECLient.PrivateKey.PEMString;
        var CmdUpdate := Session.CreateCommand(SQL_Update_Cert);
        CmdUpdate.Parameters.SetTextDataBinding(Update_Cert_Idx_RootCert, RootCertificate);
        CmdUpdate.Parameters.SetTextDataBinding(Update_Cert_Idx_Cert, Certificate);
        CmdUpdate.Parameters.SetTextDataBinding(Update_Cert_Idx_PrivateKey, PrivateKey);
        CmdUpdate.Parameters.SetIntegerDataBinding(Update_Cert_Idx_Id, Cmd.Reader.GetInteger(GetHostNames_Idx_Id));
        CmdUpdate.Execute;
      end;
      if ACMEClient.CertificateStatus in [certstatAboutToExpire, certstatOk] then
        SslIoHandler.Environment.AddContext(ACMECLient.Domain, RootCertificate, Certificate, PrivateKey);
    end;
  finally
    ACMEClient.Free;
  end;
  if TryUpgrade and (not HostnameSeen) then
  begin
    if TryHostNameMigration(ConfigParams) then
      CheckACMEConfiguration(Server, ConfigParams);
  end;
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

procedure TDWLServerSection.Start_LoadDLLHandlers(ConfigParams: IdwlParams);
const
  SQL_GetHandlers =
    'SELECT endpoint, handler_uri, params FROM dwl_handlers ORDER BY open_order';
  GetHandlers_Idx_endpoint=0; GetHandlers_Idx_handler_uri=1; GetHandlers_Idx_params=2;
begin
  try
    var DLLBasePath := ConfigParams.StrValue(Param_DLLBasePath, ExtractFileDir(ParamStr(0)));
    var Cmd := New_MySQLSession(ConfigParams).CreateCommand(SQL_GetHandlers);
    Cmd.Execute;
    while Cmd.Reader.Read do
    begin
      var EndPoint := '';
      try
        EndPoint := Cmd.Reader.GetString(GetHandlers_Idx_endpoint);
        var URI := Cmd.Reader.GetString(GetHandlers_Idx_handler_uri);
        var HandlerParams := New_Params;
        ConfigParams.AssignTo(HandlerParams);
        HandlerParams.WriteNameValueText(Cmd.Reader.GetString(GetHandlers_Idx_params ,true));
        if not HandlerParams.BoolValue(Param_Enabled, true) then
          Continue;
        HandlerParams.WriteValue(Param_Endpoint, Endpoint);
        var FileName := '';
        try
          if FServer.UnRegisterHandler(EndPoint) then
            TdwlLogger.Log('Unregistered DLL Handler at endpoint '+Endpoint, lsTrace);
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
            TdwlLogger.Log('Missing DLL '+URI+' ('+FileName+') for endpoint '+Endpoint, lsError);
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
            TdwlLogger.Log('Registered DLL Handler '+ExtractFileName(FileName)+' at endpoint '+Endpoint, lsTrace);
          end
          else
          begin
            FreeLibrary(DLLHandle);
            raise Exception.Create('No ProcessRequest or Authorize function found.');
          end;
        except
          on E: Exception do
            TdwlLogger.Log('Failed loading DLL '+URI+'('+FileName+') on endpoint '+Endpoint+': '+E.Message, lsError);
        end;
      except
        on E: Exception do
          TdwlLogger.Log('Error loading DLL handler at '+EndPoint+': '+E.Message, lsError);
      end;
    end;
  except
    on E: Exception do
      TdwlLogger.Log('Error loading DLL handlers: '+E.Message, lsError);
  end;
end;

procedure TDWLServerSection.Start_LoadURIAliases(ConfigParams: IdwlParams);
const
  SQL_Get_UriAliases =
    'SELECT alias, uri FROM dwl_urialiases';
  Get_UriAliases_Idx_alias=0; Get_UriAliases_Idx_uri=1;
begin
  try
    FServer.ClearURIAliases;
    var Cmd := New_MySQLSession(ConfigParams).CreateCommand(SQL_Get_UriAliases);
    Cmd.Execute;
    while Cmd.Reader.Read do
      FServer.AddURIAlias(Cmd.Reader.GetString(Get_UriAliases_Idx_alias), Cmd.Reader.GetString(Get_UriAliases_Idx_uri));
  except
    on E: Exception do
      TdwlLogger.Log('Error loading URI Aliases: '+E.Message, lsError);
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
      TdwlLogger.Log('DWL Server starting', lsTrace);
      var ConfigParams := New_Params;
      DWL.Server.AssignServerProcs;
      Start_PrepareParameters_Phase1(ConfigParams);
      var Session := Start_InitDataBase(ConfigParams);
      Start_PrepareParameters_Phase2(Session, ConfigParams);
      CheckACMEConfiguration(FServer, ConfigParams, true);
      Start_ProcessBindings(ConfigParams);
      Start_Enable_Logging(ConfigParams);
      Start_LoadURIAliases(ConfigParams);

      if not FServer.IsSecure then
      begin
        FServer.OnlyLocalConnections := ConfigParams.BoolValue(Param_TestMode);
        if FServer.OnlyLocalConnections then
          TdwlLogger.Log('SERVER IS NOT SECURE, only allowing local connections', lsWarning)
        else
          TdwlLogger.Log('SERVER IS NOT SECURE, please configure or review ACME parameters', lsWarning);
      end
      else
        FServer.OnlyLocalConnections := false;
      Start_DetermineServerParams(ConfigParams);
      if FServer.IsSecure or FServer.OnlyLocalConnections then
      begin
        TdwlMailQueue.Configure(ConfigParams, true);
        FServer.RegisterHandler(EndpointURI_Mail,  TdwlHTTPHandler_Mail.Create(ConfigParams));
        Start_LoadDLLHandlers(ConfigParams)
      end
      else
        TdwlLogger.Log('Skipped loading of handlers because server is not secure', lsWarning);
      // Time to start the server
      FServer.Active := true;
      TdwlLogger.Log('Enabled Server listening', lsNotice);
      FACMECheckThread := TACMECheckThread.Create(Self, ConfigParams);
      TdwlLogger.Log('DWL Server started', lsTrace);
      FServerStarted := true;
    except
      FServerStarted := false;
      FServer.Active := false;
    end;
    FServerStarting := false;
  end);
end;

procedure TDWLServerSection.Start_DetermineServerParams(ConfigParams: IdwlParams);
begin
  // The base uri is ALWAYS the standard port!!
  // Listenport is not taken into account
  if FServer.IsSecure then
    ConfigParams.WriteValue(Param_BaseURI, 'https://'+(FServer.IoHandler as IdwlSslIoHandler).Environment.MainContext.HostName)
  else
    ConfigParams.WriteValue(Param_BaseURI, 'https//localhost');
  if ConfigParams.StrValue(Param_Issuer)='' then
  begin
    var Issuer :=  ConfigParams.StrValue(Param_BaseURI)+Default_EndpointURI_OAuth2;
    ConfigParams.WriteValue(Param_Issuer, Issuer);
    TdwlLogger.Log('No issuer configured, default applied: '+Issuer, lsNotice);
  end;
end;

procedure TDWLServerSection.Start_Enable_Logging(ConfigParams: IdwlParams);
begin
  FServer.LogLevel := ConfigParams.IntValue(Param_LogLevel, httplogLevelWarning);
  TdwlLogger.Log('Enabled Request logging (level '+FServer.LogLevel.ToString+')', lsTrace);
  FLogHandler := TdwlHTTPHandler_Log.Create(ConfigParams); // init before activating DoLog!
  FCallBackLogDispatcher := EnableLogDispatchingToCallback(false, DoLog);
  FServer.RegisterHandler(EndpointURI_Log,  FLogHandler);
end;

function TDWLServerSection.Start_InitDataBase(ConfigParams: IdwlParams): IdwlMySQLSession;
const
  SQL_CheckTable_Handlers =
    'CREATE TABLE IF NOT EXISTS dwl_handlers (id INT AUTO_INCREMENT, open_order SMALLINT, endpoint VARCHAR(50), handler_uri VARCHAR(255), `params` TEXT NULL, INDEX `primaryindex` (`id`))';
  SQL_CheckTable_UriAliases =
    'CREATE TABLE IF NOT EXISTS dwl_urialiases (id INT AUTO_INCREMENT, alias VARCHAR(255), uri VARCHAR(255), PRIMARY KEY (id))';
  SQL_CheckTable_Parameters =
    'CREATE TABLE IF NOT EXISTS dwl_parameters (Id SMALLINT NOT NULL AUTO_INCREMENT, `Key` VARCHAR(50) NOT NULL, `Value` TEXT, PRIMARY KEY(Id), UNIQUE INDEX KeyIndex (`Key`))';
  SQL_CheckTable_HostNames =
    'CREATE TABLE IF NOT EXISTS dwl_hostnames (Id SMALLINT NOT NULL AUTO_INCREMENT, HostName VARCHAR(50) NOT NULL, CountryCode CHAR(2) NOT NULL, State VARCHAR(50) NOT NULL, '+
    'City VARCHAR(50) NOT NULL, BindingIp VARCHAR(39), RootCert TEXT, Cert TEXT, PrivateKey TEXT, PRIMARY KEY(Id), UNIQUE INDEX HostName (HostName))';
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
  Result.CreateCommand(SQL_CheckTable_UriAliases).Execute;
  Result.CreateCommand(SQL_CheckTable_Parameters).Execute;
  Result.CreateCommand(SQL_CheckTable_HostNames).Execute;
end;

procedure TDWLServerSection.Start_PrepareParameters_Phase1(ConfigParams: IdwlParams);
begin
  // at first take parameters from the commandline
  TdwlParamsUtils.Import_CommandLine(ConfigParams);
  // secondary pick parameters from inifile
  TdwlParamsUtils.Import_IniFile_Section(ConfigParams, ChangeFileExt(ParamStr(0), '.ini'),
    ConfigParams.StrValue(Param_Section_Dwl_Db, ParamDef_Section_Dwl_Db));
  // and later after Init of database parameters from the dwl database are added
end;

procedure TDWLServerSection.Start_PrepareParameters_Phase2(Session: IdwlMySQLSession; ConfigParams: IdwlParams);
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
  TdwlLogger.Log('Bound to '+IfThen(BindingIP='', '*', BindingIP)+':'+BindingPort.ToString);
end;

procedure TDWLServerSection.StopServer;
begin
  if not FServerStarted then
    Exit;
  TdwlLogger.Log('Stopping DWL Server', lsNotice);
  FLogHandler := nil; // do not try to log when server goes down
  FreeAndNil(FACMECheckThread);
  FServer.Active := false;
  TdwlLogger.UnregisterDispatcher(FCallBackLogDispatcher);
  TdwlMailQueue.Configure(nil); // to stop sending
  TdwlLogger.Log('Stopped DWL Server', lsNotice);
  TdwlLogger.FinalizeDispatching;
  FServerStarted := false;
end;

class function TDWLServerSection.TryHostNameMigration(ConfigParams: IdwlParams): boolean;
const
  SQL_CreateHostNameRecord = 'INSERT INTO dwl_hostnames (HostName, CountryCode, State, City) VALUES (?,?,?,?)';
  CreateHostNameRecord_Idx_HostName=0; CreateHostNameRecord_Idx_CountryCode=1; CreateHostNameRecord_Idx_State=2; CreateHostNameRecord_Idx_City=3;
begin
  // try migration from params based hostname to table dwl_hostnames
  var HostName := ConfigParams.StrValue(Param_ACMEDomain);
  Result := HostName<>'';
  if not Result then
    Exit;
  var CountryCode := ConfigParams.StrValue(Param_ACMECountry);
  var State := ConfigParams.StrValue(Param_ACMEState);
  var City := ConfigParams.StrValue(Param_ACMECity);
  if (CountryCode='') or (State='') or (City='') then
    Exit;
  var Cmd := New_MySQLSession(ConfigParams).CreateCommand(SQL_CreateHostNameRecord);
  Cmd.Parameters.SetTextDataBinding(CreateHostNameRecord_Idx_HostName, HostName);
  Cmd.Parameters.SetTextDataBinding(CreateHostNameRecord_Idx_CountryCode, CountryCode);
  Cmd.Parameters.SetTextDataBinding(CreateHostNameRecord_Idx_State, State);
  Cmd.Parameters.SetTextDataBinding(CreateHostNameRecord_Idx_City, City);
  Cmd.Execute;
  TdwlLogger.Log('Migrated hostname '+HostName+' into dwl_hostnames');
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
        TThread.Queue(nil,  procedure
          begin
            FSection.StopServer;
            FSection.StartServer;
          end);
          Continue;
    try
      TDWLServerSection.CheckACMEConfiguration(FSection.FServer, FConfigParams);
    except
      on E:Exception do
        TdwlLogger.Log(E);
    end;
  end;
end;

end.
