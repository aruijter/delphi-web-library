unit DWL.Mail.Queue;

interface

uses
  DWL.Params, IdMessage, System.Generics.Collections, DWL.Logging,
  System.Classes, System.Rtti, DWL.SyncObjs;

const
  Param_mailQueue_Domains = 'mailqueue_domains';

type
  TdwlMailQueue = record
  strict private
  class var
    FParams: IdwlParams;
    FMailSendThread: TdwlThread;
    class procedure ParamChanged(Sender: IdwlParams; const Key: string; const Value: TValue); static;
    class procedure CreateMailSendThread; static;
    class procedure CheckMailSendThread; static;
    class procedure KillMailSendThread; static;
  private
  class var
    FDomainContexts: TDictionary<string, IdwlParams>;
    FDefaultDomainContextParams: IdwlParams;
    class procedure Log(const Msg: string; SeverityLevel: TdwlLogSeverityLevel=lsNotice); static;
  public
    class constructor Create;
    class destructor Destroy;
    /// <summary>
    ///   <para>
    ///     Call Configure to activate the MailQueue. The Params object will
    ///     'taken' and when sending is active modified internally within the processor (f.e. when
    ///     refreshtokens are changed), This opens the possibility to attach
    ///     an event to the Params to save the changes persistently.
    ///   </para>
    ///   <para>
    ///     Needed keys: <br />- MySQL configuration related keys like host,
    ///     username, password and db <br />- mailqueue_domains (only when sending enabled): a JSON
    ///     string with an array of objects. each object represents a
    ///     delivery domain and needs to contains keys for domain, host,
    ///     port, username, password or endpoint/clientid/refreshtoken in the
    ///     case of oauth2 configuration <br />
    ///   </para>
    /// </summary>
    class procedure Configure(Params: IdwlParams; EnableMailSending: boolean=false); static;
    /// <summary>
    ///   Queues an Indy TIdMessage for sending. Please note ownership of
    ///   the IdMessage is not taken! You have to free it yourself (because you
    ///   also created it ;-)
    ///   if not configured yet the result of the function is false, otherwise true
    /// </summary>
    class function QueueForSending(Msg: TIdMessage): boolean; static;
  end;


implementation

uses
  DWL.MySQL, DWL.Params.Consts, System.JSON, Winapi.Windows, System.SysUtils,
  IdSMTP, IdSSLOpenSSL, DWL.HTTP.APIClient.OAuth2, DWL.HTTP.APIClient, DWL.Mail.SASL,
  IdAssignedNumbers, System.Math, IdExplicitTLSClientServerBase, DWL.Classes,
  DWL.StrUtils;

const
  MAILQUEUE_SLEEP_MSECS=120000{2 min};

type
  TdwlMailStatus = (msQueued=0, msRetrying=2, msSent=5, msError=9);

  TMailSendThread = class(TdwlThread)
  strict private
    FParams: IdwlParams;
    FSMTP: TIdSMTP;
    FIdSASL: TIdSASLOAuth2;
    FCurrentContextParams: IdwlParams;
    FCurrentRefreshToken: string;
    procedure FreeSMTP;
    procedure Process;
    function ProcessMsg(Msg: TIdMessage): TdwlResult;
    procedure Refreshtoken_Callback(var Token: string; Action: TdwlAPIAuthorizerCallBackAction);
  private
    FLastActionTick: UInt64;
    procedure CheckNow;
  protected
    procedure Execute; override;
  public
    constructor Create(Params: IdwlParams);
  end;

{ TdwlMailQueue }

class procedure TdwlMailQueue.CheckMailSendThread;
begin
  // Check if Queue is not stalled....
  // Sometimes Indy 'hangs'
  // this is a very harsh method as a last resort
  // and can lead to memory leaks, unreleased resources, etc
  try
    if (FMailSendThread<>nil) and ((TMailSendThread(FMailSendThread).FLastActionTick+MAILQUEUE_SLEEP_MSECS*2)<GetTickCount64) then
    begin
      try
        Log('Terminating Stalled MailSendThread (and creating new one)', lsError);
        TerminateThread(FMailSendThread.Handle, 0);
        FreeAndNil(FMailSendThread);
      except
        FMailSendThread := nil;
      end;
      CreateMailSendThread;
    end;
  except
  end;
end;

class procedure TdwlMailQueue.Configure(Params: IdwlParams; EnableMailSending: boolean=false);
const
  SQL_CheckTable = 'CREATE TABLE IF NOT EXISTS dwl_mailqueue (' +
    'Id INT UNSIGNED NOT NULL AUTO_INCREMENT, ' +
    'MomentInQueue DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP, ' +
    'Status TINYINT UNSIGNED NOT NULL DEFAULT ''0'', ' +
    'Attempts TINYINT UNSIGNED NULL DEFAULT ''0'', ' +
    'DelayedUntil DATETIME NULL DEFAULT NULL, ' +
    'MomentSent DATETIME NULL DEFAULT NULL, ' +
    'ProcessingLog TEXT NULL, ' +
    'BccRecipients TEXT NULL, ' +
    'Eml LONGTEXT NOT NULL, ' +
    'PRIMARY KEY (Id), ' +
    'INDEX `StatusDelayedUntilIndex` (`Status`, `DelayedUntil`))';
begin
  KillMailSendThread;
  FDomainContexts.Clear;
  FParams := New_Params;
  if Params<>nil then
    Params.AssignTo(FParams);
  if EnableMailSending then
  begin
    FParams.WriteValue(Param_CreateDatabase, true);
    FParams.WriteValue(Param_TestConnection, true);
    var Session := New_MySQLSession(FParams);
    FParams.ClearKey(Param_CreateDatabase);
    FParams.ClearKey(Param_TestConnection);
    SeSsion.CreateCommand(SQL_CheckTable).Execute;
    var DomainContextStr := FParams.StrValue(Param_mailQueue_Domains);
    if DomainContextStr<>'' then
    begin
      var JSON := TJSONObject.ParseJSONValue(DomainContextStr);
      try
        if JSON is TJSONArray then
        begin
          var Enum := TJSONArray(JSON).GetEnumerator;
          try
            while ENum.MoveNext do
            begin
              if not (ENum.Current is TJSONObject) then
              begin
                Log('Configured mailqueue_domains array contains a non-object entry', lsError);
                Break;
              end;
              var DomainParams := New_Params;
              DomainParams.WriteJSON(TJSONObject(ENum.Current));
              var Domain: string;
              if not DomainParams.TryGetStrValue('domain', Domain) then
                Log('Missing domain in on of the configured contexts', lsError)
              else
              begin
                DomainParams.EnableChangeTracking(ParamChanged);
                if Domain='*' then
                  FDefaultDomainContextParams := DomainParams
                else
                  FDomainContexts.Add(Domain, DomainParams);
              end;
            end;
          finally
            Enum.Free;
          end;
        end
        else
          Log('Configured mailqueue_domains is not an JSON array', lsError);
      finally
        JSON.Free;
      end;
    end;
    CreateMailSendThread;
  end;
end;

class constructor TdwlMailQueue.Create;
begin
  inherited;
  FDomainContexts := TDictionary<string, IdwlParams>.Create;
end;

class procedure TdwlMailQueue.CreateMailSendThread;
begin
  if FDomainContexts.Count=0 then
    Log('No domains configured, mail will not be processed', lsWarning)
  else
  begin
    var ThreadParams := New_Params;
    FParams.AssignKeysTo(ThreadParams, Params_SQLConnection);
    FMailSendThread := TMailSendThread.Create(ThreadParams);
    FMailSendThread.FreeOnTerminate := true;
  end;
end;

class destructor TdwlMailQueue.Destroy;
begin
  KillMailSendThread;
  FDomainContexts.Free;
  inherited;
end;

class procedure TdwlMailQueue.KillMailSendThread;
begin
  if FMailSendThread=nil then
    Exit;
  FMailSendThread.Terminate;
  // FmailSendThread is Free on Terminate, so no Free needed
  // but set to nil to indicate it's terminated
  FMailSendThread := nil;
end;

class procedure TdwlMailQueue.Log(const Msg: string; SeverityLevel: TdwlLogSeverityLevel=lsNotice);
begin
  TdwlLogger.Log(Msg, SeverityLevel, '', 'mailqueue');
end;

class procedure TdwlMailQueue.ParamChanged(Sender: IdwlParams; const Key: string; const Value: TValue);
const
  SQL_InsertOrUpdateParameter=
    'INSERT INTO dwl_parameters (`Key`, `Value`) VALUES (?, ?) ON DUPLICATE KEY UPDATE `Value`=VALUES(`Value`)';
begin
  // effectivly the only thing that is written to Params is a new refreshtoken
  // we need to save it back into the database
  if Key=Param_Refreshtoken then // just to be sure (and for documentation purposes ;-)
  begin
    var JSONArray := TJSONArray.Create;
    try
      if FDefaultDomainContextParams<>NIL then
      begin
        var JSONObject := TJSONObject.Create;
        JSONArray.Add(JSOnObject);
        FDefaultDomainContextParams.PutIntoJSONObject(JSONObject);
      end;
      var ENum := FDomainContexts.GetEnumerator;
      try
        while ENum.MoveNext do
        begin
          var JSONObject := TJSONObject.Create;
          JSONArray.Add(JSOnObject);
          ENum.Current.Value.PutIntoJSONObject(JSONObject);
        end;
      finally
        ENum.Free;
      end;
      var Cmd := New_MySQLSession(FParams).CreateCommand(SQL_InsertOrUpdateParameter);
      Cmd.Parameters.SetTextDataBinding(0, Param_mailQueue_Domains);
      Cmd.Parameters.SetTextDataBinding(1, JSONArray.ToJSON);
      Cmd.Execute;
    finally
      JSONArray.Free;
    end;
  end;
end;

class function TdwlMailQueue.QueueForSending(Msg: TIdMessage): boolean;
const
  SQL_InsertInQueue = 'INSERT INTO dwl_mailqueue (bccrecipients, eml) VALUES (?, ?)';
begin
  // First check if configured
  Result := FParams<>nil;
  if not Result then
    Exit;
  CheckMailSendThread;
  var Str := TStringStream.Create;
  try
    Msg.SaveToStream(Str);
    Str.Seek(0, soBeginning);
    var Cmd := New_MySQLSession(FParams).CreateCommand(SQL_InsertInQueue);
    Cmd.Parameters.SetTextDataBinding(0, Msg.BccList.EMailAddresses);
    Cmd.Parameters.SetTextDataBinding(1, Str.ReadString(MaxInt));
    Cmd.Execute;
    if FMailSendThread<>nil then
      TMailSendThread(FMailSendThread).CheckNow;
  finally
    Str.Free;
  end;
end;

{ TMailSendThread }

constructor TMailSendThread.Create(Params: IdwlParams);
begin
  FParams := Params;
  FLastActionTick := GetTickCount64;
  inherited Create;
end;

procedure TMailSendThread.Execute;
begin
  Sleep(1000); // Let logging handler in server initialize
  while not Terminated do
  begin
    Process;
    FLastActionTick := GetTickCount64;
    WaitForSingleObject(FWorkToDoEventHandle, MAILQUEUE_SLEEP_MSECS);
  end;
end;

procedure TMailSendThread.FreeSMTP;
begin
  if FSMTP=nil then
    Exit;
  FreeAndNil(FSMTP);
  FreeAndNil(FIdSASL);
  FCurrentContextParams := nil;
end;

procedure TMailSendThread.CheckNow;
begin
  SetEvent(FWorkToDoEventHandle);
end;

procedure TMailSendThread.Process;
const
  SQL_GetQueuedMail =
    'SELECT Id, eml, bccrecipients, Attempts, processinglog FROM dwl_mailqueue '+
    'WHERE (Status<?) and ((DelayedUntil is NULL) or (DelayedUntil<=CURRENT_TIMESTAMP())) '+
    'and ((Attempts IS NULL) or (Attempts<?)) ORDER BY Status, DelayedUntil';
  SQL_UPDATE_part1 = 'UPDATE dwl_mailqueue SET processinglog=?, attempts=?, status=';
  SQL_UPDATE_part2_Complete = ', DelayedUntil=NULL, momentsent=CURRENT_TIMESTAMP()';
  SQL_UPDATE_part2_Retry = ', DelayedUntil=DATE_ADD(CURRENT_TIMESTAMP(), INTERVAL 5 MINUTE)';
  SQL_UPDATE_part2_Error = ', DelayedUntil=NULL';
  SQL_UPDATE_part3 = ' WHERE id=?';
  MAX_ATTEMPTS = 5;
begin
  try
    try
      var Session := New_MySQLSession(FParams);
      var Cmd_Queue := Session.CreateCommand(SQL_GetQueuedMail);
      Cmd_Queue.Parameters.SetIntegerDataBinding(0, ord(msSent));
      Cmd_Queue.Parameters.SetIntegerDataBinding(1, MAX_ATTEMPTS);
      Cmd_Queue.Execute;
      var Reader := Cmd_Queue.Reader;
      while Reader.Read do
      begin
        var Current_ID := Reader.GetInteger(0);
        var Attempts := Reader.GetInteger(3, true);
        var ProcessingLog := Reader.GetString(4, true);
        inc(Attempts);
        var Str := TStringStream.Create(Reader.GetString(1));
        try
          var Msg := TIdMessage.Create(nil);
          try
            Msg.LoadFromStream(Str);
            Msg.BccList.EMailAddresses := Cmd_Queue.Reader.GetString(2, true);
            var Update_SQL := SQL_UPDATE_part1;
            var Res := ProcessMsg(Msg);
            if Res.Success then
              Update_SQL := Update_SQL+byte(msSent).ToString+SQL_UPDATE_part2_Complete
            else
            begin
              if Attempts=MAX_ATTEMPTS then
                Update_SQL := Update_SQL+byte(msError).ToString+SQL_UPDATE_part2_Error
              else
                Update_SQL := Update_SQL+byte(msRetrying).ToString+SQL_UPDATE_part2_Retry;
              ProcessingLog := ProcessingLog+#13#10+Res.ErrorMsg;
            end;
            Update_SQL := Update_SQL+SQL_UPDATE_part3;
            var Cmd := Session.CreateCommand(Update_SQL);
            Cmd.Parameters.SetTextDataBinding(0, ProcessingLog);
            Cmd.Parameters.SetIntegerDataBinding(1, Attempts);
            Cmd.Parameters.SetIntegerDataBinding(2, Current_ID);
            Cmd.Execute;
            if Res.Success then
              TdwlMailQueue.Log('Successfully sent mail to '+Msg.Recipients.EMailAddresses, lsTrace);
            // never Log a failed mail attempt, you can read it back in the ProcessingLog
            // a log will most probably generate a new mail and an infinite loop is created!!!!
          finally
            Msg.Free;
          end;
        finally
          Str.Free;
        end;
      end;
    finally
      FreeSMTP;
      FCurrentContextParams := nil;
    end;
  except
    on E:Exception do
      TdwlLogger.Log(E);
  end;
end;

function TMailSendThread.ProcessMsg(Msg: TIdMessage): TdwlResult;
const
  MAX_DIRECT_ATTEMPTS=2;
begin
  var MailIsSent := false;
  try
    var DomainFrom := Msg.From.Address;
    var P := pos('@', DomainFrom);
    DomainFrom := LowerCase(trim(Copy(DomainFrom, p+1, MaxInt)));
    // introduced an attemptcount to handle email servers who just disconnect the
    // first time as a spam prevention measure
    var AttemptCount := 0;
    while (not MailIsSent) and (AttemptCount<MAX_DIRECT_ATTEMPTS) do
    begin
      inc(AttemptCount);
      var DomainContextParams: IdwlParams;
      if not TdwlMailQueue.FDomainContexts.TryGetValue(DomainFrom, DomainContextParams) then
        DomainContextParams := TdwlMailQueue.FDefaultDomainContextParams;
      if (FSMTP=nil) or (not FSMTP.Connected) or (FCurrentContextParams<>DomainContextParams) then
      begin
        FreeSMTP;
        FSMTP := TIdSMTP.Create(nil);
        FCurrentContextParams := DomainContextParams;
        if FCurrentContextParams=nil then
        begin
          Result.AddErrorMsg('No Context found for from: '+Msg.From.Address);
          Exit;
        end;
        // AdR 20190820: See if setting timeouts prevent the queue from
        // hanging sometimes...
        FSMTP.ConnectTimeout := 30000; {30 secs}
        FSMTP.ReadTimeout := 30000; {30 secs}
        FSMTP.Host := FCurrentContextParams.StrValue(Param_Host);
        FSMTP.Port := FCurrentContextParams.IntValue(Param_Port, 25);
        FSMTP.Username := FCurrentContextParams.StrValue(Param_Username);
        FCurrentRefreshToken := FCurrentContextParams.StrValue(Param_Refreshtoken);
        if FCurrentRefreshToken<>'' then
        begin
          FSMTP.AuthType := satSASL;
          var Authorizer := New_OAuth2Authorizer(Refreshtoken_Callback, FCurrentContextParams.StrValue('endpoint'), FCurrentContextParams.StrValue('clientid'), FCurrentContextParams.StrValue('redirect_uri'), []{scopes not needed, are already embedded in refreshtoken}, '');
          var AccessToken := Authorizer.GetAccesstoken;
          if AccessToken='' then
          begin
            Result.AddErrorMsg('Error fetching Access token for Context: '+DomainFrom);
            Exit;
          end;
          FIdSASL := TIdSASLOAuth2.Create(nil);
          FIdSASL.Token := AccessToken;
          FIdSASL.User := FSMTP.Username;
          FSMTP.SASLMechanisms.Add.SASL := FIdSASL;
        end
        else
          FSMTP.Password := FCurrentContextParams.StrValue('password');
        if (FSMTP.Port= IdPORT_ssmtp) or (FSMTP.Port=Id_PORT_submission) then
        begin
          var sslHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FSMTP);
          sslHandler.SSLOptions.Method := sslvTLSv1_2;
          FSMTP.IOHandler := sslHandler;
          if FSMTP.Port=IdPORT_ssmtp then
            FSMTP.UseTLS := utUseImplicitTLS
          else
            FSMTP.UseTLS := utUseExplicitTLS;
        end;
        FSMTP.Connect;
      end;
      try
        if not FSMTP.Connected then
        begin
          Result.AddErrorMsg('Failed to connect to mailserver');
          Exit;
        end;
        FSMTP.Send(Msg);
        MailIsSent := true;
      except
        on E: Exception do
        begin
          if AttemptCount<MAX_DIRECT_ATTEMPTS then
            // in office 365 an exception occurs because the server just disconnects (as spam prevention)
            // let's try another attempt immediately
            FSMTP.Disconnect
          else
            raise;
        end;
      end;
    end;
    if not MailIsSent and Result.Success then
      Result.AddErrorMsg('For some unknown reason mail was not sent');
  except
    on E:Exception do
      Result.AddErrorMsg(E.Message);
  end;
end;

procedure TMailSendThread.Refreshtoken_Callback(var Token: string; Action: TdwlAPIAuthorizerCallBackAction);
begin
  if Action=acaGetRefreshtoken then
    Token := FCurrentRefreshToken;
  if Action=acaNewRefreshtoken then
  begin
    FCurrentContextParams.WriteValue(Param_Refreshtoken, Token);
    // we need to add here that the mailqueue_domains is written back to params
  end;
end;

end.
