/// <summary>
///   A specific handler that can be added to a TdwlHTTPServer for exposing the
///   DWL logging API. Requests are handled triggered, filtered etc and at the
///   end saved in the database
/// </summary>
unit DWL.HTTP.Server.Handler.Log;

interface

uses
  DWL.HTTP.Server, DWL.HTTP.Server.Types, DWL.Params,
  System.Generics.Collections, System.SysUtils, System.SyncObjs, DWL.SyncObjs;

const
  logdestinationServerConsole='serverconsole';

const
  Param_EMail_To = 'email_to';
  Param_Email_From = 'email_from';
  Param_EMail_FromName = 'email_fromname';
  Param_EMail_Subject = 'email_subject';

type
  TLogTrigger = record
    Id: integer;
    MinLevel: byte;
    MaxLevel: byte;
    Channel: string;
    Topic: string;
    Parameters: string;
    SuppressDuplicateMSecs: cardinal;
    Hashes: TDictionary<integer, UInt64>;
    CleanupCounter: cardinal;
    function IsSuppressed(Level: Byte; const Source, Channel, Topic, Msg: string): boolean;
  end;

  TdwlHTTPHandler_Log = class(TdwlHTTPHandler)
  strict private
    FReloadTriggerTick: cardinal;
    FTriggers: TdwlThreadList<TLogTrigger>;
    FLogSecret: string;
    FMySQL_Profile: IdwlParams;
    FLogSubmitAccess: TCriticalSection;
    procedure InitializeDatabase;
    procedure CheckTriggers;
    function Post_Log(const State: PdwlHTTPHandlingState): boolean;
    function Options_Log(const State: PdwlHTTPHandlingState): boolean;
    procedure ProcessTriggers(const IpAddress: string; Level: Byte; const Source, Channel, Topic, Msg, ContentType: string; const Content: TBytes);
  protected
    function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; override;
  public
    constructor Create(AParams: IdwlParams);
    destructor Destroy; override;
    function Authorize(const State: PdwlHTTPHandlingState): boolean; override;
    function SubmitLog(const IpAddress: string; Level: Byte; const Source, Channel, Topic, Msg, ContentType: string; const Content: TBytes): boolean;
  end;

implementation

uses
  DWL.Params.Consts, DWL.MySQL, DWL.HTTP.Consts, DWL.Logging,
  DWL.HTTP.Server.Globals, DWL.HTTP.Server.Utils, System.Masks, System.Classes,
  IdMessage, System.StrUtils, IdAttachmentMemory, DWL.Mail.Queue,
  Winapi.WinInet, Winapi.Windows, System.Math, System.Hash;

const
  TRIGGER_RELOAD_MSECS = 60000; // 1 minute

{ TdwlHTTPHandler_Log }

function TdwlHTTPHandler_Log.Authorize(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := true;
end;

constructor TdwlHTTPHandler_Log.Create(AParams: IdwlParams);
begin
  inherited Create;
  FMySQL_Profile := New_Params;
  AParams.AssignTo(FMySQL_Profile, Params_SQLConnection);
  FLogSubmitAccess := TCriticalSection.Create;
  FLogSecret := AParams.StrValue(param_LogSecret);
  FTriggers := TdwlThreadList<TLogTrigger>.Create;
  InitializeDatabase;
  CheckTriggers;
end;

destructor TdwlHTTPHandler_Log.Destroy;
begin
  // dispose hashes from triggers
  var TrigList := FTriggers.LockList;
  try
    FTriggers.Free;
    for var Trig in TrigList do
      Trig.Hashes.Free;
  finally
    FTriggers.UnlockList;
  end;
  FLogSubmitAccess.Free;
  inherited Destroy;
end;

procedure TdwlHTTPHandler_Log.InitializeDatabase;
const
  SQL_CheckTable_LogDebug=
    'CREATE TABLE IF NOT EXISTS dwl_log_debug ('+
    '`Id` INT(11) NOT NULL AUTO_INCREMENT, '+
    '`IpAddress` VARCHAR(50), '+
    '`TimeStamp` DATETIME  DEFAULT CURRENT_TIMESTAMP, '+
    '`Level` TINYINT  DEFAULT 0, '+
    '`Source` VARCHAR(50), '+
    '`Channel` VARCHAR(50), '+
    '`Topic` VARCHAR(50), '+
    '`Msg` VARCHAR(250), '+
    '`ContentType` VARCHAR(50),	'+
    '`Content` LONGBLOB, '+
    'PRIMARY KEY (`ID`))';
  SQL_CheckTable_LogMessages=
    'CREATE TABLE IF NOT EXISTS dwl_log_messages ('+
    '`Id` INT(11) NOT NULL AUTO_INCREMENT, '+
    '`IpAddress` VARCHAR(50), '+
    '`TimeStamp` DATETIME  DEFAULT CURRENT_TIMESTAMP, '+
    '`Level` TINYINT  DEFAULT 0, '+
    '`Source` VARCHAR(50), '+
    '`Channel` VARCHAR(50), '+
    '`Topic` VARCHAR(50), '+
    '`Msg` VARCHAR(250), '+
    '`ContentType` VARCHAR(50),	'+
    '`Content` LONGBLOB, '+
    'PRIMARY KEY (`ID`))';
  SQL_CheckTable_LogTriggers=
    'CREATE TABLE IF NOT EXISTS `dwl_log_triggers` ('+
    '`Id` INT(11) NOT NULL AUTO_INCREMENT, '+
    '`Level_From` TINYINT, '+
    '`Level_To` TINYINT, '+
    '`Channel` VARCHAR(50), '+
    '`Topic` VARCHAR(50), '+
    '`Parameters` TEXT, '+
    '`SuppressDuplicateSeconds` INT, '+
    'PRIMARY KEY (`ID`))';
begin
  FMySQL_Profile.WriteValue(Param_CreateDatabase, true);
  FMySQL_Profile.WriteValue(Param_TestConnection, true);
  var Session := New_MySQLSession(FMySQL_Profile);
  FMySQL_Profile.ClearKey(Param_CreateDatabase);
  FMySQL_Profile.ClearKey(Param_TestConnection);
  Session.CreateCommand(SQL_CheckTable_LogDebug).Execute;
  Session.CreateCommand(SQL_CheckTable_LogMessages).Execute;
  Session.CreateCommand(SQL_CheckTable_LogTriggers).Execute;
end;

procedure TdwlHTTPHandler_Log.CheckTriggers;
const
  SQL_Get_Triggers=
    'SELECT Id, Level_From, Level_to, Channel, Topic, Parameters, SuppressDuplicateSeconds FROM dwl_log_triggers';
begin
  try
    var T := GetTickCount64;
    if T>FReloadTriggerTick then
    begin
      FReloadTriggerTick := T+TRIGGER_RELOAD_MSECS;
      var TrigList := FTriggers.LockList;
      try
        var CurrentHashes := TDictionary<integer, TDictionary<integer, UInt64>>.Create;
        try
          // Keep hashes from current triggers
          for var Trig in TrigList do
            if Trig.Hashes<>nil then
              CurrentHashes.Add(Trig.Id, Trig.Hashes);
          TrigList.Clear;
          var Cmd := New_MySQLSession(FMySQL_Profile).CreateCommand(SQL_Get_Triggers);
          Cmd.Execute;
          while Cmd.Reader.Read do
          begin
            var Trig: TLogTrigger;
            Trig.Id := Cmd.Reader.GetInteger(0);
            // never have triggers for levels below lsNotice
            // said in another way: triggering only acts on the items put in the dwl_log_messages table
            // (ignore the items from dwl_log_debug)
            Trig.MinLevel := Max(integer(lsNotice), Cmd.Reader.GetInteger(1, true));
            Trig.MaxLevel := Cmd.Reader.GetInteger(2, true, integer(lsFatal));
            Trig.Channel := Cmd.Reader.GetString(3, true, '*');
            Trig.Topic := Cmd.Reader.GetString(4, true, '*');
            Trig.Parameters := Cmd.Reader.GetString(5, true);
            Trig.SuppressDuplicateMSecs := Max(0, Cmd.Reader.GetInteger(6, true))*1000;
            if Trig.SuppressDuplicateMSecs>0 then
            begin
              Trig.CleanupCounter := 0;
              if CurrentHashes.TryGetValue(Trig.Id, Trig.Hashes) then
                CurrentHashes.Remove(Trig.Id)
              else
                Trig.Hashes := TDictionary<integer, UInt64>.Create;
            end;
            TrigList.Add(Trig);
          end;
          // dispose no longer used hases
          for var Hashes in CurrentHashes.Values do
            Hashes.Free;
        finally
          CurrentHashes.Free;
        end;
      finally
        FTriggers.UnlockList;
      end;
    end;
  except
    on E:Exception do
      SubmitLog('', integer(lsError), '', '', '', 'Error loading Triggers: '+E.Message, '', nil);
  end;
end;

function TdwlHTTPHandler_Log.Options_Log(const State: PdwlHTTPHandlingState): boolean;
begin
  serverProcs.SetHeaderValueProc(State, 'Access-Control-Allow-Origin', '*');
  serverProcs.SetHeaderValueProc(State, 'Access-Control-Allow-Methods', 'OPTIONS, POST');
  var Hdrs: string;
  if State.TryGetHeaderValue('Access-Control-Request-Headers', Hdrs) then
    serverProcs.SetHeaderValueProc(State, 'Access-Control-Allow-Headers', PWideChar(Hdrs));
  Result := true;
end;

function TdwlHTTPHandler_Log.Post_Log(const State: PdwlHTTPHandlingState): boolean;
var
  LogSecret: string;
  Msg: String;
  IpAddress: string;
  Source: string;
  Level: integer;
  Channel: string;
  Topic: string;
  ContentType: string;
begin
  Result := false;
  if not State.TryGetRequestParamStr('secret', LogSecret) then
    Exit;
  if not SameText(LogSecret, FLogSecret) then
    Exit;
  if not State.TryGetRequestParamStr('msg', Msg) then
    Exit;
  if not State.TryGetRequestParamStr('remoteip', IpAddress) then
    IpAddress := '';
  var LevelStr: string;
  if not (State.TryGetRequestParamStr('level', LevelStr) and integer.TryParse(LevelStr, Level)) then
    Level := 0;
  if not State.TryGetRequestParamStr('source', Source) then
    Source := '';
  if not State.TryGetRequestParamStr('channel', Channel) then
    Channel := '';
  if not State.TryGetRequestParamStr('topic', Topic) then
    Channel := '';
  var Data: pointer;
  var DataSize: Int64;
  var Content: TBytes := nil;
  if serverProcs.GetPayloadPtrProc(State, Data, DataSize) and (DataSize>0) then
  begin
    SetLength(Content, DataSize);
    Move(Data^, Content[0], DataSize);
  end;
  if Content<>nil then
  begin
    if not State.TryGetHeaderValue('Content-Type', ContentType) then
      ContentType := 'application/octet-stream';
  end
  else
    ContentType := '';
  if not SubmitLog(IpAddress, Level, Source, Channel, Topic, Msg, ContentType, Content) then
    State.StatusCode := HTTP_STATUS_SERVER_ERROR;
  serverProcs.SetHeaderValueProc(State, 'Access-Control-Allow-Origin', '*');
  State.Flags := State.Flags or HTTP_FLAG_NOLOGGING;
  Result := true;
end;

function TdwlHTTPHandler_Log.ProcessRequest(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := false;
  if SameText(State.URI, '') then
  begin
    if State.Command=dwlhttpPOST then
      Result := Post_Log(State)
    else
    if State.Command=dwlhttpOPTIONS then
      Result := Options_Log(State);
  end;
end;

procedure TdwlHTTPHandler_Log.ProcessTriggers(const IpAddress: string; Level: Byte; const Source, Channel, Topic, Msg, ContentType: string; const Content: TBytes);
begin
  CheckTriggers;
  try
    var TrigList := FTriggers.LockListRead;
    try
      for var Trig in TrigList do
      begin
        if MatchesMask(Channel, Trig.Channel) and MatchesMask(Topic, Trig.Topic) and
          (Level>=Trig.MinLevel) and (Level<=Trig.MaxLevel) and
          (not Trig.IsSuppressed(Level, Source, Channel, Topic, Msg)) then
        begin
          var Parms := TStringList.Create;
          try
            Parms.Text := Trig.Parameters;
            var MailMsg := TIdMessage.Create(nil);
            try
              MailMsg.Recipients.EMailAddresses := Parms.Values[Param_EMail_To];
              MailMsg.From.Address := Parms.Values[Param_Email_From];
              MailMsg.From.Name := Parms.Values[Param_EMail_FromName];
              var Subject := parms.Values[Param_EMail_Subject];
              Subject := ReplaceStr(Subject, '$(level)', TdwlLogger.GetSeverityLevelAsString(TdwlLogSeverityLevel(Level)));
              Subject := ReplaceStr(Subject, '$(source)', Source);
              Subject := ReplaceStr(Subject, '$(channel)', Channel);
              Subject := ReplaceStr(Subject, '$(topic)', Topic);
              Subject := ReplaceStr(Subject, '$(msg)', Msg.Substring(0, 100).Replace(#13, '').Replace(#10, ''));
              MailMsg.Subject := Subject;
              if SameText(Copy(trim(ContentType), 1, 5), 'text/') then
              begin
                MailMsg.Body.Text := TEncoding.UTF8.GetString(Content);
                MailMsg.ContentType := ContentType;
              end
              else
              begin
                if Length(Content)>0 then
                begin
                  var Attachment := TIdAttachmentMemory.Create(MailMsg.MessageParts);
                  Attachment.ContentType := ContentType;
                  Attachment.DataStream.WriteBuffer(Content[0], Length(Content));
                end;
              end;
              TdwlMailQueue.QueueForSending(MailMsg);
            finally
              MailMsg.Free;
            end;
          finally
            Parms.Free;
          end;
        end;
      end;
    finally
      FTriggers.UnlockListRead;
    end;
  except
    on E: Exception do
      SubmitLog('', integer(lsError), '', '', '', 'Error executing trigger: '+E.Message, '', nil);
  end;
end;

function TdwlHTTPHandler_Log.SubmitLog(const IpAddress: string; Level: Byte; const Source, Channel, Topic, Msg, ContentType: string; const Content: TBytes): boolean;
const
  SQL_Insert_Debug=
    'INSERT INTO dwl_log_debug (IpAddress, Level, Source, Channel, Topic, Msg, ContentType, Content) values (?, ?, ?, ?, ?, ?, ?, ?)';
  SQL_Insert_Log=
    'INSERT INTO dwl_log_messages (IpAddress, Level, Source, Channel, Topic, Msg, ContentType, Content) values (?, ?, ?, ?, ?, ?, ?, ?)';
begin
  Result := false;
  try
    FLogSubmitAccess.Enter;
    try
      // only save notice and more severe to database
      var Cmd: IdwlMySQLCommand;
      if TdwlLogSeverityLevel(Level)<lsNotice then
        Cmd := New_MySQLSession(FMySQL_Profile).CreateCommand(SQL_Insert_Debug)
      else
        Cmd := New_MySQLSession(FMySQL_Profile).CreateCommand(SQL_Insert_Log);
      Cmd.Parameters.SetTextDataBinding(0, IpAddress);
      Cmd.Parameters.SetIntegerDataBinding(1, Level);
      Cmd.Parameters.SetTextDataBinding(2, Source);
      Cmd.Parameters.SetTextDataBinding(3, Channel);
      Cmd.Parameters.SetTextDataBinding(4, Topic);
      Cmd.Parameters.SetTextDataBinding(5, Msg.Substring(0, 250));
      Cmd.Parameters.SetTextDataBinding(6, ContentType);
      if Content=nil then
        Cmd.Parameters.SetNullDataBinding(7)
      else
        Cmd.Parameters.SetBinaryRefDataBinding(7, @Content[0], Length(Content));
      Cmd.Execute;
      {$IFDEF DEBUG}
      // Forward the logmessage to the server for debugging purposes
      if Source<>TdwlLogger.Default_Source then
      begin
        var LogItem := TdwlLogger.PrepareLogitem;
        LogItem.Msg := Msg;
        LogItem.SeverityLevel := TdwlLogSeverityLevel(Level);
        LogItem.Source := Source;
        LogItem.Channel := Channel;
        LogItem.Topic := Topic;
        LogItem.ContentType := ContentType;
        LogItem.Content := Content;
        LogItem.Destination := logdestinationServerConsole;
        TdwlLogger.Log(LogItem);
      end;
      {$ENDIF}
      // Process the triggers
      ProcessTriggers(IpAddress, Level, Source, Channel, Topic, Msg, ContentType, Content);
    finally
      FLogSubmitAccess.Leave;
    end;
    Result := true;
  except
    // never let an exception escape.... Just Return Result=false
  end;
end;

{ TLogTrigger }

function TLogTrigger.IsSuppressed(Level: Byte; const Source, Channel, Topic, Msg: string): boolean;
begin
  if Hashes=nil then
    Exit(false);
  var Tick := GetTickCount64;
  // remove Old Hashes every thousand triggering moments
  if CleanUpCounter=0 then
  begin
    var Enum := Hashes.GetEnumerator;
    while ENum.MoveNext do
    begin
      if ENum.Current.Value<Tick then
        Hashes.Remove(ENum.Current.Key);
    end;
    CleanUpCounter := 1000;
  end;
  dec(CleanUpCounter);
  var Hash := THashBobJenkins.Create;
  Hash.Update(Msg);
  Hash.Update(Level, SizeOf(Level));
  Hash.Update(Source);
  Hash.Update(Channel);
  Hash.Update(Topic);
  var LogTick: UInt64;
  Result := not Hashes.TryGetValue(Hash.HashAsInteger, LogTick);
  if not Result then
  begin
    if LogTick<Tick then
    begin
      Result := true;
      Hashes.Remove(Hash.HashAsInteger);
    end;
  end;
  if Result then
    Hashes.Add(Hash.HashAsInteger, Tick+SuppressDuplicateMSecs);
end;

end.

