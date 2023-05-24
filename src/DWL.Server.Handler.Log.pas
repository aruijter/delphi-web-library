/// <summary>
///   A specific handler that can be added to a TdwlHTTPServer for exposing the
///   DWL logging API. Requests are handled triggered, filtered etc and at the
///   end saved in the database
/// </summary>
unit DWL.Server.Handler.Log;

interface

uses
  DWL.Server, DWL.Server.Types, DWL.Params,
  System.Generics.Collections, System.SysUtils, System.SyncObjs, DWL.SyncObjs,
  System.RegularExpressions;

const
  Param_EMail_To = 'email_to';
  Param_Email_From = 'email_from';
  Param_EMail_FromName = 'email_fromname';
  Param_EMail_Subject = 'email_subject';

type
  TLogTrigger = class
  strict private
    FId: integer;
    FMinLevel: byte;
    FMaxLevel: byte;
    FChannel: TRegEx;
    FTopic: TRegEx;
    FParameters: string;
    FSuppressDuplicateMSecs: cardinal;
    FSuppressEvaluateContent: boolean;
    // suppresshash is a list of hashes and the tick when the hashed value should not longer be suppressed
    FSuppressHashes: TDictionary<integer, UInt64>;
    // cleanup is done every TRIGGER_CLEANUP_COUNT trigger events
    FCleanupCounter: cardinal;
    procedure SetSuppressDuplicateMSecs(const Value: cardinal);
  private
    property Id: integer read FId;
    property MinLevel: byte read FMinLevel write FMinLevel;
    property MaxLevel: byte read FMaxLevel write FMaxLevel;
    property Channel: TRegEx read FChannel write FChannel;
    property Topic: TRegEx read FTopic write FTopic;
    property Parameters: string read FParameters write FParameters;
    property SuppressDuplicateMSecs: cardinal read FSuppressDuplicateMSecs write SetSuppressDuplicateMSecs;
    property SuppressEvaluateContent: boolean read FSuppressEvaluateContent write FSuppressEvaluateContent;
    function IsSuppressed(Level: Byte; const Source, Channel, Topic, Msg: string; Content: TBytes): boolean;
  public
    constructor Create(AId: integer);
    destructor Destroy; override;
  end;

  TdwlHTTPHandler_Log = class(TdwlHTTPHandler)
  strict private
    FReloadTriggerTick: cardinal;
    FTriggers: TdwlThreadList<TLogTrigger>;
    FLogSecret: string;
    FMySQL_Profile: IdwlParams;
    FLogSubmitAccess: TCriticalSection;
    procedure InitializeDatabase;
    procedure CheckTriggers(TrigList: TList<TLogTrigger>);
    function Post_Log(const State: PdwlHTTPHandlingState): boolean;
    function Options_Log(const State: PdwlHTTPHandlingState): boolean;
    procedure ProcessTriggers(const IpAddress: string; Level: Byte; const Source, Channel, Topic, Msg, ContentType: string; const Content: TBytes);
  public
    constructor Create(AParams: IdwlParams);
    destructor Destroy; override;
    function Authorize(const State: PdwlHTTPHandlingState): boolean; override;
    function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; override;
    function SubmitLog(const IpAddress: string; Level: Byte; const Source, Channel, Topic, Msg, ContentType: string; const Content: TBytes): boolean;
  end;

implementation

uses
  DWL.Params.Consts, DWL.MySQL, DWL.HTTP.Consts, DWL.Logging,
  DWL.Server.Globals, DWL.Server.Utils, System.Masks, System.Classes,
  IdMessage, System.StrUtils, IdAttachmentMemory, DWL.Mail.Queue,
  Winapi.WinInet, Winapi.Windows, System.Math, System.Hash, DWL.Server.Consts;

const
  TRIGGER_RELOAD_MSECS = 60000; // 1 minute
  TRIGGER_CLEANUP_COUNT = 1000;

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
end;

destructor TdwlHTTPHandler_Log.Destroy;
begin
  // dispose hashes from triggers
  var TrigList := FTriggers.LockList;
  try
    for var Trigger in TrigList do
      Trigger.Free;
  finally
    FTriggers.UnlockList;
  end;
  FTriggers.Free;
  FLogSubmitAccess.Free;
  inherited Destroy;
end;

procedure TdwlHTTPHandler_Log.InitializeDatabase;
const
  SQL_CheckTable_LogDebug=
    'CREATE TABLE IF NOT EXISTS dwl_log_debug ('+
    '`Id` INT UNSIGNED NOT NULL AUTO_INCREMENT, '+
    '`IpAddress` VARCHAR(50), '+
    '`TimeStamp` TIMESTAMP DEFAULT CURRENT_TIMESTAMP, '+
    '`Level` TINYINT UNSIGNED DEFAULT 0, '+
    '`Source` VARCHAR(50), '+
    '`Channel` VARCHAR(50), '+
    '`Topic` VARCHAR(50), '+
    '`Msg` VARCHAR(250), '+
    '`ContentType` VARCHAR(50),	'+
    '`Content` LONGBLOB, '+
    'PRIMARY KEY (`ID`))';
  SQL_CheckTable_LogMessages=
    'CREATE TABLE IF NOT EXISTS dwl_log_messages ('+
    '`Id` INT UNSIGNED NOT NULL AUTO_INCREMENT, '+
    '`IpAddress` VARCHAR(50), '+
    '`TimeStamp` TIMESTAMP  DEFAULT CURRENT_TIMESTAMP, '+
    '`Level` TINYINT UNSIGNED DEFAULT 0, '+
    '`Source` VARCHAR(50), '+
    '`Channel` VARCHAR(50), '+
    '`Topic` VARCHAR(50), '+
    '`Msg` VARCHAR(250), '+
    '`ContentType` VARCHAR(50),	'+
    '`Content` LONGBLOB, '+
    'PRIMARY KEY (`ID`))';
  SQL_CheckTable_LogTriggers=
    'CREATE TABLE IF NOT EXISTS `dwl_log_triggers` ('+
    '`Id` INT UNSIGNED NOT NULL AUTO_INCREMENT, '+
    '`Level_From` TINYINT UNSIGNED, '+
    '`Level_To` TINYINT UNSIGNED, '+
    '`Channel` VARCHAR(50), '+
    '`Topic` VARCHAR(50), '+
    '`Parameters` TEXT, '+
    '`SuppressDuplicateSeconds` SMALLINT UNSIGNED, '+
    '`SuppressEvaluateContent` TINYINT UNSIGNED, '+
    'PRIMARY KEY (`Id`))';
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

procedure TdwlHTTPHandler_Log.CheckTriggers(TrigList: TList<TLogTrigger>);
const
  SQL_Get_Triggers=
    'SELECT Id, Level_From, Level_to, Channel, Topic, Parameters, SuppressDuplicateSeconds, SuppressEvaluateContent FROM dwl_log_triggers';
  GetTriggers_Idx_Id=0; GetTriggers_Idx_Level_From=1; GetTriggers_Idx_Level_to=2;
  GetTriggers_Idx_Channel=3; GetTriggers_Idx_Topic=4; GetTriggers_Idx_Parameters=5;
  GetTriggers_Idx_SuppressDuplicateSeconds=6; GetTriggers_Idx_SuppressEvaluateContent=7;
begin
  try
    var Tick := GetTickCount64;
    if Tick<FReloadTriggerTick then
      Exit;
    FReloadTriggerTick := Tick+TRIGGER_RELOAD_MSECS;
    var PreviousTriggers := TDictionary<integer, TLogTrigger>.Create;
    try
      // Keep current triggers
      for var Trig in TrigList do
        PreviousTriggers.Add(Trig.Id, Trig);
      TrigList.Clear;
      var Cmd := New_MySQLSession(FMySQL_Profile).CreateCommand(SQL_Get_Triggers);
      Cmd.Execute;
      while Cmd.Reader.Read do
      begin
        var TriggerId := Cmd.Reader.GetInteger(GetTriggers_Idx_Id);
        var Trigger: TLogTrigger;
        if not PreviousTriggers.TryGetValue(TriggerId, Trigger) then
          Trigger := TLogTrigger.Create(TriggerId)
        else
          PreviousTriggers.Remove(TriggerId);
        // never have triggers for levels below lsNotice
        // said in another way: triggering only acts on the items put in the dwl_log_messages table
        // (ignore the items from dwl_log_debug)
        Trigger.MinLevel := Max(integer(lsNotice), Cmd.Reader.GetInteger(GetTriggers_Idx_Level_From, true));
        Trigger.MaxLevel := Cmd.Reader.GetInteger(GetTriggers_Idx_Level_to, true, integer(lsFatal));
        var RegExStr := Cmd.Reader.GetString(GetTriggers_Idx_Channel, true, '.*');
        try
          Trigger.Channel := TRegEx.Create(RegExStr, [roCompiled, roSingleLine]);
        except
          TdwlLogger.Log('Invalid regex for Channel (TriggerID='+TriggerID.ToString+'): '+RegExStr);
          Trigger.Channel := TRegEx.Create('.*', [roCompiled, roSingleLine]);
        end;
        RegExStr := Cmd.Reader.GetString(GetTriggers_Idx_Topic, true, '.*');
        try
          Trigger.Topic := TRegEx.Create(RegExStr, [roCompiled, roSingleLine]);
        except
          TdwlLogger.Log('Invalid regex for Topic (TriggerID='+TriggerID.ToString+'): '+RegExStr);
          Trigger.Topic := TRegEx.Create('.*', [roCompiled, roSingleLine]);
        end;
        Trigger.Parameters := Cmd.Reader.GetString(GetTriggers_Idx_Parameters, true);
        Trigger.SuppressDuplicateMSecs := Max(0, Cmd.Reader.GetInteger(GetTriggers_Idx_SuppressDuplicateSeconds, true))*1000;
        Trigger.SuppressEvaluateContent := Cmd.Reader.GetInteger(GetTriggers_Idx_SuppressEvaluateContent, true)<>0;
        TrigList.Add(Trigger);
      end;
      // dispose no longer used triggers
      for var Trig in PreviousTriggers.Values do
        Trig.Free;
    finally
      PreviousTriggers.Free;
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
  if not State.TryGetRequestParamStr(SpecialRequestParam_RemoteIP, IpAddress) then
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
  Result := true;
end;

function TdwlHTTPHandler_Log.ProcessRequest(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := false;
  if SameText(State.URI, '') then
  begin
    if State.RequestMethod=dwlhttpPOST then
      Result := Post_Log(State)
    else
    if State.RequestMethod=dwlhttpOPTIONS then
      Result := Options_Log(State);
  end;
end;

procedure TdwlHTTPHandler_Log.ProcessTriggers(const IpAddress: string; Level: Byte; const Source, Channel, Topic, Msg, ContentType: string; const Content: TBytes);
begin
  try
    var TrigList := FTriggers.LockListRead;
    try
      CheckTriggers(TrigList);
      for var Trig in TrigList do
      begin
        if Trig.Channel.IsMatch(Channel) and Trig.Topic.IsMatch(Topic) and
          (Level>=Trig.MinLevel) and (Level<=Trig.MaxLevel) and
          (not Trig.IsSuppressed(Level, Source, Channel, Topic, Msg, Content)) then
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

constructor TLogTrigger.Create(AId: integer);
begin
  inherited Create;
  FId := AId;
  FCleanupCounter := TRIGGER_CLEANUP_COUNT;
end;

destructor TLogTrigger.Destroy;
begin
  FSuppressHashes.Free;
  inherited Destroy;
end;

function TLogTrigger.IsSuppressed(Level: Byte; const Source, Channel, Topic, Msg: string; Content: TBytes): boolean;
begin
  if FSuppressHashes=nil then
    Exit(false);
  var CurrentTick := GetTickCount64;
  // remove Old Hashes every thousand triggering moments
  if FCleanUpCounter=0 then
  begin
    var Enum := FSuppressHashes.GetEnumerator;
    try
      while ENum.MoveNext do
      begin
        if ENum.Current.Value<CurrentTick then
          FSuppressHashes.Remove(ENum.Current.Key);
      end;
    finally
      Enum.Free;
    end;
    FCleanUpCounter := TRIGGER_CLEANUP_COUNT;
  end;
  dec(FCleanUpCounter);
  // calculate hash
  var Hash := THashBobJenkins.Create;
  Hash.Update(Msg);
  Hash.Update(Level, SizeOf(Level));
  Hash.Update(Source);
  Hash.Update(Channel);
  Hash.Update(Topic);
  if SuppressEvaluateContent then
    Hash.Update(Content);
  // is hash present in
  var SuppressUntilLogTick: UInt64;
  Result := FSuppressHashes.TryGetValue(Hash.HashAsInteger, SuppressUntilLogTick);
  if Result then
  begin
    if SuppressUntilLogTick<CurrentTick then
    begin
      Result := false;
      FSuppressHashes.Remove(Hash.HashAsInteger);
    end;
  end;
  if not Result then
    FSuppressHashes.Add(Hash.HashAsInteger, CurrentTick+FSuppressDuplicateMSecs);
end;

procedure TLogTrigger.SetSuppressDuplicateMSecs(const Value: cardinal);
begin
  FSuppressDuplicateMSecs := Value;
  if (FSuppressDuplicateMSecs=0) and (FSuppressHashes<>nil) then
    FreeAndNil(FSuppressHashes);
  if (FSuppressDuplicateMSecs>0) and (FSuppressHashes=nil) then
   FSuppressHashes := TDictionary<integer, UInt64>.Create;
end;

end.

