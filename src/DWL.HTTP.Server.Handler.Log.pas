/// <summary>
///   A specific handler that can be added to a TdwlHTTPServer for exposing the
///   DWL logging API. Requests are handled triggers, fieltered etc and at the
///   end saved in the database
/// </summary>
unit DWL.HTTP.Server.Handler.Log;

interface

uses
  DWL.HTTP.Server, DWL.HTTP.Server.Types, DWL.Params,
  System.Generics.Collections, System.SysUtils;

const
  logdestinationServerConsole='serverconsole';

const
  Param_EMail_To = 'email_to';
  Param_Email_From = 'email_from';
  Param_EMail_FromName = 'email_fromname';
  Param_EMail_Subject = 'email_subject';

type
  TLogTrigger = record
    MinLevel: byte;
    Channel: string;
    Topic: string;
    Parameters: string;
  end;

  TdwlHTTPHandler_Log = class(TdwlHTTPHandler)
  strict private
    FTriggers: TList<TLogTrigger>;
    FLogSecret: string;
    FMySQL_Profile: IdwlParams;
    procedure InitializeDatabase;
    function Post_Log(const State: PdwlHTTPHandlingState): boolean;
    function Options_Log(const State: PdwlHTTPHandlingState): boolean;
    procedure ProcessTriggers(const IpAddress: string; Level: Byte; const Source, Channel, Topic, Msg, ContentType: string; const Content: TBytes);
  protected
    function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; override;
  public
    constructor Create(AParams: IdwlParams);
    destructor Destroy; override;
    function Authorize(const State: PdwlHTTPHandlingState): boolean; override;
    procedure SubmitLog(const IpAddress: string; Level: Byte; const Source, Channel, Topic, Msg, ContentType: string; const Content: TBytes);
  end;

implementation

uses
  DWL.Params.Consts, DWL.MySQL, DWL.HTTP.Consts, DWL.Logging,
  DWL.HTTP.Server.Globals, DWL.HTTP.Server.Utils, System.Masks, System.Classes,
  IdMessage, System.StrUtils, IdAttachmentMemory, DWL.Mail.Queue;

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
  FLogSecret := AParams.StrValue(param_LogSecret);
  FTriggers := TList<TLogTrigger>.Create;
  InitializeDatabase;
end;

destructor TdwlHTTPHandler_Log.Destroy;
begin
  FTriggers.Free;
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
    '`Level` TINYINT  DEFAULT 0, '+
    '`Channel` VARCHAR(50), '+
    '`Topic` VARCHAR(50), '+
    '`Parameters` TEXT, '+
    'PRIMARY KEY (`ID`))';
  SQL_Get_Triggers=
    'SELECT Level, Channel, Topic, Parameters FROM dwl_log_triggers';
begin
  FMySQL_Profile.WriteValue(Param_CreateDatabase, true);
  FMySQL_Profile.WriteValue(Param_TestConnection, true);
  var Session := New_MySQLSession(FMySQL_Profile);
  FMySQL_Profile.ClearKey(Param_CreateDatabase);
  FMySQL_Profile.ClearKey(Param_TestConnection);
  Session.CreateCommand(SQL_CheckTable_LogDebug).Execute;
  Session.CreateCommand(SQL_CheckTable_LogMessages).Execute;
  Session.CreateCommand(SQL_CheckTable_LogTriggers).Execute;
  var Cmd := Session.CreateCommand(SQL_Get_Triggers);
  Cmd.Execute;
  while Cmd.Reader.Read do
  begin
    var Trig: TLogTrigger;
    Trig.MinLevel := Cmd.Reader.GetInteger(0, true);
    Trig.Channel := Cmd.Reader.GetString(1, true, '*');
    Trig.Topic := Cmd.Reader.GetString(2, true, '*');
    Trig.Parameters := Cmd.Reader.GetString(3, true, '');
    FTriggers.Add(Trig);
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
  SubmitLog(IpAddress, Level, Source, Channel, Topic, Msg, ContentType, Content);
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
  for var Trig in FTriggers do
  begin
    if MatchesMask(Channel, Trig.Channel) and MatchesMask(Topic, Trig.Topic) and (Level>=Trig.MinLevel) then
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
          Subject := ReplaceStr(Subject, '$(msg)', Msg);
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
end;

procedure TdwlHTTPHandler_Log.SubmitLog(const IpAddress: string; Level: Byte; const Source, Channel, Topic, Msg, ContentType: string; const Content: TBytes);
const
  SQL_Insert_Debug=
    'INSERT INTO dwl_log_debug (IpAddress, Level, Source, Channel, Topic, Msg, ContentType, Content) values (?, ?, ?, ?, ?, ?, ?, ?)';
  SQL_Insert_Log=
    'INSERT INTO dwl_log_messages (IpAddress, Level, Source, Channel, Topic, Msg, ContentType, Content) values (?, ?, ?, ?, ?, ?, ?, ?)';
begin
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
  Cmd.Parameters.SetTextDataBinding(5, Msg);
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
end;

end.

