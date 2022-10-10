unit DWLServer.Section;

interface

uses
  DWLServer.Server, DWL.Params, DWL.MySQL;

type
  TDWLServerSection = class
  strict private
    FServer: TdwlServerCore;
    procedure FeedConfig(Params: IdwlParams);
    procedure InsertOrUpdateDbParameter(Session: IdwlMySQLSession; const Key, Value: string);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure StartServer;
    procedure StopServer;
  end;

implementation

uses
  DWL.Params.Utils, System.SysUtils, Winapi.ActiveX,
  System.Win.ComObj, DWLServer.Consts, System.Rtti, System.Classes,
  DWL.Params.Consts, DWL.HTTP.Consts;

{ TDWLServerSection }

procedure TDWLServerSection.AfterConstruction;
begin
  inherited AfterConstruction;
  FServer := TdwlServerCore.Create;
end;

procedure TDWLServerSection.BeforeDestruction;
begin
  FServer.Free;
  inherited BeforeDestruction;
end;

procedure TDWLServerSection.FeedConfig(Params: IdwlParams);
begin
  // for now only post a restart to the main thread, later on we'll think about storing configuration changes
  TThread.Queue(nil,  procedure
    begin
      StopServer;
      StartServer;
    end);
end;

procedure TDWLServerSection.InsertOrUpdateDbParameter(Session: IdwlMySQLSession; const Key, Value: string);
const
  SQL_InsertOrUpdateParameter=
    'INSERT INTO dwl_parameters (`Key`, `Value`) VALUES (?, ?) ON DUPLICATE KEY UPDATE `Value`=VALUES(`Value`)';
begin
  var Cmd := Session.CreateCommand(SQL_InsertOrUpdateParameter);
  Cmd.Parameters.SetTextDataBinding(0, Key);
  Cmd.Parameters.SetTextDataBinding(1, Value);
  Cmd.Execute;
end;

procedure TDWLServerSection.StartServer;
const
  SQL_CheckTable_Parameters =
    'CREATE TABLE IF NOT EXISTS dwl_parameters (Id SMALLINT NOT NULL AUTO_INCREMENT, `Key` VARCHAR(50) NOT NULL, `Value` TEXT, PRIMARY KEY(Id), UNIQUE INDEX KeyIndex (`Key`))';
  SQL_GetParameters =
    'SELECT `Key`, `Value` FROM dwl_parameters';
begin
  var Params := New_Params;
  // at first take parameters from the commandline
  TdwlParamsUtils.Import_CommandLine(Params);
  // secondary pick parameters from inifile
  TdwlParamsUtils.Import_IniFile_Section(Params, ChangeFileExt(ParamStr(0), '.ini'),
    Params.StrValue(Param_Section_Dwl_Db, ParamDef_Section_Dwl_Db));
  // and after that take parameters from the dwl database
  //  (default bootstrap database is dwl)
  if Params.StrValue(Param_Db)='' then
    Params.WriteValue(Param_Db, 'dwl');
  Params.WriteValue(Param_CreateDatabase, true);
  Params.WriteValue(Param_TestConnection, true);
  var Session := New_MySQLSession(Params);
  Params.ClearKey(Param_CreateDatabase);
  Params.ClearKey(Param_TestConnection);
  // get additional parameters from database configuration
  Session.CreateCommand(SQL_CheckTable_Parameters).Execute;
  var Cmd := Session.CreateCommand(SQL_GetParameters);
  Cmd.Execute;
  var Reader := Cmd.Reader;
  while Reader.Read do
    Params.WriteValue(Reader.GetString(0), Reader.GetString(1, true));
  // Set LogSecret if needed
  if not Params.ContainsKey(param_LogSecret) then
  begin
    var LogSecret := Random(MaxInt).ToHexString;
    Params.WriteValue(param_LogSecret, LogSecret);
    InsertOrUpdateDbParameter(Session, Param_LogSecret, LogSecret);
  end;
  // and provide the config feedback procedure
  Params.WriteValue(Param_FeedConfigProc, TValue.From<TFeedConfigProc>(FeedConfig));
  // finally start the server
  FServer.Start(Params);
end;

procedure TDWLServerSection.StopServer;
begin
  FServer.Stop;
end;

end.
