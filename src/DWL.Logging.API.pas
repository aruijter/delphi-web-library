/// <summary>
///   A Logging Dispatcher to post information to the DWL Logging API
/// </summary>
unit DWL.Logging.API;

interface

uses
  DWL.Logging;

/// <summary>
///   Enabled Dispatching to the DWL Logging API
///   The endpoint and logsecret of the API must be provided
///   and if needed an identifier (to receive specific
///   destination logs) <br />
/// </summary>
function EnableLogDispatchingToAPI(const Endpoint, LogSecret: string; const Identifier: string=''): IdwlLogDispatcher;

implementation

uses
  DWL.HTTP.Client, System.NetEncoding, System.SysUtils, DWL.HTTP.Consts,
  System.Classes;

type
  TdwlLogDispatcher_API = class(TdwlLogDispatcher)
  private
    FEndpoint: string;
    FLogSecret: string;
  protected
    procedure DispatchLog(LogItem: PdwlLogItem); override;
  public
    constructor Create(const Identifier, Endpoint, LogSecret: string);
  end;

function EnableLogDispatchingToAPI(const Endpoint, LogSecret: string; const Identifier: string=''): IdwlLogDispatcher;
begin
  Result := TdwlLogDispatcher_API.Create(Identifier, Endpoint, LogSecret);
  TdwlLogger.RegisterDispatcher(Result);
end;

{ TdwlLogDispatcher_API }

constructor TdwlLogDispatcher_API.Create(const Identifier, Endpoint, LogSecret: string);
begin
  inherited Create(Identifier);
  FEndpoint := Endpoint;
  FLogSecret := LogSecret;
end;

procedure TdwlLogDispatcher_API.DispatchLog(LogItem: PdwlLogItem);
begin
  var Rq := New_HTTPRequest(FEndpoint+
    '?secret='+FLogSecret+
    '&msg='+TNetEncoding.URL.Encode(LogItem.Msg)+
    '&level='+integer(LogItem.SeverityLevel).ToString+
    '&source='+TNetEncoding.URL.Encode(LogItem.Source)+
    '&channel='+TNetEncoding.URL.Encode(LogItem.Channel)+
    '&topic='+TNetEncoding.URL.Encode(LogItem.Topic));
  if Length(LogItem.Content)>0 then
  begin
    Rq.PostStream.WriteData(@logItem.Content[0], Length(LogItem.Content));
    Rq.Header[HTTP_FIELD_CONTENT_TYPE] := LogItem.ContentType;
  end;
  Rq.Method  := HTTP_METHOD_POST;
  Rq.Execute;
end;

end.
