/// <summary>
///   A specific handler that can be added to a TdwlHTTPServer for exposing
///   a possibility to drop an email for sending
/// </summary>
unit DWL.HTTP.Server.Handler.Mail;

interface

uses
  DWL.HTTP.Server, DWL.HTTP.Server.Types, DWL.Params;

type
  TdwlHTTPHandler_Mail = class(TdwlHTTPHandler)
  strict private
    FLogSecret: string;
    function Post_Mail(const State: PdwlHTTPHandlingState): boolean;
    function Options_Mail(const State: PdwlHTTPHandlingState): boolean;
  protected
    function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; override;
  public
    constructor Create(AParams: IdwlParams);
    function Authorize(const State: PdwlHTTPHandlingState): boolean; override;
  end;

implementation

uses
  DWL.HTTP.Consts, DWL.HTTP.Server.Globals, DWL.HTTP.Server.Utils,
  System.SysUtils, DWL.Classes, IdMessage, DWL.Mail.Queue;


{ TdwlHTTPHandler_Mail }

function TdwlHTTPHandler_Mail.Authorize(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := true;
end;

constructor TdwlHTTPHandler_Mail.Create(AParams: IdwlParams);
begin
  inherited Create;
  FLogSecret := AParams.StrValue(param_LogSecret);
end;

function TdwlHTTPHandler_Mail.Options_Mail(const State: PdwlHTTPHandlingState): boolean;
begin
  serverProcs.SetHeaderValueProc(State, 'Access-Control-Allow-Origin', '*');
  serverProcs.SetHeaderValueProc(State, 'Access-Control-Allow-Methods', 'OPTIONS, POST');
  var Hdrs: string;
  if State.TryGetHeaderValue('Access-Control-Request-Headers', Hdrs) then
    serverProcs.SetHeaderValueProc(State, 'Access-Control-Allow-Headers', PWideChar(Hdrs));
  Result := true;
end;

function TdwlHTTPHandler_Mail.Post_Mail(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := false;
  var LogSecret: string;
  if not State.TryGetRequestParamStr('secret', LogSecret) then
    Exit;
  if not SameText(LogSecret, FLogSecret) then
    Exit;
  var Data: pointer;
  var DataSize: Int64;
  if serverProcs.GetPayloadPtrProc(State, Data, DataSize) and (DataSize>0) then
  begin
    var Stream := TdwlReadOnlyBufferStream.Create(Data, DataSize);
    try
      var Msg := TIdMessage.Create;
      try
        Msg.LoadFromStream(Stream);
        var BccRecipients: string;
        if State.TryGetRequestParamStr('bcc', BccRecipients) then
          Msg.BccList.EmailAddresses := Bccrecipients;
        TdwlMailQueue.QueueForSending(msg);
        Result := true;
      finally
        Msg.Free;
      end;
    finally
      Stream.Free;
    end;
  end;
end;

function TdwlHTTPHandler_Mail.ProcessRequest(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := false;
  if SameText(State.URI, '') then
  begin
    if State.Command=dwlhttpPOST then
      Result := Post_Mail(State)
    else
    if State.Command=dwlhttpOPTIONS then
      Result := Options_Mail(State);
  end;
end;

end.

