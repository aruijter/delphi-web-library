unit DWL.Server.Handler.Ping;

interface

uses
  DWL.Server, DWL.Server.Types;

type
  TdwlHTTPHandler_Ping = class(TdwlHTTPHandler)
  strict private
    function Get_Ping(const State: PdwlHTTPHandlingState): boolean;
  protected
    function Authorize(const State: PdwlHTTPHandlingState): boolean; override;
    function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; override;
  end;


implementation

uses
  System.SysUtils, DWL.HTTP.Consts, DWL.Server.Utils, DWL.MediaTypes,
  DWL.TCP.Consts;

{ TdwlHTTPHandler_Ping }

function TdwlHTTPHandler_Ping.Authorize(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := true;
end;

function TdwlHTTPHandler_Ping.Get_Ping(const State: PdwlHTTPHandlingState): boolean;
begin
  State.Flags := FLAG_SKIPLOG;
  State.SetContentType(MEDIA_TYPE_HTML);
  State.SetContentText('<!DOCTYPE html><html><head><title>Hello</title></head><body>Hello</body></html>');
  Result := true;
end;

function TdwlHTTPHandler_Ping.ProcessRequest(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := false;
  if SameText(State.URI, '') then
  begin
    if State.RequestMethod=dwlhttpGET then
      Result := Get_Ping(State)
  end;
end;

end.
