/// <summary>
///   Maybe the most important handler of the TdwlHTTPServer implementation.
///   Defines the handler that makes it possible to develop handling functionality
///   in DLL's. These DLL's can dymanically replaced are added to a running server
///   See the DLL bootstrap project to start a DLL implementation
/// </summary>
unit DWL.Server.Handler.DLL;

interface

uses
  System.Classes, DWL.Server.Types, DWL.Params, DWL.Server;

type
  TdwlHTTPHandler_DLL = class(TdwlHTTPHandler)
  strict private
    FEndPoint: string;
    FDLLHandle: HModule;
    FProcessProc: TDLL_ProcessRequestProc;
    FAuthorizeProc: TDLL_AuthorizeProc;
    FAllowOrigins: TStringList;
    FAllowAllOrigins: boolean;
    FDoAllowOrigins: boolean;
  protected
    function LogDescription: string; override;
    function Authorize(const State: PdwlHTTPHandlingState): boolean; override;
    function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; override;
  public
    constructor Create(DLLHandle: HModule; ProcessProc: TDLL_ProcessRequestProc; AuthorizeProc: TDLL_AuthorizeProc; const Endpoint: string; Params: IdwlParams);
    destructor Destroy; override;
  end;

implementation

uses
  Winapi.Windows, System.SysUtils, DWL.Server.Globals,
  DWL.Server.Utils;

const
  Param_AllowOrigin = 'AllowOrigin';

{ TDLL_RestHandler }

function TdwlHTTPHandler_DLL.Authorize(const State: PdwlHTTPHandlingState): boolean;
begin
  try
    Result := FAuthorizeProc(State);
  except
    // just protect DLL function, an exception should never be returned, but you never know
    Result := false;
  end;
end;

constructor TdwlHTTPHandler_DLL.Create(DLLHandle: HModule; ProcessProc: TDLL_ProcessRequestProc; AuthorizeProc: TDLL_AuthorizeProc; const EndPoint: string; Params: IdwlParams);
var
  ConfigureProc: TDLL_ConfigureProc;
begin
  inherited Create;
  FDLLHandle := DLLHandle;
  FProcessProc := ProcessProc;
  FAuthorizeProc := AuthorizeProc;
  FEndPoint := Endpoint;
  FAllowOrigins := TStringList.Create;
  FAllowOrigins.CaseSensitive := false;
  FAllowOrigins.Delimiter := ',';
  FAllowOrigins.DelimitedText := Params.StrValue(Param_AllowOrigin);
  FDoAllowOrigins := FAllowOrigins.Count>0;
  FAllowAllOrigins := FAllowOrigins.IndexOf('*')>=0;
  ConfigureProc := GetProcAddress(FDLLHandle, 'Configure');
  if Assigned(ConfigureProc) then
  try
    ConfigureProc(@serverProcs, PWideChar(Params.GetAsNameValueText));
  except
    // just protect DLL function, an exception should never be returned, but you never know
  end;
  FWrapUpProc := GetProcAddress(FDLLHandle, 'WrapUp');
end;

destructor TdwlHTTPHandler_DLL.Destroy;
begin
  FAllowOrigins.Free;
  inherited Destroy;
  // inherited destroy will call the FWrapUpProc(nil) that's inside the DLL, so FreeLibary after inherited
  FreeLibrary(FDLLHandle);
end;

function TdwlHTTPHandler_DLL.LogDescription: string;
begin
  Result := 'DLL at '+FEndPoint;
end;

function TdwlHTTPHandler_DLL.ProcessRequest(const State: PdwlHTTPHandlingState): boolean;
begin
  if Assigned(FProcessProc) then
  begin
    try
      Result := FProcessProc(State);
    except
      // just protect DLL function, an exception should never be returned, but you never know
      Result := false;
    end;
    if Result and FDoAllowOrigins then
    begin
      var Origin: string;
      if State.TryGetHeaderValue('Origin', Origin) then
      begin
        if FAllowAllOrigins or (FAllowOrigins.IndexOf(Origin)>=0) then
          State.SetHeaderValue('Access-Control-Allow-Origin', Origin);
      end;
    end;
  end
  else
    Result := false;
end;

end.
