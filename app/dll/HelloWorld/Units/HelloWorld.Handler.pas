unit HelloWorld.Handler;

interface

uses
  DWL.Server.Handler.DLL.Classes, DWL.Server.Types;

type
  THandler_HelloWorld = class(TdwlDLLHandling)
  strict private
    class function Get_(const State: PdwlHTTPHandlingState): boolean;
  public
    class function Authorize(const State: PdwlHTTPHandlingState): boolean; override;
    class procedure Configure(const Params: string); override;
  end;


implementation

uses
  DWL.HTTP.Consts, DWL.Server.Utils;

{ THandler_HelloWorld }

class function THandler_HelloWorld.Authorize(const State: PdwlHTTPHandlingState): boolean;
begin
  // everyone may see my magnificent 'Hello World!' message
  Result := true;
  // most of the time you want to call the inherited Authorize (or not even override this function)
  // to rely on accesstoken authentication
end;

class procedure THandler_HelloWorld.Configure(const Params: string);
begin
  inherited Configure(Params);
  // register my sub uri on which to output my message
  RegisterHandling(dwlhttpGET, '', Get_, []);
end;

class function THandler_HelloWorld.Get_(const State: PdwlHTTPHandlingState): boolean;
begin
  // THE moment: output my message
  State.SetContentText('Hello World!');
  // indicate everything went well
  Result := true;
end;

end.
