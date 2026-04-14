unit ServeFiles.Handler;

interface

uses
  DWL.Server.Handler.DLL.Classes, DWL.Server.Types;

type
  THandler_ServeFiles = class(TdwlDLLHandling)
  strict private
    class var FBaseDir: string;
  public
    class function Authorize(const State: PdwlHTTPHandlingState): boolean; override;
    class procedure Configure(const Params: string); override;
    class procedure ProcessRequest(const State: PdwlHTTPHandlingState; var Success: boolean); override;
  end;


implementation

uses
  DWL.HTTP.Consts, DWL.Server.Utils, Winapi.Windows, System.SysUtils,
  System.Classes, DWL.Server.Globals, DWL.MediaTypes;

const
  Param_BaseDir = 'BaseDir';

{ THandler_ServeFiles }

class function THandler_ServeFiles.Authorize(const State: PdwlHTTPHandlingState): boolean;
begin
  // The servefiles project does current expose all files to everyone
  Result := true;
end;

class procedure THandler_ServeFiles.Configure(const Params: string);
begin
  inherited Configure(Params);
  FBaseDir := FConfigParams.StrValue(Param_BaseDir, ExtractFileDir(GetModuleName(HInstance))+'\content');
end;

class procedure THandler_ServeFiles.ProcessRequest(const State: PdwlHTTPHandlingState; var Success: boolean);
begin
  var Filename := FBaseDir+StringReplace(State.URI, '/', '\', [rfReplaceAll]);
  if (pos('..', FileName)>0) or not FileExists(FileName) then
  begin
    Success := false;
    Exit;
  end;
  var FileStream := TFileStream.Create(Filename, fmOpenRead);
  try
    var ContentBuffer: pointer := nil;
    var Size: Int64 := FileStream.Size;
    serverProcs.ArrangeContentBufferProc(State, ContentBuffer, Size);
    FileStream.Read(ContentBuffer^, Size);
  finally
    FileStream.Free;
  end;
  State.SetContentType(TMediaTypeHelper.GetMediaTypeByPath(FileName));
  Success := true;
end;

end.
