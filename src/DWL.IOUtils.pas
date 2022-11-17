unit DWL.IOUtils;

interface

uses
  System.IOUtils;

type
  TdwlFile = record
    class function ExtractBareName(const Path: string=''): string; static;
  end;


implementation

uses
  System.SysUtils;

{ TdwlFile }

class function TdwlFile.ExtractBareName(const Path: string=''): string;
begin
  if Path='' then
    Result := ChangeFileExt(ExtractFileName(ParamStr(0)),'')
  else
    Result := ChangeFileExt(ExtractFileName(Path),'');
end;

end.
