unit DWL.Compression;

interface

uses
  JclCompression, DWL.Classes;

type
  TdwlCompression = record
  public
    class constructor Create;
    class function ExtractArchive(const ArchiveFileName: string; const DestinationDir: string=''; const Password: string=''; OnProgress: TJclCompressionProgressEvent=nil): TdwlResult; static;
  end;

implementation

uses
  System.SysUtils, sevenzip;

{ TdwlCompression }

class constructor TdwlCompression.Create;
begin
  {$IFDEF WIN64}
  SevenzipLibraryName := '7z64.dll';
  {$ELSE}
  SevenzipLibraryName := '7z.dll';
  {$ENDIF}
end;

class function TdwlCompression.ExtractArchive(const ArchiveFileName: string; const DestinationDir: string=''; const Password: string=''; OnProgress: TJclCompressionProgressEvent=nil): TdwlResult;
begin
  try
    ForceDirectories(DestinationDir);
    var DecompressArchiveClass := GetArchiveFormats.FindDecompressFormat(ArchiveFileName);
    var Archive := DecompressArchiveClass.Create(ArchiveFileName);
    try
      Archive.Password := Password;
      Archive.OnProgress := OnProgress;
      Archive.ListFiles;
      Archive.ExtractAll(DestinationDir, True);
    finally
      Archive.Free;
    end;
  except
    on E: Exception do
      Result.AddErrorMsg(E.Message);
  end;
end;

end.