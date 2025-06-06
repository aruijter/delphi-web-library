unit DWL.Compression;

interface

uses
  JclCompression, DWL.Classes, DWL.IOUtils;

type
  TdwlCompression = record
  public
    class constructor Create;
    class function ExtractArchive(const ArchiveFileName: string; const DestinationDir: string=''; const Password: string=''; OnProgress: TJclCompressionProgressEvent=nil): TdwlResult; static;
    class function ZipFile(const ArchiveFileName, FileToZip: string): TdwlResult; static;
    class function ZipDirectory(const ArchiveFileName, DirectoryToZip: string; Options: TdwlIOEnumOptions=[ioIncludeFiles]; const FileMask: string = '*.*'): TdwlResult; static;
  end;

implementation

uses
  System.SysUtils, sevenzip, System.Classes;

{ TdwlCompression }

class constructor TdwlCompression.Create;
begin
  inherited;
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

class function TdwlCompression.ZipDirectory(const ArchiveFileName, DirectoryToZip: string; Options: TdwlIOEnumOptions=[ioIncludeFiles]; const FileMask: string = '*.*'): TdwlResult;
begin
  try
    // in case someone asked something strange:
    Options := Options - [ioIncludeDirectories];
    var CompressArchive :=  TJcl7ZCompressArchive.Create(ArchiveFileName);
    try
      var ENum := TdwlDirectory.Enumerator(DirectoryToZip, Options, FileMask);
      while ENum.MoveNext do
        CompressArchive.AddFile(ENum.Current.RelativePathName, ENum.Current.FullPathName);
      CompressArchive.Compress;
    finally
      CompressArchive.Free;
    end;
  except
    on E: Exception do
      Result.AddErrorMsg(E.Message);
  end;
end;

class function TdwlCompression.ZipFile(const ArchiveFileName, FileToZip: string): TdwlResult;
begin
  try
    var CompressArchive :=  TJcl7ZCompressArchive.Create(ArchiveFileName);
    try
      CompressArchive.AddFile(ExtractFilename(FileToZip), FileToZip);
      CompressArchive.Compress;
    finally
      CompressArchive.Free;
    end;
  except
    on E: Exception do
      Result.AddErrorMsg(E.Message);
  end;
end;

end.
