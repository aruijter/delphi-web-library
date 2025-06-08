unit DWL.MediaTypes;

interface

uses
  System.Generics.Collections;

const
  MEDIA_TYPE_7Z = 'application/x-7z-compressed';
  MEDIA_TYPE_CSV = 'text/csv';
  MEDIA_TYPE_HTML = 'text/html';
  MEDIA_TYPE_IMAGE_PNG = 'image/png';
  MEDIA_TYPE_JOSE_JSON = 'application/jose+json';
  MEDIA_TYPE_JPEG = 'image/jpeg';
  MEDIA_TYPE_TIFF = 'image/tiff';
  MEDIA_TYPE_JSON = 'application/json';
  MEDIA_TYPE_JSONAPI = 'application/vnd.api+json';
  MEDIA_TYPE_MS_EXCEL = 'application/vnd.ms-excel';
  MEDIA_TYPE_MS_WORD = 'application/msword';
  MEDIA_TYPE_OPENXML_DOCUMENT = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document';
  MEDIA_TYPE_OPENXML_SPREADSHEET = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet';
  MEDIA_TYPE_OPENDOC_SPREADSHEET = 'application/vnd.oasis.opendocument.spreadsheet';
  MEDIA_TYPE_OCTET_STREAM = 'application/octet-stream';
  MEDIA_TYPE_PDF = 'application/pdf';
  MEDIA_TYPE_PNG = 'image/png';
  MEDIA_TYPE_PLAIN = 'text/plain';
  MEDIA_TYPE_X_WWW_FORM_URLENCODED = 'application/x-www-form-urlencoded';
  MEDIA_TYPE_MULTIPART_MIXED='multipart/mixed';
  MEDIA_TYPE_MULTIPART_ALTERNATIVE='multipart/alternative';
  MEDIATYPE_TAR_GZIP='application/tar+gzip';

  FILE_EXT_CSV = '.csv';
  FILE_EXT_DOC = '.doc';
  FILE_EXT_DOCX = '.docx';
  FILE_EXT_HTM = '.htm';
  FILE_EXT_HTML = '.html';
  FILE_EXT_JPEG = '.jpeg';
  FILE_EXT_JPG = '.jpg';
  FILE_EXT_PDF = '.pdf';
  FILE_EXT_PNG = '.png';
  FILE_EXT_TXT = '.txt';
  FILE_EXT_TIF = '.tif';
  FILE_EXT_TIFF = '.tiff';
  FILE_EXT_XLS = '.xls';
  FILE_EXT_XLSX = '.xlsx';
  FILE_EXT_GZ = '.gz';
  FILE_EXT_TAR_GZ = '.tar.gz';

type
  TMediaTypeHelper = class abstract
  strict private
    class var
      FFileExtension2MediaType: TDictionary<string, string>;
      FMediaType2FileExtension: TDictionary<string, string>;
    class procedure InitLookupLists;
  public
    class destructor Destroy;
    class function GetMediaTypeByPath(const Path: string): string;
    class function GetMediaTypeByFileExtension(const FileExtension: string): string;
    class function GetFileExtensionByMediaType(const MediaType: string): string;
  end;

implementation

uses
  System.SysUtils;

{ TMediaTypeHelper }

class destructor TMediaTypeHelper.Destroy;
begin
  FFileExtension2MediaType.Free;
  FMediaType2FileExtension.Free;
  inherited;
end;

class function TMediaTypeHelper.GetFileExtensionByMediaType(const MediaType: string): string;
begin
  InitLookupLists;
  if not FFileExtension2MediaType.TryGetValue(MediaType.ToLower, Result) then
    Result := '';
  if Sametext(MediaType, MEDIA_TYPE_TIFF) then
    Result := '.tif'
  else
  if Sametext(MediaType, 'application/tar+gzip') then
    Result := '.tar.gz'
end;

class function TMediaTypeHelper.GetMediaTypeByFileExtension(const FileExtension: string): string;
begin
  InitLookupLists;
  var Ext := FileExtension.ToLower;
  if Ext.Substring(0,1)<>'.' then
    Ext := '.'+Ext;
  if not FFileExtension2MediaType.TryGetValue(Ext, Result) then
    Result := '';
end;

class function TMediaTypeHelper.GetMediaTypeByPath(const Path: string): string;
begin
  var Extension := ExtractFileExt(Path);
  // handle special 'double extension'
  if SameText(Extension, FILE_EXT_GZ) and Path.EndsWith(FILE_EXT_TAR_GZ, true) then
    Extension := FILE_EXT_TAR_GZ;
  Result := GetMediaTypeByFileExtension(Extension);
end;

class procedure TMediaTypeHelper.InitLookupLists;
begin
  if FFileExtension2MediaType<>nil then
    Exit;
  FFileExtension2MediaType := TDictionary<string, string>.Create;
  FFileExtension2MediaType.Add(FILE_EXT_CSV, MEDIA_TYPE_CSV);
  FFileExtension2MediaType.Add(FILE_EXT_DOC, MEDIA_TYPE_MS_WORD);
  FFileExtension2MediaType.Add(FILE_EXT_DOCX, MEDIA_TYPE_OPENXML_DOCUMENT);
  FFileExtension2MediaType.Add(FILE_EXT_HTM, MEDIA_TYPE_HTML);
  FFileExtension2MediaType.Add(FILE_EXT_HTML, MEDIA_TYPE_HTML);
  FFileExtension2MediaType.Add(FILE_EXT_JPEG, MEDIA_TYPE_JPEG);
  FFileExtension2MediaType.Add(FILE_EXT_JPG, MEDIA_TYPE_JPEG);
  FFileExtension2MediaType.Add(FILE_EXT_PDF, MEDIA_TYPE_PDF);
  FFileExtension2MediaType.Add(FILE_EXT_PNG, MEDIA_TYPE_PNG);
  FFileExtension2MediaType.Add(FILE_EXT_TXT, MEDIA_TYPE_PLAIN);
  FFileExtension2MediaType.Add(FILE_EXT_TIF, MEDIA_TYPE_TIFF);
  FFileExtension2MediaType.Add(FILE_EXT_TIFF, MEDIA_TYPE_TIFF);
  FFileExtension2MediaType.Add(FILE_EXT_XLS, MEDIA_TYPE_MS_EXCEL);
  FFileExtension2MediaType.Add(FILE_EXT_XLSX, MEDIA_TYPE_OPENXML_SPREADSHEET);
  FFileExtension2MediaType.Add(FILE_EXT_TAR_GZ, MEDIATYPE_TAR_GZIP);

  FMediaType2FileExtension := TDictionary<string, string>.Create;
  FMediaType2FileExtension.Add(MEDIA_TYPE_CSV, FILE_EXT_CSV);
  FMediaType2FileExtension.Add(MEDIA_TYPE_MS_WORD, FILE_EXT_DOC);
  FMediaType2FileExtension.Add(MEDIA_TYPE_OPENXML_DOCUMENT, FILE_EXT_DOCX);
  FMediaType2FileExtension.Add(MEDIA_TYPE_HTML, FILE_EXT_HTML);
  FMediaType2FileExtension.Add(MEDIA_TYPE_JPEG, FILE_EXT_JPEG);
  FMediaType2FileExtension.Add(MEDIA_TYPE_PDF, FILE_EXT_PDF);
  FMediaType2FileExtension.Add(MEDIA_TYPE_PNG, FILE_EXT_PNG);
  FMediaType2FileExtension.Add(MEDIA_TYPE_PLAIN, FILE_EXT_TXT);
  FMediaType2FileExtension.Add(MEDIA_TYPE_TIFF, FILE_EXT_TIF);
  FMediaType2FileExtension.Add(MEDIA_TYPE_MS_EXCEL, FILE_EXT_XLS);
  FMediaType2FileExtension.Add(MEDIA_TYPE_OPENXML_SPREADSHEET, FILE_EXT_XLSX);
  FMediaType2FileExtension.Add(MEDIATYPE_TAR_GZIP, FILE_EXT_TAR_GZ);
end;

end.

