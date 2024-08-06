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
  FILE_EXT_XLS = '.xls';
  FILE_EXT_XLSX = '.xlsx';

type
  TMediaTypeHelper = class abstract
  strict private
    class var
      FKnownFileExtensions: TDictionary<string, string>;
    class procedure InitKnownFileExtensions;
  public
    class destructor Destroy;
    class function GetMediaTypeByFileExtension(const FileExtension: string): string;
  end;

implementation

uses
  System.SysUtils;

{ TMediaTypeHelper }

class destructor TMediaTypeHelper.Destroy;
begin
  FKnownFileExtensions.Free;
  inherited;
end;

class function TMediaTypeHelper.GetMediaTypeByFileExtension(const FileExtension: string): string;
begin
  InitKnownFileExtensions;
  var Ext := FileExtension.ToLower;
  if Ext.Substring(0,1)<>'.' then
    Ext := '.'+Ext;
  if not FKnownFileExtensions.TryGetValue(Ext, Result) then
    Result := '';
end;

class procedure TMediaTypeHelper.InitKnownFileExtensions;
begin
  if FKnownFileExtensions<>nil then
    Exit;
  FKnownFileExtensions := TDictionary<string, string>.Create;
  FKnownFileExtensions.Add(FILE_EXT_CSV, MEDIA_TYPE_CSV);
  FKnownFileExtensions.Add(FILE_EXT_DOC, MEDIA_TYPE_MS_WORD);
  FKnownFileExtensions.Add(FILE_EXT_DOCX, MEDIA_TYPE_OPENXML_DOCUMENT);
  FKnownFileExtensions.Add(FILE_EXT_HTM, MEDIA_TYPE_HTML);
  FKnownFileExtensions.Add(FILE_EXT_HTML, MEDIA_TYPE_HTML);
  FKnownFileExtensions.Add(FILE_EXT_JPEG, MEDIA_TYPE_JPEG);
  FKnownFileExtensions.Add(FILE_EXT_JPG, MEDIA_TYPE_JPEG);
  FKnownFileExtensions.Add(FILE_EXT_PDF, MEDIA_TYPE_PDF);
  FKnownFileExtensions.Add(FILE_EXT_PNG, MEDIA_TYPE_PNG);
  FKnownFileExtensions.Add(FILE_EXT_TXT, MEDIA_TYPE_PLAIN);
  FKnownFileExtensions.Add(FILE_EXT_XLS, MEDIA_TYPE_MS_EXCEL);
  FKnownFileExtensions.Add(FILE_EXT_XLSX, MEDIA_TYPE_OPENXML_SPREADSHEET);
end;

end.
