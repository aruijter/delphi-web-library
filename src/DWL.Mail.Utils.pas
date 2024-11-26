unit DWL.Mail.Utils;

interface

uses
  IdMessage, System.SysUtils, DWL.Classes, IdAttachment, System.Classes;

type
  TdwlMailCheckOption=(mcEmptyStringIsValid, mcDoNotTrimSpaces, mcEvaluateCommaSeparatedList);
  TdwlMailCheckOptions=set of TdwlMailCheckOption;

  TdwlMailUtils = record
    class function IsValidEmailAddress(const Value: string; Options: TdwlMailCheckOptions=[]): boolean; static;
    class function SendMailToAPI(const Endpoint, LogSecret: string; Msg: TIdMessage): TdwlResult; static;
    class function IdMessageToBytes(Msg: TIdMessage): TBytes; static;
    class function IdMessageToString(Msg: TIdMessage): string; static;
    class procedure FilldIdMessageFromString(Msg: TIdMessage; const Value: string); static;
  end;

  TdwlIndyBugFix = class abstract
    class procedure CreateAttachment(const AMsg: TIdMessage; const AHeaders: TStrings; var AAttachment: TIdAttachment);
  end;

implementation

uses
  System.RegularExpressions, DWL.HTTP.Client, DWL.HTTP.Consts,
  System.NetEncoding, Winapi.WinInet, DWL.MediaTypes, IdAttachmentFile,
  IdGlobal, DWL.IOUtils, DWL.Types;

type
  TIdAttachmentFile_BugFix = class(TIdAttachmentFile)
  public
    function PrepareTempStream: TStream; override;
  end;

{ TdwlMailUtils }

class procedure TdwlMailUtils.FilldIdMessageFromString(Msg: TIdMessage; const Value: string);
begin
  var Stream := TStringStream.Create(Value);
  try
    Msg.LoadFromStream(Stream);
    Msg.ProcessHeaders;
  finally
    Stream.Free;
  end;
end;

class function TdwlMailUtils.IdMessageToBytes(Msg: TIdMessage): TBytes;
begin
  var Stream := TMemoryStream.Create;
  try
    Msg.SaveToStream(Stream);
    var Size := Stream.Size;
    SetLength(Result, Size);
    Stream.Seek(0, soBeginning);
    Stream.Read(Result, Size);
  finally
    Stream.Free;
  end;
end;

class function TdwlMailUtils.IdMessageToString(Msg: TIdMessage): string;
begin
  var Stream := TStringStream.Create;
  try
    Msg.SaveToStream(Stream);
    Stream.Seek(0, soBeginning);
    Result := Stream.ReadString(MaxInt);
  finally
    Stream.Free;
  end;
end;

class function TdwlMailUtils.IsValidEmailAddress(const Value: string; Options: TdwlMailCheckOptions=[]): boolean;
const
  EMAIL_REGEX = '^((?>[a-zA-Z\d!#$%&''*+\-/=?^_`{|}~]+\x20*|"((?=[\x01-\x7f])'
             +'[^"\\]|\\[\x01-\x7f])*"\x20*)*(?<angle><))?((?!\.)'
             +'(?>\.?[a-zA-Z\d!#$%&''*+\-/=?^_`{|}~]+)+|"((?=[\x01-\x7f])'
             +'[^"\\]|\\[\x01-\x7f])*")@(((?!-)[a-zA-Z\d\-]+(?<!-)\.)+[a-zA-Z]'
             +'{2,}|\[(((?(?<!\[)\.)(25[0-5]|2[0-4]\d|[01]?\d?\d))'
             +'{4}|[a-zA-Z\d\-]*[a-zA-Z\d]:((?=[\x01-\x7f])[^\\\[\]]|\\'
             +'[\x01-\x7f])+)\])(?(angle)>)$';
begin
  var EMail2Check := Value;
  if not (mcDoNotTrimSpaces in Options) then
    EMail2Check := EMail2Check.Trim;
  if (EMail2Check='') then
    Exit(mcEmptyStringIsValid in Options);
  if mcEvaluateCommaSeparatedList in Options then
  begin
    var NewOptions := Options - [mcEvaluateCommaSeparatedList];
    Result := true;
    repeat
      var P := pos(',', EMail2Check);
      if P>0 then
        Result := Result and TdwlMailUtils.IsValidEmailAddress(EMail2Check.Substring(0, P-1), NewOptions)
      else
      begin
        Result := Result and TdwlMailUtils.IsValidEmailAddress(EMail2Check, NewOptions);
        Break;
      end;
      EMail2Check := EMail2Check.Substring(P);
    until false;
  end
  else
    Result := TRegEx.IsMatch(EMail2Check, EMAIL_REGEX);
end;

class function TdwlMailUtils.SendMailToAPI(const Endpoint, LogSecret: string; Msg: TIdMessage): TdwlResult;
begin
  var Url := Endpoint+'?secret='+TNetEncoding.URL.Encode(LogSecret);
  if Msg.BccList.Count>0 then
    Url := Url+'&bcc='+TNetEncoding.URL.Encode(Msg.BccList.EMailAddresses);
  var Request := New_HTTPRequest(Url);
  var Stream := TMemoryStream.Create;
  try
    Msg.SaveToStream(Stream);
    Request.PostStream.WriteData(Stream.Memory, Stream.Size);
  finally
    Stream.Free;
  end;
  Request.Header[HTTP_FIELD_CONTENT_TYPE] := MEDIA_TYPE_OCTET_STREAM;
  Request.Method  := HTTP_METHOD_POST;
  var Response := Request.Execute;
  if Response.StatusCode<>HTTP_STATUS_OK then
    Result.AddErrorMsg('Error '+Response.StatusCode.ToString+': '+Response.ErrorMsg);
end;

{ TdwlIndyBugFix }

class procedure TdwlIndyBugFix.CreateAttachment(const AMsg: TIdMessage; const AHeaders: TStrings; var AAttachment: TIdAttachment);
begin
  AAttachment := TIdAttachmentFile_BugFix.Create(AMsg.MessageParts);
end;

{ TIdAttachmentFile_BugFix }

function TIdAttachmentFile_BugFix.PrepareTempStream: TStream;
begin
  FStoredPathName := TdwlDirectory.Application_TempDir+'\Indy_'+TdwlUUID.CreateNew.AsString+'.tmp';
  FTempFileStream := TIdFileCreateStream.Create(FStoredPathName);
  FFileIsTempFile := True;
  Result := FTempFileStream;
end;

end.
