unit DWL.Mail.Utils;

interface

uses
  IdMessage;

type
  TdwlMailUtils = record
    class function IsValidEmailAddress(const Value: string): boolean; static;
    class procedure SendMailToAPI(const Endpoint: string; Msg: TIdMessage); static;
  end;

implementation

uses
  System.RegularExpressions, System.Classes, DWL.HTTP.Client, DWL.HTTP.Consts;

{ TdwlMailUtils }

class function TdwlMailUtils.IsValidEmailAddress(const Value: string): boolean;
const
  EMAIL_REGEX = '^((?>[a-zA-Z\d!#$%&''*+\-/=?^_`{|}~]+\x20*|"((?=[\x01-\x7f])'
             +'[^"\\]|\\[\x01-\x7f])*"\x20*)*(?<angle><))?((?!\.)'
             +'(?>\.?[a-zA-Z\d!#$%&''*+\-/=?^_`{|}~]+)+|"((?=[\x01-\x7f])'
             +'[^"\\]|\\[\x01-\x7f])*")@(((?!-)[a-zA-Z\d\-]+(?<!-)\.)+[a-zA-Z]'
             +'{2,}|\[(((?(?<!\[)\.)(25[0-5]|2[0-4]\d|[01]?\d?\d))'
             +'{4}|[a-zA-Z\d\-]*[a-zA-Z\d]:((?=[\x01-\x7f])[^\\\[\]]|\\'
             +'[\x01-\x7f])+)\])(?(angle)>)$';
begin
  Result := TRegEx.IsMatch(Value, EMAIL_REGEX);
end;

class procedure TdwlMailUtils.SendMailToAPI(const Endpoint: string; Msg: TIdMessage);
begin
  var Rq := New_HTTPRequest(Endpoint);
  var Stream := TMemoryStream.Create;
  try
    Msg.SaveToStream(Stream);
    Rq.PostStream.WriteData(Stream.Memory, Stream.Size);
  finally
    Stream.Free;
  end;
  Rq.Header[HTTP_HEADER_CONTENT_TYPE] := CONTENT_TYPE_OCTET_STREAM;
  Rq.Method  := HTTP_COMMAND_POST;
  Rq.Execute;
end;

end.
