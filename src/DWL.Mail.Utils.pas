unit DWL.Mail.Utils;

interface

uses
  IdMessage;

type
  TdwlMailCheckOption=(mcEmptyStringIsValid, mcDoNotTrimSpaces, mcEvaluateCommaSeparatedList);
  TdwlMailCheckOptions=set of TdwlMailCheckOption;

  TdwlMailUtils = record
    class function IsValidEmailAddress(const Value: string; Options: TdwlMailCheckOptions=[]): boolean; static;
    class procedure SendMailToAPI(const Endpoint, LogSecret: string; Msg: TIdMessage); static;
  end;

implementation

uses
  System.RegularExpressions, System.Classes, DWL.HTTP.Client, DWL.HTTP.Consts,
  System.NetEncoding, System.SysUtils;

{ TdwlMailUtils }

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

class procedure TdwlMailUtils.SendMailToAPI(const Endpoint, LogSecret: string; Msg: TIdMessage);
begin
  var Url := Endpoint+'?secret='+TNetEncoding.URL.Encode(LogSecret);
  if Msg.BccList.Count>0 then
    Url := Url+'&bcc='+TNetEncoding.URL.Encode(Msg.BccList.EMailAddresses);
  var Rq := New_HTTPRequest(Url);
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
