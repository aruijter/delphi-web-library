unit DWL.HTTP.Utils;

interface

type
  TdwlHTTPUtils = record
    class function StringTodwlhttpCommand(const HttpCommand: string): byte; static;
  end;

implementation

uses
  DWL.HTTP.Consts, System.StrUtils;

{ TdwlHTTPUtils }

class function TdwlHTTPUtils.StringTodwlhttpCommand(const HttpCommand: string): byte;
begin
  Result := IndexStr(HttpCommand, dwlhttpCommandToString);
end;

end.
