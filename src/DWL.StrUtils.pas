unit DWL.StrUtils;

interface

type
  TdwlStrUtils = record
    class function TrimStrLeft(const Value, TrimStr: string; IgnoreCase: boolean=false): string; static;
  end;

implementation

uses
  System.SysUtils;

{ TdwlStrUtils }

class function TdwlStrUtils.TrimStrLeft(const Value, TrimStr: string; IgnoreCase: boolean=false): string;
begin
  Result := Value;
  while Result.StartsWith(TrimStr, IgnoreCase) do
    Result := Result.Substring(Length(TrimStr));
end;

end.
