unit DWL.StrUtils;

interface

type
  TdwlSanatizeOption = (soRemoveInvalidFileNameChars, soRemoveInvalidPathChars, soRemoveLineBreaks);
  TdwlSanatizeOptions = set of TdwlSanatizeOption;

  TdwlStrUtils = record
    class function RemoveChars(const Value: string; CharsToRemove: array of char): string; static;
    class function Sanatize(const Value: string; Options: TdwlSanatizeOptions): string; static;
    class function TrimLeft(const Value, TrimStr: string; IgnoreCase: boolean=false): string; static;
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

{ TdwlStrUtils }

class function TdwlStrUtils.RemoveChars(const Value: string; CharsToRemove: array of char): string;
  function CharInArray(const C: Char; const InArray: array of Char): Boolean;
  begin
    for var AChar in InArray do
      if AChar = C then
        Exit(True);
    Result := False;
  end;
begin
  Result := Value;
  var i := 1;
  while i<=length(Result) do
  begin
    if CharInArray(Result[i], CharsToRemove) then
      Result := Result.SubString(0, i-1)+Result.Substring(i)
    else
      inc(i);
  end;
end;

class function TdwlStrUtils.Sanatize(const Value: string; Options: TdwlSanatizeOptions): string;
begin
  Result := Value;
  if (soRemoveInvalidFileNameChars in Options) then
    Result := RemoveChars(Result, TPath.GetInvalidFileNameChars);
  if (soRemoveInvalidPathChars in Options) then
    Result := RemoveChars(Result, TPath.GetInvalidPathChars);
  if (soRemoveLineBreaks in Options) then
    Result := RemoveChars(Result, [#10,#13]);
end;

class function TdwlStrUtils.TrimLeft(const Value, TrimStr: string; IgnoreCase: boolean=false): string;
begin
  Result := Value;
  while Result.StartsWith(TrimStr, IgnoreCase) do
    Result := Result.Substring(Length(TrimStr));
end;

end.
