unit DWL.StrUtils;

interface

uses
  System.SysUtils;

type
  TdwlSanatizeOption = (soRemoveInvalidFileNameChars, soRemoveInvalidPathChars, soRemoveLineBreaks, soOnlyAZ);
  TdwlSanatizeOptions = set of TdwlSanatizeOption;

  TdwlStrUtils = record
    class function CharInArray(const C: Char; const InArray: TCharArray): boolean; static;
    class function RemoveChars(const Value: string; CharsToRemove: TCharArray): string; overload; static;
    class function RemoveChars(const Value: string; CharsToRemove: TSysCharSet): string; overload; static;
    class function FilterChars(const Value: string; CharsToAllow: TCharArray): string; overload; static;
    class function FilterChars(const Value: string; CharsToAllow: TSysCharSet): string; overload; static;
    class function Sanatize(const Value: string; Options: TdwlSanatizeOptions): string; static;
    class function TrimLeft(const Value, TrimStr: string; IgnoreCase: boolean=false): string; static;
    class function TrimRight(const Value, TrimStr: string; IgnoreCase: boolean=false): string; static;
  end;

implementation

uses
  System.IOUtils;

{ TdwlStrUtils }

class function TdwlStrUtils.CharInArray(const C: Char; const InArray: TCharArray): boolean;
begin
  for var AChar in InArray do
    if AChar = C then
      Exit(True);
  Result := False;
end;

class function TdwlStrUtils.FilterChars(const Value: string; CharsToAllow: TCharArray): string;
begin
  Result := Value;
  var i := 1;
  while i<=length(Result) do
  begin
    if not CharInArray(Result[i], CharsToAllow) then
      Result := Result.SubString(0, i-1)+Result.Substring(i)
    else
      inc(i);
  end;
end;

class function TdwlStrUtils.FilterChars(const Value: string; CharsToAllow: TSysCharSet): string;
begin
  Result := Value;
  var i := 1;
  while i<=length(Result) do
  begin
    if not CharInSet(Result[i], CharsToAllow) then
      Result := Result.SubString(0, i-1)+Result.Substring(i)
    else
      inc(i);
  end;
end;

class function TdwlStrUtils.RemoveChars(const Value: string; CharsToRemove: TSysCharSet): string;
begin
  Result := Value;
  var i := 1;
  while i<=length(Result) do
  begin
    if CharInSet(Result[i], CharsToRemove) then
      Result := Result.SubString(0, i-1)+Result.Substring(i)
    else
      inc(i);
  end;
end;

class function TdwlStrUtils.RemoveChars(const Value: string; CharsToRemove: TCharArray): string;
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
  if (soOnlyAZ in Options) then
    Result := FilterChars(Result, ['A'..'Z','a'..'z']);
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

class function TdwlStrUtils.TrimRight(const Value, TrimStr: string; IgnoreCase: boolean): string;
begin
  Result := Value;
  while Result.EndsWith(TrimStr, IgnoreCase) do
    Result := Result.Substring(0, Length(Result)-Length(TrimStr));
end;

end.
