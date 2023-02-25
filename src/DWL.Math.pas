unit DWL.Math;

interface

type
  TdwlMathUtils = record
    class function FuzzyEqual(Value1, Value2: double; Fuzziness: double=0): boolean; static;
  end;

implementation

uses
  System.Math;

{ TdwlMath }

class function TdwlMathUtils.FuzzyEqual(Value1, Value2, Fuzziness: double): boolean;
// be warned, you can use SameValue if you're sure about which overload is used
// before you know a division of doubles becomes extended and is fed
// into the samevalue extended overload with of course a different epsilon
begin
  if Fuzziness=0 then
    Fuzziness := Max(1, Min(abs(Value1), abs(Value2)))/1000000;
  if Value1>Value2 then
    Result := (Value1-Value2)<=Fuzziness
  else
    Result := (Value2-Value1)<=Fuzziness;
end;

end.
