unit DWL.Rtti.Utils;

interface

uses
  System.Rtti;

type
  TdwlTValueHelper= record helper for TValue
    function Equals(CompareValue: TValue): boolean;
  end;

implementation

uses
  System.SysUtils, System.TypInfo;

{ TdwlTValueHelper }

function TdwlTValueHelper.Equals(CompareValue: TValue): boolean;
begin
  // We take for granted than TTypeInfo instances are unqiue, so we simply can compare pointers
  Result := Self.TypeInfo=CompareValue.TypeInfo;
  if not Result then
    Exit;
  // of course have the same size
  var Size := Self.DataSize;
  Result := Size=CompareValue.DataSize;
  if not Result then
    Exit;
  // and having the same raw data
  Result := CompareMem(Self.GetReferenceToRawData, CompareValue.GetReferenceToRawData, Size);
end;

end.
