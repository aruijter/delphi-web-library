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
  Result := TypeInfo=CompareValue.TypeInfo;
  if not Result then
    Exit;
  // of course have the same datasize
  var DtSize := DataSize;
  Result := DtSize=CompareValue.DataSize;
  if not Result then
    Exit;
  case TypeInfo.Kind of
    tkInteger, tkEnumeration, tkChar, tkWChar, tkFloat, tkInt64,tkRecord, tkMRecord, tkArray, tkVariant:
        Result := (DtSize>0) and CompareMem(GetReferenceToRawData, CompareValue.GetReferenceToRawData, DtSize);
    tkString: Result := PShortString(GetReferenceToRawData)^=PShortString(CompareValue.GetReferenceToRawData)^;
    tkWString: Result := PWideString(GetReferenceToRawData)^=PWideString(CompareValue.GetReferenceToRawData)^;
    tkLString: Result := PRawByteString(GetReferenceToRawData)^=PRawByteString(CompareValue.GetReferenceToRawData)^;
    tkUString: Result := PUnicodeString(GetReferenceToRawData)^=PUnicodeString(CompareValue.GetReferenceToRawData)^;
    tkSet, tkClass, tkClassRef, tkMethod, tkInterface, tkPointer, tkDynArray: Result := false;
  end;
end;

end.
