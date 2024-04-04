unit DWL.Params.Persisted;

interface

uses
  DWL.Params, System.TypInfo;

/// <summary>
///   Will generate an interface to a persisted store of key/value pairs
/// </summary>
/// <param name="FileName">
///    An obligated Filename is needed for the file where the params are persistently stored
/// </param>
/// <param name="Domain">
///   An optional domain that will be used to work with the metadata added for
///   this particular domain
/// </param>
function New_PersistedParams(const FileName: string; const Domain: string=''): IdwlParams;
procedure Register_PersistedParamsClass(AClass: TClass; AIntfInfo: PTypeInfo);
procedure Register_PersistedParamsRecord(RecordName: string; ATypeInfo: PTypeInfo);

implementation

uses
  Winapi.Windows, System.Rtti, System.Generics.Collections, DWL.IO,
  System.Classes, System.SysUtils, System.IOUtils, Vcl.Dialogs, DWL.Types;

const
  MAGIC_WORD = 61374;
  MAGIC_CARDINAL = 3992898270;
  CURRENT_FILE_VERSION_BYTE = 1;
  OFFSET_FILESIZE = 8;
  OFFSET_EPOCH_DELETED_FROM_PARAMETERKIND = 10;
  METHOD_SERIALIZETOCURSOR = 'SerializeToCursor';
  METHOD_SERIALIZETOCURSOR_FULL = 'procedure SerializeToCursor(Cursor: IdwlCursor_Write)';
  METHOD_DESERIALIZEFROMCURSOR = 'DeSerializeFromCursor';
  METHOD_DESERIALIZEFROMCURSOR_FULL = 'procedure DeSerializeFromCursor(Cursor: IdwlCursor_Write)';
  // Param kinds in file
  pkString = 0;
  pkUInt8 = 1;
  pkUInt16 = 2;
  pkUInt32 = 3;
  pkUInt64 = 4;
  pkInt8 = 5;
  pkInt16 = 6;
  pkInt32 = 7;
  pkInt64 = 8;
  pkSingle = 9;
  pkDouble = 10;
  pkExtended = 11;
  pkBoolean = 12;
  pkEnumeration = 13;
  pkDynArraySimpleType = 14;
  pkRecord = 15;
  pkInterface = 16;


// File Layout
//   0- 3: uint32  MAGIC_CARDINAL
//   4- 7: uint32  CURRENT_FILE_VERSION_BYTE_AND_3_RESERVED_BYTES
//   8-15: uint64 File size
//  Followed by Paramaters blocks

// Parameter Block layout:
//   0- 1: uint16 MAGIC_WORD
//   2- 5: uint32 Total Block size
//   6- 6: uint8  Parameter Kind
//   7- 7: uint8  Reserved
//   8-15:  int64 Epoch Created
//  16-23:  int64 Epoch Deleted
//  24-24: uint8  Bytes used to store paramter Name
//  25-  : Parameter Name
//  followed by the Parameter data

type
  TdwlPersistedParamType = record
    ParamKind: byte;
    SimpleDataSize: byte;
    Info: PTypeInfo;
  end;


  PdwlPersistedParam = ^TdwlPersistedParam;
  TdwlPersistedParam = record
    ParamKind: UInt8;
    ParamKindOffset: UInt64;
    DataOffSet: UInt64;
  end;

  TdwlSerializeClass = record
    RttiType: TRttiType;
    MetaClassType: TClass;
    DeSerializeMethod: TRttiMethod;
    SerializeMethod: TRttiMethod;
    IntfInfo: PTypeInfo;
  end;

  TdwlSerializeRecord = record
    Info: PTypeInfo;
  end;

  TdwlPersistedParams_Hook = class(TInterfacedObject, IdwlParamsPersistHook)
  strict private
    class var
      FSerializeClasses: TDictionary<string, TdwlSerializeClass>;
      FSerializeRecords: TDictionary<string, PTypeInfo>;
      FTypeInfo2ParamType: TDictionary<PTypeInfo, TdwlPersistedParamType>;
      FParamTypes: TList<TdwlPersistedParamType>;
    var
      FProvideProc: TProvideKeyCallBack;
    procedure CheckInitReady;
    procedure Flush;
    function TryGetValueFromFile(Param: PdwlPersistedParam; var Value: TValue): boolean;
  private
    class var
      FClassInitNeeded: boolean;
      FCtx: TRttiContext;
    var
      // to be able to get full change tracking, don't optimize, overwriting breaks history
      FOverwriteChangedParams: boolean;
      FInitNotReady: boolean;
      FInitEvent: THandle;
      FFileName: string;
      FPersistedParams: TDictionary<string, TdwlPersistedParam>; // Dictionary that holds absolute offset from beginning of file
      FFileOptions: TdwlFileOptions;
      FCursor: IdwlFileCursor_Write;
    class procedure InitParamKinds;
//    class procedure InitSerializeClasses;
    class procedure AddSerializeClass(RttiType: TRttiType; AClass: TClass; AIntfInfo: PTypeInfo);
    class procedure AddSerializeRecord(const RecordName: string; Info: PTypeInfo);
    procedure AddOrSetValue(const LowerKey: string; const Value: TValue);
    procedure Clear;
    procedure ClearKey(const LowerKey: string);
    function ContainsKey(const LowerKey: string): boolean;
    procedure Initialize(ProvideProc: TProvideKeyCallBack);
    procedure ProvideUnknownKeys(const KnownKeys: TArray<string>);
    function TryGetValue(const LowerKey: string; var Value: TValue): boolean;
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create(const FileName: string);
    destructor Destroy; override;
  end;

type
  TInitThread = class(TThread)
  strict private
    FHook: IdwlParamsPersistHook;
  protected
    procedure Execute; override;
  public
    constructor Create(Hook: IdwlParamsPersistHook);
  end;

function New_PersistedParams(const FileName: string; const Domain: string=''): IdwlParams;
begin
  Result := New_Params(Domain);
  Result.RegisterPersistHook(TdwlPersistedParams_Hook.Create(Filename));
end;

procedure Register_PersistedParamsClass(AClass: TClass; AIntfInfo:PTypeInfo);
begin
  TdwlPersistedParams_Hook.AddSerializeClass(nil, AClass, AIntfInfo);
end;

procedure Register_PersistedParamsRecord(RecordName: string; ATypeInfo: PTypeInfo);
begin
  TdwlPersistedParams_Hook.AddSerializeRecord(RecordName, ATypeInfo);
end;

{ TInitThread }

constructor TInitThread.Create(Hook: IdwlParamsPersistHook);
begin
  FHook := Hook;
  FreeOnTerminate := true;
  inherited Create;
end;

procedure TInitThread.Execute;
begin
  var Hook := FHook as TdwlPersistedParams_Hook; // to work with internal values
  try
    if Hook.FClassInitNeeded then
    begin
      Hook.FClassInitNeeded := false;
      Hook.InitParamKinds;
//      Hook.InitSerializeClasses;
    end;
    var Cursor := New_File(Hook.FFilename, Hook.FFileOptions).GetWriteCursor;
    Hook.FCursor := Cursor;
    if Cursor.FileSize=0 then
    begin
      Cursor.WriteUInt32(MAGIC_CARDINAL);
      Cursor.WriteUInt32(CURRENT_FILE_VERSION_BYTE); // including 3 reserved bytes
      Cursor.WriteUInt64(16); // empty file size
    end
    else
    begin
      if Cursor.ReadUInt32<>MAGIC_CARDINAL then
        raise Exception.Create('PersistedParams: Invalid Magic Word: wrong filetype?');
      if Cursor.ReadUInt32<>CURRENT_FILE_VERSION_BYTE {3 reserverd bytes should be zero, so read in one go} then
        raise Exception.Create('PersistedParams: Unknown File Verion!!');
      var StoredFileSize := Cursor.ReadUInt64;
      // check the filesize and correct if needed
      if StoredFileSize<>Cursor.FileSize then
        Cursor.SetFileSize(StoredFileSize);
      var NextOffSet := Cursor.CursorOffset;
      while not Cursor.Eof do
      begin
        if Cursor.ReadUInt16<>MAGIC_WORD then
          raise Exception.Create('PersistedParams: Invalid Entry: wrong filetype?');
        // add total blocksize to offset to get pointer to next parameter
        inc(NextOffset, Cursor.ReadUInt32);
        var Param: TdwlPersistedParam;
        // keep offset to paramkind for fast access
        Param.ParamKindOffset := Cursor.CursorOffset;
        Param.ParamKind := Cursor.ReadUInt8;
        Cursor.ReadUInt8; //reserved
        Cursor.ReadInt64; // skip Epoch Created
        var EpochDeleted: TUnixEpoch := Cursor.ReadInt64;
        if EpochDeleted=0 then // not deleted
        begin
          var Name := Cursor.ReadString_LenByte;
          Param.DataOffSet := Cursor.CursorOffset;
          Hook.FPersistedParams.Add(Name, Param);
        end;
        Cursor.Seek(NextOffSet, soBeginning); // next one
      end;
    end;
  finally
    Hook.FInitNotReady := false;
    SetEvent(Hook.FInitEvent);
  end;
end;

{ TdwlPersistedParams_Hook }

procedure TdwlPersistedParams_Hook.AddOrSetValue(const LowerKey: string; const Value: TValue);
begin
  CheckInitReady;
  var ParamType: TdwlPersistedParamType;
  if not FTypeInfo2ParamType.TryGetValue(Value.TypeInfo, ParamType) then
  begin
    ParamType.SimpleDataSize := 0;
    case Value.TypeInfo.Kind of
    tkEnumeration:
      begin
        if Value.TypeInfo=TypeInfo(Boolean) then
          ParamType.ParamKind := pkBoolean
        else
          ParamType.ParamKind := pkEnumeration;
      end;
    tkDynArray:
      begin
        var ElementParamType: TdwlPersistedParamType;
        if (not FTypeInfo2ParamType.TryGetValue(Value.TypeInfo.TypeData.DynArrElType^, ElementParamType)) or
          (ElementParamType.SimpleDataSize=0) then
          raise Exception.Create('currently only simple dynarray elements allowed: '+string(Value.TypeInfo.Name));
        ParamType.ParamKind := pkDynArraySimpleType;
      end;
    tkRecord: ParamType.ParamKind := pkRecord;
    tkInterface: ParamType.ParamKind := pkInterface;
    else
      raise Exception.Create('PersistedParams: Missing type '+string(Value.TypeInfo.Name));
    end;
  end;
  // check if value already exists
  var OldParam: TdwlPersistedParam;
  if FPersistedParams.TryGetValue(LowerKey, OldParam) then
  begin
    if FOverwriteChangedParams and (OldParam.ParamKind=ParamType.ParamKind) and (ParamType.SimpleDataSize>0) then
    begin
      FCursor.Seek(OldParam.DataOffset, soBeginning);
      var ValueRawData := Value.GetReferenceToRawData;
      FCursor.Write(ValueRawData^, ParamType.SimpleDataSize);
      Exit; // that's it for a simpletype overwrite: finished
    end;
    // otherwise no reuse, delete current value
    FCursor.Seek(OldParam.ParamKindOffset+OFFSET_EPOCH_DELETED_FROM_PARAMETERKIND, soBeginning);
    FCursor.WriteInt64(TUnixEpoch.Now); // assign oldvalue as deleted
    FPersistedParams.Remove(LowerKey);
  end;
  var Param: TdwlPersistedParam;
  var DataSizeOffset: UInt64 := 0;;
  var FallBackFileSize := FCursor.FileSize;
  try
    FCursor.Seek(0, soEnd);
    FCursor.WriteUInt16(MAGIC_WORD);
    DataSizeOffset := FCursor.CursorOffset;
    FCursor.WriteUInt32(0); // datasize to be written when flushing
    Param.ParamKindOffset := FCursor.CursorOffset;
    Param.ParamKind := ParamType.ParamKind;
    FCursor.WriteUInt8(Param.ParamKind);
    FCursor.WriteUInt8(0); // reserved
    FCursor.WriteInt64(TUnixEpoch.Now); // write epoch created
    FCursor.WriteInt64(0); // write epoch deleted=0 (not deleted)
    FCursor.WriteString_LenByte(LowerKey);
    Param.DataOffSet := FCursor.CursorOffset;
    // Header is ready, now write data
    // write value to file
    if ParamType.SimpleDataSize>0 then
    begin
      var ValueData := Value.GetReferenceToRawData;
      FCursor.Write(ValueData^, ParamType.SimpleDataSize);
    end
    else
    begin
      case ParamType.ParamKind of
      pkString: FCursor.WriteString_LenCardinal(Value.AsString);
      pkBoolean:
        begin
          var ValueData := Value.GetReferenceToRawData;
          FCursor.Write(ValueData^, 1);
        end;
      pkEnumeration:
        begin
          FCursor.WriteString_LenByte(string(Value.TypeInfo.Name));
          var UnderlyingType := Value.TypeInfo.TypeData.BaseType^;
          var ValueData := Value.GetReferenceToRawData;
          FCursor.Write(ValueData^, UnderlyingType.TypeData.elSize);
        end;
      pkDynArraySimpleType:
        begin
          FCursor.WriteString_LenByte(string(Value.TypeInfo.TypeData.DynArrElType^.Name));
          var ByteCount: UInt32 := Value.GetArrayLength*Value.TypeInfo.TypeData.elSize;
          FCursor.WriteUInt32(ByteCount);
          var ValueData := Value.GetReferenceToRawArrayElement(0);
          FCursor.Write(ValueData^, ByteCount);
        end;
      pkRecord:
        begin
          var RecordName := string(Value.TypeInfo.Name);
          var RecordNameByteCount := WideCharToMultiByte(CP_UTF8, 0, PWideChar(RecordName), Length(RecordName), nil, 0, nil, nil);
          var RecordSize := Value.TypeInfo.TypeData.RecSize;
          FCursor.WriteString_LenByte(RecordName, RecordNameByteCount);
          var ValueData := Value.GetReferenceToRawData;
          FCursor.Write(ValueData^, RecordSize);
        end;
      pkInterface:
        begin
          var Intf := Value.AsInterface;
          var Obj := Intf as TObject;
          var ClassDef: TdwlSerializeClass;
          if not FSerializeClasses.TryGetValue(Obj.ClassName, ClassDef) then
            raise Exception.Create('PersistedParams: No Serialize Class found for '+Obj.ClassName);
          var IntfName := '';
          // for now interface name is always known, maybe later we need to guess again
//          if ClassDef.IntfInfo<>nil then
            IntfName := string(ClassDef.IntfInfo.Name)
         { else
          begin
            for var IntfType in ClassDef.RttiType.AsInstance.GetImplementedInterfaces do
            begin
              var tmpIntf: IInterface;
              if Obj.GetInterface(IntfType.GUID, tmpIntf) then
              begin
                if Intf = tmpIntf then
                begin
                  IntfName := IntfType.Name;
                  Break;
                end;
              end;
            end;
            if IntfName='' then
              raise Exception.Create('PersistedParams: problems finding interface for '+Obj.ClassName);
          end};
          FCursor.WriteString_LenByte(Obj.ClassName);
          FCursor.WriteString_LenByte(IntfName);
          ClassDef.SerializeMethod.Invoke(Obj, [TValue.From(FCursor)]);
        end;
      end;
    end;
  except
    FCursor.SetFileSize(FallBackFileSize);
  end;
  var DataSize := FCursor.CursorOffset-DataSizeOffSet+SizeOf(UInt16); {magic word}
  FCursor.Seek(DataSizeOffset, soBeginning);
  FCursor.WriteUInt32(DataSize); // Final DataSize
  Flush;
  FPersistedParams.Add(LowerKey, Param);
end;

procedure TdwlPersistedParams_Hook.Initialize(ProvideProc: TProvideKeyCallBack);
begin
  FProvideProc := ProvideProc;
  TInitThread.Create(Self);
end;

// for now don't initialize interfaces by RTTI, maybe later
//class procedure TdwlPersistedParams_Hook.InitSerializeClasses;
//begin
//  var TheClasses := FCtx.GetTypes;
//  for var RttiType in TheClasses do
//    if RttiType.IsInstance then
//      AddSerializeClass(RttiType, RttiType.AsInstance.MetaclassType);
//end;

class procedure TdwlPersistedParams_Hook.InitParamKinds;
  procedure AddType(ParamKind: byte; SimpleDataSize: byte; TypeInfo: PTypeInfo);
  begin
    var ParameterType: TdwlPersistedParamType;
    Assert(byte(ParamKind)=FParamTypes.Count);
    ParameterType.ParamKind := ParamKind;
    ParameterType.SimpleDataSize := SimpleDataSize;
    ParameterType.Info := TypeInfo;
    FParamTypes.Add(ParameterType);
    if TypeInfo<>nil then
      FTypeInfo2ParamType.Add(TypeInfo, ParameterType);
  end;
begin
  AddType(pkString, 0, TypeInfo(string));
  AddType(pkUInt8, SizeOf(UInt8), TypeInfo(UInt8));
  AddType(pkUInt16, SizeOf(UInt16), TypeInfo(UInt16));
  AddType(pkUInt32, SizeOf(UInt32), TypeInfo(UInt32));
  AddType(pkUInt64, SizeOf(UInt64), TypeInfo(UInt64));
  AddType(pkInt8, SizeOf(Int8), TypeInfo(Int8));
  AddType(pkInt16, SizeOf(Int16), TypeInfo(Int16));
  AddType(pkInt32, SizeOf(Int32), TypeInfo(Int32));
  AddType(pkInt64, SizeOf(Int64), TypeInfo(Int64));
  AddType(pkSingle, Sizeof(single), TypeInfo(single));
  AddType(pkDouble, SizeOf(double), TypeInfo(double));
  AddType(pkExtended, SizeOf(extended), TypeInfo(extended));
  AddType(pkBoolean, 0, nil);
  AddType(pkEnumeration, 0, nil);
  AddType(pkDynArraySimpleType, 0, nil);
  AddType(pkRecord, 0, nil);
  AddType(pkInterface, 0, nil);
end;

class procedure TdwlPersistedParams_Hook.AddSerializeClass(RttiType: TRttiType; AClass: TClass; AIntfInfo: PTypeInfo);
begin
  if RttiType=nil then
    RttiType := TdwlPersistedParams_Hook.FCtx.GetType(AClass);
  if RttiType=nil then
    Exit;
  var SerializeMethod := RttiType.GetMethod(METHOD_SERIALIZETOCURSOR);
  if SerializeMethod=nil then
    Exit;
  if not SameText(SerializeMethod.ToString, METHOD_SERIALIZETOCURSOR_FULL) then
    Exit;
  var DeSerializeMethod := RttiType.GetMethod(METHOD_DESERIALIZEFROMCURSOR);
  if DeSerializeMethod=nil then
    Exit;
  if not SameText(DeSerializeMethod.ToString, METHOD_DESERIALIZEFROMCURSOR_FULL) then
    Exit;
  var SerializeClass: TdwlSerializeClass;
  SerializeClass.RttiType := RttiType;
  SerializeClass.DeSerializeMethod := DeSerializeMethod;
  SerializeClass.SerializeMethod := SerializeMethod;
  SerializeClass.MetaClassType := RttiType.AsInstance.MetaclassType;
  SerializeClass.IntfInfo := AIntfInfo;
  FSerializeClasses.Add(RttiType.Name, SerializeClass);
end;

class procedure TdwlPersistedParams_Hook.AddSerializeRecord(const RecordName: string; Info: PTypeInfo);
begin
   FSerializeRecords.Add(RecordName, Info);
end;

procedure TdwlPersistedParams_Hook.CheckInitReady;
begin
  if FInitNotReady then
    WaitForSingleObject(FInitEvent, INFINITE);
end;

procedure TdwlPersistedParams_Hook.Clear;
begin
  CheckInitReady;
  FPersistedParams.Clear;
  FCursor := nil;
  FFileOptions := [pfoCreateEmptyNew];
  FInitNotReady := true;
  ResetEvent(FInitEvent);
  TInitThread.Create(Self);
end;

procedure TdwlPersistedParams_Hook.ClearKey(const LowerKey: string);
begin
  CheckInitReady;
  var Param: TdwlPersistedParam;
  if FPersistedParams.TryGetValue(LowerKey, Param) then
  begin
    FCursor.Seek(Param.ParamKindOffset+OFFSET_EPOCH_DELETED_FROM_PARAMETERKIND, soBeginning);
    FCursor.WriteInt64(TUnixEpoch.Now); // assign oldvalue as deleted
    FPersistedParams.Remove(LowerKey);
  end;
end;

function TdwlPersistedParams_Hook.ContainsKey(const LowerKey: string): boolean;
begin
  Result := FPersistedParams.ContainsKey(LowerKey);
end;

class constructor TdwlPersistedParams_Hook.Create;
begin
  inherited;
  FTypeInfo2ParamType := TDictionary<PTypeInfo, TdwlPersistedParamType>.Create;
  FParamTypes := TList<TdwlPersistedParamType>.Create;
  FSerializeClasses := TDictionary<string, TdwlSerializeClass>.Create;
  FSerializeRecords := TDictionary<string, PTypeInfo>.Create;
  FClassInitNeeded := true;
end;

constructor TdwlPersistedParams_Hook.Create(const FileName: string);
begin
  inherited Create;
  FPersistedParams := TDictionary<string, TdwlPersistedParam>.Create;
  FFileName := FileName;
  FInitEvent := CreateEvent(nil, false, false, nil);
  FInitNotReady := true;
  FFileOptions := [pfoCreateIfNeeded];
  FOverwriteChangedParams := true; // default optimized
end;

class destructor TdwlPersistedParams_Hook.Destroy;
begin
  FTypeInfo2ParamType.Free;
  FParamTypes.Free;
  FSerializeClasses.Free;
  FSerializeRecords.Free;
  inherited;
end;

destructor TdwlPersistedParams_Hook.Destroy;
begin
  // in case of early destroy: wait for initialisation to finish
  CheckInitReady;
  // close cursor to release file early
  FCursor := nil;
  CloseHandle(FInitEvent);
  FPersistedParams.Free;
  inherited Destroy;
end;

procedure TdwlPersistedParams_Hook.Flush;
begin
  // write current filesize for quality purposes
  FCursor.Seek(OFFSET_FILESIZE, soBeginning);
  FCursor.WriteUInt64(FCursor.FileSize);
  // flush the memory mapped file
  FCursor.Flush;
end;

function TdwlPersistedParams_Hook.TryGetValueFromFile(Param: PdwlPersistedParam; var Value: TValue): boolean;
begin
  try
    var ParamType := FParamTypes[Param.ParamKind];
    FCursor.Seek(Param.DataOffSet, soBeginning);
    if ParamType.SimpleDataSize>0 then
      TValue.Make(FCursor.CursorPtr, ParamType.Info, Value)
    else
    begin
      case ParamType.ParamKind of
      pkString: Value := FCursor.ReadString_LenCardinal;
      pkBoolean:
        begin
          TValue.Make(FCursor.CursorPtr, TypeInfo(Boolean), Value);
        end;
      pkEnumeration:
        begin
          var TypeName := FCursor.ReadString_LenByte;
          var RttiInfo := FCtx.FindType(TypeName);
          if RttiInfo=nil then
            RttiInfo := FCtx.FindType('System.'+TypeName);
          if RttiInfo=nil then
            raise Exception.Create('PersistedParams: No TypeInfo found for '+TypeName);
          TValue.Make(FCursor.CursorPtr, RttiInfo.Handle, Value);
        end;
      pkDynArraySimpleType:
        begin
          var TypeName := FCursor.ReadString_LenByte;
          var RttiInfo := FCtx.FindType('System.TArray<System.'+TypeName+'>');
          if RttiInfo=nil then
          begin
            RttiInfo := FCtx.FindType('System.TArray<'+TypeName+'>');
            if RttiInfo=nil then
              raise Exception.Create('PersistedParams: No TypeInfo found for '+TypeName);
          end;
          var arr := nil;
          var ByteCount: NativeInt := FCursor.ReadUInt32;
          var Len: NativeInt := ByteCount div PTypeInfo(RttiInfo.Handle).TypeData.elSize;
          DynArraySetLength(arr, RttiInfo.Handle, 1, @Len);
          try
            Move(FCursor.CursorPtr^, arr^, ByteCount);
            TValue.Make(@arr, RttiInfo.Handle, Value); // makes copy of array
          finally
            DynArrayClear(arr, RttiInfo.Handle);
          end;
         end;
      pkRecord:
        begin
          var RecordName := FCursor.ReadString_LenByte;
          var Info: PTypeInfo;
          if not FSerializeRecords.TryGetValue(RecordName, Info) then
            raise Exception.Create('PersistedParams: No DeSerialize Info found for '+RecordName);
          TValue.Make(FCursor.CursorPtr, Info, Value);
        end;
      pkInterface:
        begin
          var ClassName := FCursor.ReadString_LenByte;
          var IntfName := FCursor.ReadString_LenByte;
          var ClassDef: TdwlSerializeClass;
          if not FSerializeClasses.TryGetValue(ClassName, ClassDef) then
            raise Exception.Create('PersistedParams: No DeSerialize Class found for '+ClassName);
//          var IntfDef: TRttiInterfaceType := nil;
//          for var IntfType in  ClassDef.RttiType.AsInstance.GetImplementedInterfaces do
//          begin
//            if IntfType.Name=IntfName then
//            begin
//              IntfDef := IntfType;
//              Break;
//            end;
//          end;
//          if IntfDef=nil then
//            raise Exception.Create('PersistedParams: No DeSerialize Interface found for '+IntfName);
          var Obj :=  ClassDef.MetaClassType.Create;
          try
            ClassDef.DeSerializeMethod.Invoke(Obj, [TValue.From(FCursor)]);
          except
            Exit(false);
          end;
          var IntfInst: IUnknown;
//          Obj.GetInterface(IntfDef.GUID, IntfInst);
//          TValue.Make(@IntfInst, IntfDef.Handle, Value);
          Obj.GetInterface(ClassDef.IntfInfo.TypeData.GUID, IntfInst);
          TValue.Make(@IntfInst, ClassDef.IntfInfo, Value);
        end;
      end;
    end;
    Result := true;
  except
    Result := false;
  end;
end;

procedure TdwlPersistedParams_Hook.ProvideUnknownKeys(const KnownKeys: TArray<string>);
begin
  CheckInitReady;
  var SkipKeys := TStringList.Create;
  try
    for var KnownKey in KnownKeys do
      SkipKeys.Add(KnownKey);
    SkipKeys.Sorted := true;
    var Enumerator := FPersistedParams.GetEnumerator;
    try
      while Enumerator.MoveNext do
      begin
        if SkipKeys.IndexOf(Enumerator.Current.Key)>=0 then
          Continue;
        var Value: TValue;
        var Param := Enumerator.Current.Value;
        if TryGetValueFromFile(@Param, Value) then
          FProvideProc(Enumerator.Current.Key, Value);
      end;
    finally
      Enumerator.Free;
    end;
  finally
    SkipKeys.Free;
  end;
end;

function TdwlPersistedParams_Hook.TryGetValue(const LowerKey: string; var Value: TValue): boolean;
begin
  CheckInitReady;
  var Param: TdwlPersistedParam;
  Result := FPersistedParams.TryGetValue(LowerKey, Param) and TryGetValueFromFile(@Param, Value);
end;

end.
