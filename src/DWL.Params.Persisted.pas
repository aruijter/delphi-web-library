unit DWL.Params.Persisted;

interface

uses
  DWL.Params;

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

implementation

uses
  Winapi.Windows, System.Rtti, System.TypInfo, System.Generics.Collections, DWL.IO,
  System.Classes, System.SysUtils;

const
  MAGIC_WORD = 61374;
  MAGIC_CARDINAL = 3992898270;
  METHOD_CREATE = 'Create';
  METHOD_SERIALIZETOCURSOR = 'SerializeToCursor';
  METHOD_DESERIALIZEFROMCURSOR = 'DeSerializeFromCursor';
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
  pkInterface = 12;
  pkDeleted = 255;

type
  TdwlPersistedParamType = record
    ParamKind: byte;
    SimpleDataSize: byte;
    TypeInfo: PTypeInfo;
  end;

  TdwlDeSerializeClass = record
    RttiType: TRttiType;
    MetaClassType: TClass;
    CreateMethod: TRttiMethod;
    DeSerializeMethod: TRttiMethod;
  end;

  TdwlPersistedParams_Hook = class(TInterfacedObject, IdwlParamsPersistHook)
  strict private
    class var
      FDeSerializeClasses: TDictionary<string, TdwlDeSerializeClass>;
      FTypeInfo2ParamKind: TDictionary<PTypeInfo, TdwlPersistedParamType>;
      FParamKinds: TList<TdwlPersistedParamType>;
      RttiContext: TRttiContext;
    var
      FProvideProc: TProvideKeyCallBack;
    procedure CheckInitReady;
    procedure Flush;
    function TryGetValueFromOffset(Offset: UInt64; var Value: TValue): boolean;
  private
    class var
      FClassInitNeeded: boolean;
      FCtx: TRttiContext;
    var
      FInitNotReady: boolean;
      FInitEvent: THandle;
      FFileName: string;
      FPersistedParams: TDictionary<string, UInt64>; // Dictionary that holds absolute offset from beginning of file
      FFileOptions: TdwlFileOptions;
      FCursor: IdwlFileCursor_Write;
    class procedure InitParamKinds;
    class procedure InitDeserializeClasses;
    procedure AddOrSetValue(const LowerKey: string; const Value: TValue);
    procedure Clear;
    procedure ClearKey(const LowerKey: string);
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
      Hook.InitDeserializeClasses;
    end;
    var Cursor := New_File(Hook.FFilename, Hook.FFileOptions).GetWriteCursor;
    Hook.FCursor := Cursor;
    if Cursor.FileSize=0 then
    begin
      Cursor.WriteUInt32(MAGIC_CARDINAL);
      Cursor.WriteUInt64(12); // empty file size
    end
    else
    begin
      if Cursor.ReadUInt32<>MAGIC_CARDINAL then
        raise Exception.Create('PersistedParams: Invalid Magic Word: wrong filetype?');
      var StoredFileSize := Cursor.ReadUInt64;
      // check the filesize and correct if needed
      if StoredFileSize<>Cursor.FileSize then
        Cursor.SetFileSize(StoredFileSize);
      while not Cursor.Eof do
      begin
        if Cursor.ReadUInt16<>MAGIC_WORD then
          raise Exception.Create('PersistedParams: Invalid Entry: wrong filetype?');
        var Name := Cursor.ReadString_LenByte;
        if Cursor.ReadUInt8<>pkDeleted then
          Hook.FPersistedParams.Add(Name, Cursor.CursorOffset-SizeOf(UInt8){correction for read ParamKind});
        Cursor.Seek(Cursor.ReadUInt32, soCurrent); // skip the data
      end;
    end;
  finally
    Hook.FInitNotReady := false;
    SetEvent(Hook.FInitEvent);
  end;
end;

{ TdwlPersistedParams_Hook }

procedure TdwlPersistedParams_Hook.AddOrSetValue(const LowerKey: string; const Value: TValue);
  procedure WriteHeader(ParamKind: byte);
  begin
    FCursor.Seek(0, soEnd);
    FCursor.WriteUInt16(MAGIC_WORD);
    FCursor.WriteString_LenByte(LowerKey);
    FPersistedParams.Add(LowerKey, FCursor.CursorOffset);
    FCursor.WriteUInt8(ParamKind);
  end;
begin
  CheckInitReady;
  var ParamType: TdwlPersistedParamType;
  if not FTypeInfo2ParamKind.TryGetValue(Value.TypeInfo, ParamType) then
  begin
    ParamType.SimpleDataSize := 0;
    if Value.TypeInfo.Kind=tkInterface then
      ParamType.ParamKind := pkInterface
    else
      raise Exception.Create('PersistedParams: Missing type '+string(Value.TypeInfo.Name));
  end;
  var FallBackFileSize := FCursor.FileSize;
  try
    // check is value already exists
    var Offset: uInt64;
    if FPersistedParams.TryGetValue(LowerKey, Offset) then
    begin
      // check if current value is overwritable
      FCursor.Seek(Offset, soBeginning);
      if ParamType.SimpleDataSize>0 then
      begin
        var ReadParamType := FCursor.ReadUInt8;
        if ReadParamType=ParamType.ParamKind then
        begin
          FCursor.ReadUInt32; // datasize, should be same as SimpleDataSize
          var ValueRawData := Value.GetReferenceToRawData;
          FCursor.Write(ValueRawData^, ParamType.SimpleDataSize);
          Exit; // that's it for a simpletype overwrite: finished
        end;
        FCursor.Seek(-SizeOf(UInt8), soCurrent); // to step back onto paramkind
      end;
      // otherwise no reuse, delete current value
      FCursor.WriteUInt8(pkDeleted); // assign oldvalue as deleted
      FPersistedParams.Remove(LowerKey);
    end;
    // write value to file
    if ParamType.SimpleDataSize>0 then
    begin
      WriteHeader(ParamType.ParamKind);
      FCursor.WriteUInt32(ParamType.SimpleDataSize);
      var ValueData := Value.GetReferenceToRawData;
      FCursor.Write(ValueData^, ParamType.SimpleDataSize);
    end
    else
    begin
      case ParamType.ParamKind of
      pkString:
        begin
          WriteHeader(ParamType.ParamKind);
          FCursor.WriteString_LenCardinal(Value.AsString);
        end;
      pkInterface:
        begin
          var Intf := Value.AsInterface;
          var Obj := Intf as TObject;
          var RttiType :=   RttiContext.GetType(Obj.ClassType);
          var Method := RttiType.GetMethod(METHOD_SERIALIZETOCURSOR);
          if Method=nil then
            raise Exception.Create('PersistedParams: unimplemented '+RttiType.Name+'.'+METHOD_SERIALIZETOCURSOR);
          var IntfName := '';
          for var IntfType in RttiType.AsInstance.GetImplementedInterfaces do
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
            raise Exception.Create('PersistedParams: problems finding interfacename for '+RttiType.Name);
          WriteHeader(ParamType.ParamKind);
          var DataSizeOffset := FCursor.CursorOffset;
          FCursor.WriteUInt32(0); // Temporary DataSize
          FCursor.WriteString_LenByte(Obj.ClassName);
          FCursor.WriteString_LenByte(IntfName);
          Method.Invoke(Obj, [TValue.From(FCursor)]);
          var DataSize := FCursor.CursorOffset-DataSizeOffSet-SizeOf(UInt32);
          FCursor.Seek(DataSizeOffset, soBeginning);
          FCursor.WriteUInt32(DataSize); // Final DataSize
        end;
      end;
    end;
  except
    FCursor.SetFileSize(FallBackFileSize);
  end;
  Flush;
end;

procedure TdwlPersistedParams_Hook.Initialize(ProvideProc: TProvideKeyCallBack);
begin
  FProvideProc := ProvideProc;
  TInitThread.Create(Self);
end;

class procedure TdwlPersistedParams_Hook.InitDeserializeClasses;
begin
  var TheClasses := FCtx.GetTypes;
  for var RttiType in TheClasses do
  begin
    if RttiType.IsInstance then
    begin
      var CreateMethod := RttiType.GetMethod(METHOD_CREATE);
      var DeSerializeMethod := RttiType.GetMethod(METHOD_DESERIALIZEFROMCURSOR);
      if (DeSerializeMethod<>nil) and (CreateMethod<>nil)  then
      begin
        var DeSerializeClass: TdwlDeSerializeClass;
        DeSerializeClass.RttiType := RttiType;
        DeSerializeClass.CreateMethod := CreateMethod;
        DeSerializeClass.DeSerializeMethod := DeSerializeMethod;
        DeSerializeClass.MetaClassType := RttiType.AsInstance.MetaclassType;
        FDeSerializeClasses.Add(RttiType.Name, DeSerializeClass);
      end;
    end;
  end;
end;

class procedure TdwlPersistedParams_Hook.InitParamKinds;
  procedure AddType(ParamKind: byte; SimpleDataSize: byte; TypeInfo: PTypeInfo);
  begin
    var ParameterType: TdwlPersistedParamType;
    Assert(byte(ParamKind)=FParamKinds.Count);
    ParameterType.ParamKind := ParamKind;
    ParameterType.SimpleDataSize := SimpleDataSize;
    ParameterType.TypeInfo := TypeInfo;
    FParamKinds.Add(ParameterType);
    FTypeInfo2ParamKind.Add(ParameterType.TypeInfo, ParameterType);
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
  AddType(pkInterface, 0, nil);
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
  var Offset: UInt64;
  if FPersistedParams.TryGetValue(LowerKey, Offset) then
  begin
    FCursor.Seek(Offset, soBeginning);
    FCursor.WriteUInt8(pkDeleted);
    FPersistedParams.Remove(LowerKey);
  end;
end;

class constructor TdwlPersistedParams_Hook.Create;
begin
  inherited;
  FTypeInfo2ParamKind := TDictionary<PTypeInfo, TdwlPersistedParamType>.Create;
  FParamKinds := TList<TdwlPersistedParamType>.Create;
  FDeSerializeClasses := TDictionary<string, TdwlDeSerializeClass>.Create;
  FClassInitNeeded := true;
end;

constructor TdwlPersistedParams_Hook.Create(const FileName: string);
begin
  inherited Create;
  FPersistedParams := TDictionary<string, UInt64>.Create;
  FFileName := FileName;
  FInitEvent := CreateEvent(nil, false, false, nil);
  FInitNotReady := true;
  FFileOptions := [pfoCreateIfNeeded];
end;

class destructor TdwlPersistedParams_Hook.Destroy;
begin
  FTypeInfo2ParamKind.Free;
  FParamKinds.Free;
  FDeSerializeClasses.Free;
  inherited;
end;

destructor TdwlPersistedParams_Hook.Destroy;
begin
  CloseHandle(FInitEvent);
  FPersistedParams.Free;
  inherited Destroy;
end;

procedure TdwlPersistedParams_Hook.Flush;
begin
  // write current filesize for quality purposes
  FCursor.Seek(4, soBeginning);
  FCursor.WriteUInt64(FCursor.FileSize);
  // flush the memory mapped file
  FCursor.Flush;
end;

type
  THackObj = class(TInterfacedObject)
  end;

function TdwlPersistedParams_Hook.TryGetValueFromOffset(Offset: UInt64; var Value: TValue): boolean;
begin
  try
    FCursor.Seek(Offset, soBeginning);
    var ParamType := FParamKinds[FCursor.ReadUInt8];
    if ParamType.SimpleDataSize>0 then
    begin
      FCursor.ReadUInt32; // skip datasize
      TValue.Make(FCursor.CursorPtr, ParamType.TypeInfo, Value)
    end
    else
    begin
      case ParamType.ParamKind of
      pkString: FCursor.ReadString_LenCardinal;
      pkInterface:
        begin
          FCursor.ReadUInt32; //seek over datasize
          var ClassName := FCursor.ReadString_LenByte;
          var IntfName := FCursor.ReadString_LenByte;
          var ClassDef: TdwlDeSerializeClass;
          if not FDeSerializeClasses.TryGetValue(ClassName, ClassDef) then
            raise Exception.Create('PersistedParams: No DeSerialize Class found for '+ClassName);
          var IntfDef: TRttiInterfaceType := nil;
          for var IntfType in  ClassDef.RttiType.AsInstance.GetImplementedInterfaces do
          begin
            if IntfType.Name=IntfName then
            begin
              IntfDef := IntfType;
              Break;
            end;
          end;
          if IntfDef=nil then
            raise Exception.Create('PersistedParams: No DeSerialize Interface found for '+IntfName);
          var Obj := ClassDef.CreateMethod.Invoke(ClassDef.MetaclassType, []).AsObject;
          var IntfInst: IUnknown;
          Obj.GetInterface(IntfDef.GUID, IntfInst);
          ClassDef.DeSerializeMethod.Invoke(Obj, [TValue.From(FCursor)]);
          TValue.Make(@IntfInst, IntfDef.Handle, Value);
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
        if TryGetValueFromOffset(Enumerator.Current.Value, Value) then
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
  var Offset: UInt64;
  Result := FPersistedParams.TryGetValue(LowerKey, Offset) and TryGetValueFromOffset(Offset, Value);
end;

end.
