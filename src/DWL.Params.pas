unit DWL.Params;

interface

uses
  System.Rtti, System.Generics.Collections, DWL.Resolver, System.JSON;

type
  IdwlParams=interface;

  /// <summary>
  ///   Definition of callback procedure that will be used to signal changes
  ///   ofthe content of the IdwlParams store
  /// </summary>
  TChangeMethodCallbackProc = procedure(Sender: IdwlParams; const Key: string; const Value: TValue) of object;
  TChangeRegularCallbackProc = procedure(Sender: IdwlParams; const Key: string; const Value: TValue);

  /// <summary>
  ///   Enumeration interface that will be return when requestion an
  ///   enumeratoron a key/value store <br />
  /// </summary>
  IdwlParamsEnumerator = interface
    function CurrentKey: string;
    function CurrentValue: TValue;
    function MoveNext: boolean;
  end;

  /// <summary>
  ///   The Params interface is a reference to an object representing an
  ///   inmemory store of key/value pairs. An interface to an empty object can
  ///   becreated by calling New_Params <br />
  /// </summary>
  IdwlParams = interface
    /// <summary>
    ///   Clones the Params object, f.e. as thread protection to be used in another thread
    /// </summary>
    function Clone: IdwlParams;
    /// <summary>
    ///   Sometimes triggers must be executed manually by a caller f.e. when
    ///   the stored filename does not change, but the contents of the file do
    ///   only use when you know what you are doing ;-)
    /// </summary>
    procedure ExecuteTriggers(const TriggerKey: string);
    /// <summary>
    ///   Assigns for the given keys the resolved, metadata enriched value to
    ///   the given params
    /// </summary>
    /// <param name="Params">
    ///   The Params that will receive the key/value pairs
    /// </param>
    /// <param name="Keys">
    ///   The keys of the key/value pairs that will be copied to the Params
    /// </param>
    procedure AssignTo(Params: IdwlParams; Keys: TArray<string>); overload;
    /// <summary>
    ///   With AddTrigger you can add a one-shot trigger to be fired. Once the
    ///   TriggerKey is changed (or cleared) the DependentKey will
    ///   automatically be removed from the params (cleared)
    /// </summary>
    /// <param name="TriggerKey">
    ///   Key that will watched for changes
    /// </param>
    /// <param name="DependentKey">
    ///   Key of pair that will be removed from the store
    /// </param>
    procedure AddTrigger(const TriggerKey, DependentKey: string);
    /// <summary>
    ///   Copy all key/value pairs in the store to another store
    /// </summary>
    /// <param name="Params">
    ///   The Params that will receive the key/value pairs
    /// </param>
    procedure AssignTo(Params: IdwlParams); overload;
    /// <summary>
    ///   Get the value of the pair, if needed converted to the boolean type
    /// </summary>
    /// <param name="Key">
    ///   The key of the pair
    /// </param>
    /// <param name="Default">
    ///   a default to be returned when the pair does not exist
    /// </param>
    /// <remarks>
    ///   Please note that: <br />- metakeys will be applied with higher
    ///   priority <br />- when an existent value is not a valid boolean, the
    ///   default will be returned <br />- textual values will be resolved
    ///   using TdwlResolver functionality
    /// </remarks>
    function BoolValue(const Key: string; Default: boolean=false): boolean;
    /// <summary>
    ///   Get the value of the pair, if needed converted to the double type
    /// </summary>
    /// <param name="Key">
    ///   The key of the pair
    /// </param>
    /// <param name="Default">
    ///   default to be returned when the pair does not exist
    /// </param>
    /// <remarks>
    ///   Please note that: <br />- metakeys will be applied with higher
    ///   priority <br />- when an existent value is not a valid double, the
    ///   default will be returned <br />- textual values will be resolved
    ///   using TdwlResolver functionality
    /// </remarks>
    function DoubleValue(const Key: string; Default: double=0): double;
    /// <summary>
    ///   Get the value of the pair, if needed converted to the cardinal type
    /// </summary>
    /// <param name="Key">
    ///   The key of the pair
    /// </param>
    /// <param name="Default">
    ///   default to be returned when the pair does not exist
    /// </param>
    /// <remarks>
    ///   Please note that: <br />- metakeys will be applied with higher
    ///   priority <br />- when an existent value is not a valid cardinal, the
    ///   default will be returned <br />- textual values will be resolved
    ///   using TdwlResolver functionality
    /// </remarks>
    function CardinalValue(const Key: string; Default: cardinal=0): cardinal;
    /// <summary>
    ///   Get the value of the pair, if needed converted to the integer type
    /// </summary>
    /// <param name="Key">
    ///   The key of the pair
    /// </param>
    /// <param name="Default">
    ///   default to be returned when the pair does not exist
    /// </param>
    /// <remarks>
    ///   Please note that: <br />- metakeys will be applied with higher
    ///   priority <br />- when an existent value is not a valid integer, the
    ///   default will be returned <br />- textual values will be resolved
    ///   using TdwlResolver functionality
    /// </remarks>
    function IntValue(const Key: string; Default: integer=0): integer;
    /// <summary>
    ///   Get the value of the pair, if needed converted to the integer64 type
    /// </summary>
    /// <param name="Key">
    ///   The key of the pair
    /// </param>
    /// <param name="Default">
    ///   default to be returned when the pair does not exist
    /// </param>
    /// <remarks>
    ///   Please note that: <br />- metakeys will be applied with higher
    ///   priority <br />- when an existent value is not a valid integer64, the
    ///   default will be returned <br />- textual values will be resolved
    ///   using TdwlResolver functionality
    /// </remarks>
    function Int64Value(const Key: string; Default: Int64=0): Int64;
    /// <summary>
    ///   Get the value of the pair, if needed converted to the string type
    /// </summary>
    /// <param name="Key">
    ///   The key of the pair
    /// </param>
    /// <param name="Default">
    ///   default to be returned when the pair does not exist
    /// </param>
    /// <remarks>
    ///   Please note that: <br />- metakeys will be applied with higher
    ///   priority <br />- values will be resolved using TdwlResolver
    ///   functionality <br />
    /// </remarks>
    function StrValue(const Key: string; const Default: string=''): string;
    /// <summary>
    ///   function to check of a pair with the give key existst in the store
    /// </summary>
    /// <param name="Key">
    ///   Key to check
    /// </param>
    function ContainsKey(const Key: string): boolean;
    /// <summary>
    ///   TryGetBareValue is the faster, but simple retrieval function of
    ///   values by key, NO meta data lookup, resolving etc is done in this
    ///   function
    /// </summary>
    /// <param name="Key">
    ///   the key of the pair to be returned
    /// </param>
    /// <param name="Value">
    ///   will receive the retrieved value
    /// </param>
    /// <returns>
    ///   indicating if the retrieval was successful
    /// </returns>
    function TryGetBareValue(const Key: string; out Value: TValue): boolean;
    /// <summary>
    ///   The main retrieval function, all functionality like meta data lookup,
    ///   resolving etc is done in this function if you want to retrieve the
    ///   bare stored value you can use the TryGetBareValue variant.
    /// </summary>
    /// <param name="Key">
    ///   the key of the pair to be returned
    /// </param>
    /// <param name="Value">
    ///   will receive the retrieved value
    /// </param>
    /// <returns>
    ///   indicating if the retrieval was successful
    /// </returns>
    /// <remarks>
    ///   If applicable metakeys will be applied and the retrieval will be
    ///   successful
    /// </remarks>
    function TryGetValue(const Key: string; out Value: TValue): boolean;
    /// <summary>
    ///   Try to get the value of the pair, if needed converted to the byte
    ///   type
    /// </summary>
    /// <param name="Key">
    ///   the key of the pair to be returned
    /// </param>
    /// <param name="Value">
    ///   will receive the retrieved value
    /// </param>
    /// <returns>
    ///   indicating if the retrieval was successful
    /// </returns>
    /// <remarks>
    ///   Please note that: <br />- if applicable metakeys will be applied and
    ///   the retrieval will be successful <br />- textual values will be
    ///   resolved using TdwlResolver functionality
    /// </remarks>
    function TryGetByteValue(const Key: string; out Value: byte): boolean;
    /// <summary>
    ///   Try to get the value of the pair, if needed converted to the byte
    ///   type
    /// </summary>
    /// <param name="Key">
    ///   the key of the pair to be returned
    /// </param>
    /// <param name="Value">
    ///   will receive the retrieved value
    /// </param>
    /// <returns>
    ///   indicating if the retrieval was successful
    /// </returns>
    /// <remarks>
    ///   Please note that: <br />- if applicable metakeys will be applied and
    ///   the retrieval will be successful <br />- textual values will be
    ///   resolved using TdwlResolver functionality
    /// </remarks>
    function TryGetIntValue(const Key: string; out Value: integer): boolean;
    /// <summary>
    ///   Try to get the value of the pair, if needed converted to the byte
    ///   type
    /// </summary>
    /// <param name="Key">
    ///   the key of the pair to be returned
    /// </param>
    /// <param name="Value">
    ///   will receive the retrieved value
    /// </param>
    /// <returns>
    ///   indicating if the retrieval was successful
    /// </returns>
    /// <remarks>
    ///   Please note that: <br />- if applicable metakeys will be applied and
    ///   the retrieval will be successful <br />- textual values will be
    ///   resolved using TdwlResolver functionality
    /// </remarks>
    function TryGetInt64Value(const Key: string; out Value: Int64): boolean;
    /// <summary>
    ///   Try to get the value of the pair, if needed converted to the byte
    ///   type
    /// </summary>
    /// <param name="Key">
    ///   the key of the pair to be returned
    /// </param>
    /// <param name="Value">
    ///   will receive the retrieved value
    /// </param>
    /// <returns>
    ///   indicating if the retrieval was successful
    /// </returns>
    /// <remarks>
    ///   Please note that: <br />- if applicable metakeys will be applied and
    ///   the retrieval will be successful <br />- textual values will be
    ///   resolved using TdwlResolver functionality
    /// </remarks>
    function TryGetStrValue(const Key: string; out Value: string): boolean;
    /// <summary>
    ///   GetBareValue is the faster, but simple retrieval function of values
    ///   by key, NO meta data lookup, resolving etc is done in this function
    /// </summary>
    /// <param name="Key">
    ///   the key of the pair to be returned
    /// </param>
    /// <param name="Default">
    ///   Default to be returned when the pair is not present
    /// </param>
    function GetBareValue(const Key: string; Default: TValue): TValue;
    /// <summary>
    ///   value retrieval function, all functionality like meta data lookup,
    ///   resolving etc is done in this function if you want to retrieve the
    ///   bare stored value you can use the GetBareValue variant.
    /// </summary>
    /// <param name="Key">
    ///   the key of the pair to be returned
    /// </param>
    /// <param name="Default">
    ///   Default to be returned when the pair is not present
    /// </param>
    /// <remarks>
    ///   If applicable metakeys will be applied with higher priority than the
    ///   default
    /// </remarks>
    function GetValue(const Key: string; Default: TValue): TValue;
    /// <summary>
    ///   Writes a key/value pair into the store
    /// </summary>
    /// <param name="Key">
    ///   the key of the pair
    /// </param>
    /// <param name="Value">
    ///   the value of the pair
    /// </param>
    /// <remarks>
    ///   A very important paradigm is that keys are always enforced to be
    ///   lowercase (No matter how you feed them into the function) This is to
    ///   guarantee a uniform case-insensitive resolving and reading of keys <br />
    /// </remarks>
    procedure WriteValue(const Key: string; const Value: TValue);
    /// <summary>
    ///   put the presented keys in the form of Name Value pairs (as f.e. used
    ///   in TStringList) into the store'
    /// </summary>
    /// <param name="NameValueLines">
    ///   string containing pairs. every line has a pair. Key and value are
    ///   seperated by an equal sign (=)
    /// </param>
    procedure WriteNameValueText(const NameValueLines: string);
    /// <summary>
    ///   put the presented keys in the form of single json object text into
    ///   the store'
    /// </summary>
    /// <param name="JSON">
    ///   string containing the JSON object (or as overload a TJSONObject) of which the properties will be put
    ///   in the store as pairs
    /// </param>
    procedure WriteJSON(const JSON: string); overload;
    procedure WriteJSON(const JSON: TJSONObject); overload;
    /// <summary>
    ///   Removes the pair with teh given key from the store
    /// </summary>
    /// <param name="Key">
    ///   the key of the pair to be removed
    /// </param>
    procedure ClearKey(const Key: string);
    /// <summary>
    ///   resolves the given string resolvable keys will be replaced by their
    ///   values
    /// </summary>
    /// <param name="Str">
    ///   string to be resolved
    /// </param>
    procedure Resolve(var Str: string);
    /// <summary>
    ///   Gets an enumerator to process al the pairs in the store
    /// </summary>
    function GetEnumerator: IdwlParamsEnumerator;
    /// <summary>
    ///   Gets the pairs from the store as a name value text. every line has a
    ///   pair, the key and the values of the pair are separated by an equal
    ///   sign (=)
    /// </summary>
    function GetAsNameValueText(UrlEncodeValues: boolean=true): string;
    /// <summary>
    ///   Put the pairs from the store into a JSON Object,
    ///   the params are the pairs
    /// </summary>
    procedure PutIntoJSONObject(JSONObject: TJSONObject);
    /// <summary>
    ///   empties the store
    /// </summary>
    procedure Clear;
    /// <summary>
    ///   Change Tracking is disabled by default. If you enable Changetracking,
    ///   the keys will be watched for changes and the CallBackProc will be
    ///   called
    /// </summary>
    /// <param name="CallBackProc">
    ///   The procedure to be called in the case of changes
    /// </param>
    procedure EnableChangeTracking(CallBackProc: TChangeMethodCallBackProc); overload;
    procedure EnableChangeTracking(CallBackProc: TChangeRegularCallBackProc); overload;
  end;

  /// <summary>
  ///   A record structure to represent a key/value pair as one record
  /// </summary>
  TdwlParamPair = record
    Key: string;
    Value: TValue;
    constructor Create(const AKey: string; const AValue: TValue);
  end;

/// <summary>
///   Will generate an interface to an empty in-memory store of key/value pairs
/// </summary>
/// <param name="Domain">
///   An optional domain that will be used to work with the metadata added for
///   this particular domain
/// </param>
function New_Params(const Domain: string=''): IdwlParams; overload;

/// <summary>
///   Will generate an interface to an in-memory store of key/value pairs.
///   Thestore will be intially filled with the key/value pair provided
/// </summary>
/// <param name="Key">
///   The Key of the initial key/value pair
/// </param>
/// <param name="Value">
///   The Value of the initial key/value pair
/// </param>
function New_Params(const Key: string; const Value: TValue): IdwlParams; overload;

/// <summary>
///   Will generate an interface to an in-memory store of key/value pairs.
///   Theprovided pairs will be used to initial fill the store
/// </summary>
/// <param name="ParamPairs">
///   The key/value pairs that will be added as initial fill
/// </param>
function New_Params(ParamPairs: array of TdwlParamPair): IdwlParams; overload;

/// <summary>
///   Adding a metakey provides additional intelligence to the Params store.
///   f.e. the meta default will be taken into account when retrieving a value.
///   This default has a higher priority than the default from the calling
///   procedure.
/// </summary>
/// <param name="Domain">
///   The domain in which the metkey will have it's applicability
/// </param>
/// <param name="Key">
///   The key applicable for the given default value
/// </param>
/// <param name="Default">
///   The default value to be returned if the key/value pari is not present
/// </param>
/// <remarks>
///   <para>
///     Note that when using metakeys, using a domain is a requirement
///   </para>
///   <para>
///     You can differentiate between metakeys by using domains. metakeys are
///     system wide and applicable to all Params using the provided domain <br />
///   </para>
///   <para>
///     WARNING: MetaKey usage is not threadsafe, add meta keys before using
///     a IdwlParams instance in a multithreaded context
///   </para>
/// </remarks>
procedure AddGlobalMetaKey(const Domain, Key: string; Default: TValue);


implementation

uses
  System.SysUtils, System.Generics.Defaults,
  System.TypInfo, System.Classes, DWL.Rtti.Utils, DWL.ConvUtils,
  System.NetEncoding;

type
  TdwlMetaKey = class
  strict private
    FDefault: TValue;
  public
    property Default: TValue read FDefault;
    constructor Create(ADefault: TValue);
  end;

  TdwlParamsEnumerator = class(TInterfacedObject, IdwlParamsEnumerator)
  strict private
    FEnumerator: TEnumerator<TPair<string,TValue>>;
  protected
    function CurrentKey: string;
    function CurrentValue: TValue;
    function MoveNext: boolean;
  public
    constructor Create(Enumerator: TEnumerator<TPair<string,TValue>>);
    destructor Destroy; override;
  end;

  TdwlParams = class(TInterfacedObject, IdwlParams)
  strict private
  class var
    FMetaKeys: TObjectDictionary<string, TdwlMetaKey>;
  var
    FParams: TDictionary<string, TValue>;
    FTriggers: TList<TPair<string, string>>;
    FDomain: string;
    FChangeMethodCallbackProc: TChangeMethodCallBackProc;
    FChangeRegularCallbackProc: TChangeRegularCallBackProc;
    class function DictKey(const Domain, Key: string): string;
    function BoolValue(const Key: string; Default: boolean=false): boolean;
    function CardinalValue(const Key: string; Default: cardinal=0): cardinal;
    function IntValue(const Key: string; Default: integer=0): integer;
    function Int64Value(const Key: string; Default: Int64=0): Int64;
    function StrValue(const Key: string; const Default: string=''): string;
    function DoubleValue(const Key: string; Default: double=0): double;
    function GetBareValue(const Key: string; Default: TValue): TValue;
    function GetValue(const Key: string; Default: TValue): TValue;
    function TryGetBareValue(const Key: string; out Value: TValue): boolean;
    function TryGetValue(const Key: string; out Value: TValue): boolean; overload;
    function TryGetValue<T>(const Key: string; out Value: T): boolean; overload;
    function TryGetByteValue(const Key: string; out Value: byte): boolean;
    function TryGetIntValue(const Key: string; out Value: integer): boolean;
    function TryGetInt64Value(const Key: string; out Value: Int64): boolean;
    function TryGetCardinalValue(const Key: string; out Value: cardinal): boolean;
    function TryGetStrValue(const Key: string; out Value: string): boolean;
    function TryGetDoubleValue(const Key: string; out Value: double): boolean;
    procedure AddTrigger(const TriggerKey, DependentKey: string);
    function Clone: IdwlParams;
    procedure ExecuteTriggers(const TriggerKey: string);
    procedure Resolve(var Str: string);
    procedure EnableChangeTracking(CallBackProc: TChangeMethodCallBackProc); overload;
    procedure EnableChangeTracking(CallBackProc: TChangeRegularCallBackProc); overload;
    procedure WriteValue(const Key: string; const Value: TValue);
    procedure ClearKey(const Key: string);
    function ContainsKey(const Key: string): boolean;
    procedure AssignTo(Params: IdwlParams; Keys: TArray<string>); overload;
    procedure AssignTo(Params: IdwlParams); overload;
    function GetAsNameValueText(UrlEncodeValues: boolean=true): string;
    procedure PutIntoJSONObject(JSONObject: TJSONObject);
    function GetEnumerator: IdwlParamsEnumerator;
    procedure WriteNameValueText(const NameValueLines: string);
    procedure WriteJSON(const JSON: string); overload;
    procedure WriteJSON(const JSON: TJSONObject); overload;
    procedure Clear;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure AddMetaKey(const Domain, Key: string; Default: TValue);
    constructor Create(const Domain: string);
    destructor Destroy; override;
  end;

function New_Params(const Domain: string): IdwlParams;
begin
  Result := TdwlParams.Create(Domain);
end;

function New_Params(ParamPairs: array of TdwlParamPair): IdwlParams;
begin
  Result := New_Params;
  for var P in ParamPairs do
    Result.WriteValue(P.Key, P.Value);
end;

function New_Params(const Key: string; const Value: TValue): IdwlParams; overload;
begin
  Result := New_Params;
  Result.WriteValue(Key, Value);
end;

procedure AddGlobalMetaKey(const Domain, Key: string; Default: TValue);
begin
  TdwlParams.AddMetaKey(Domain, Key, Default);
end;

{ TdwlParams }

class procedure TdwlParams.AddMetaKey(const Domain, Key: string; Default: TValue);
begin
  FMetaKeys.Add(DictKey(Domain.ToLower, Key.ToLower), TdwlMetaKey.Create(Default));
end;

procedure TdwlParams.AddTrigger(const TriggerKey, DependentKey: string);
begin
  // triggers are not created immediately, but on first use
  // this is efficient and (ftriggers=nil) can be used
  // elsewhere in the object to check if triggering is active
  if FTriggers=nil then
    FTriggers := TList<TPair<string, string>>.Create;
  FTriggers.Add(TPair<string, string>.Create(TriggerKey.ToLower, DependentKey.ToLower));
end;

procedure TdwlParams.AssignTo(Params: IdwlParams);
begin
  var Enumerator := FParams.GetEnumerator;
  try
    while Enumerator.MoveNext do
      Params.WriteValue(Enumerator.Current.Key, Enumerator.Current.Value);
  finally
    Enumerator.Free;
  end;
end;

procedure TdwlParams.AssignTo(Params: IdwlParams; Keys: TArray<string>);
begin
  var V: TValue;
  for var Key in Keys do
    if TryGetValue(Key, V) then
      Params.WriteValue(Key, V);
end;

function TdwlParams.GetAsNameValueText(UrlEncodeValues: boolean=true): string;
begin
  Result := '';
  var Enumerator := FParams.GetEnumerator;
  try
    if UrlEncodeValues then
    begin
      while Enumerator.MoveNext do
        Result := Result+Enumerator.Current.Key+'='+TNetEncoding.URL.Encode(Enumerator.Current.Value.ToString)+#13#10;
    end
    else
    begin
      while Enumerator.MoveNext do
        Result := Result+Enumerator.Current.Key+'='+Enumerator.Current.Value.ToString+#13#10;
    end;
  finally
    Enumerator.Free;
  end;
end;

function TdwlParams.BoolValue(const Key: string; Default: boolean): boolean;
begin
  var Value: TValue;
 if (not TryGetValue(Key, Value)) or (not TryStrToBool(Value.ToString, Result)) then
    Result := Default;
end;

function TdwlParams.CardinalValue(const Key: string;Default: cardinal): cardinal;
begin
  if not TryGetCardinalValue(Key, Result) then
    Result := Default;
end;

procedure TdwlParams.Clear;
begin
  FParams.Clear;
end;

procedure TdwlParams.ClearKey(const Key: string);
begin
  var LowerKey := Key.ToLower;
  FParams.Remove(LowerKey);
  if Assigned(FChangeMethodCallbackProc) then
    FChangeMethodCallbackProc(Self, Key, TValue.Empty);
  if Assigned(FChangeRegularCallbackProc) then
    FChangeRegularCallbackProc(Self, Key, TValue.Empty);
  ExecuteTriggers(LowerKey);
end;

function TdwlParams.Clone: IdwlParams;
begin
  Result := New_Params(FDomain);
  AssignTo(Result);
  if FTriggers<>nil then
  begin
    for var Trigger in FTriggers do
      Result.AddTrigger(Trigger.Key, Trigger.Value);
  end;
end;

function TdwlParams.ContainsKey(const Key: string): boolean;
begin
  Result := FParams.ContainsKey(Key.ToLower);
end;

class constructor TdwlParams.Create;
begin
  inherited;
  FMetaKeys := TObjectDictionary<string, TdwlMetaKey>.Create([doOwnsValues]);
end;

constructor TdwlParams.Create(const Domain: string);
begin
  inherited Create;
  FDomain := Domain.ToLower;
  FParams := TDictionary<string, TValue>.Create(TIStringComparer.Ordinal);
end;

class destructor TdwlParams.Destroy;
begin
  FMetaKeys.Free;
  inherited;
end;

destructor TdwlParams.Destroy;
begin
  FTriggers.Free;
  FParams.Free;
  inherited Destroy;
end;

class function TdwlParams.DictKey(const Domain, Key: string): string;
begin
  Result := '#'+Domain+'#'+Key;
end;

function TdwlParams.DoubleValue(const Key: string; Default: double): double;
begin
  if not TryGetDoubleValue(Key, Result) then
    Result := Default
end;

procedure TdwlParams.EnableChangeTracking(CallBackProc: TChangeMethodCallBackProc);
begin
  FChangeMethodCallbackProc := CallBackProc;
end;

procedure TdwlParams.EnableChangeTracking(CallBackProc: TChangeRegularCallBackProc);
begin
  FChangeRegularCallbackProc := CallBackProc;
end;

procedure TdwlParams.ExecuteTriggers(const TriggerKey: string);
begin
  if FTriggers=nil then
    Exit;
  var i := FTriggers.Count-1;
  while i>=0 do
  begin
    var Pair := FTriggers[i];
    if Pair.Key=TriggerKey then
    begin
      ClearKey(Pair.Value);
      FTriggers.Delete(i);
    end;
    dec(i);
  end;
end;

function TdwlParams.GetBareValue(const Key: string; Default: TValue): TValue;
begin
  if not TryGetBareValue(Key, Result) then
    Result := Default;
end;

function TdwlParams.GetEnumerator: IdwlParamsEnumerator;
begin
  Result := TdwlParamsEnumerator.Create(FParams.GetEnumerator);
end;

function TdwlParams.Int64Value(const Key: string; Default: Int64): Int64;
begin
  if not TryGetInt64Value(Key, Result) then
    Result := Default
end;

function TdwlParams.IntValue(const Key: string; Default: integer): integer;
begin
  if not TryGetIntValue(Key, Result) then
    Result := Default
end;

procedure TdwlParams.PutIntoJSONObject(JSONObject: TJSONObject);
begin
  var Enumerator := FParams.GetEnumerator;
  try
    while Enumerator.MoveNext do
    begin
      var Value := Enumerator.Current.Value;
      case Value.Kind of
      tkChar,
      tkWChar,
      tkUString,
      tkWString,
      tkString: JSONObject.AddPair(Enumerator.Current.Key, Value.ToString);
      tkInteger: JSONObject.AddPair(Enumerator.Current.Key, Value.AsInteger);
      tkInt64: JSONObject.AddPair(Enumerator.Current.Key, Value.AsInt64);
      tkFloat: JSONObject.AddPair(Enumerator.Current.Key, Value.AsExtended);
      else
        raise Exception.Create('TValue: Unknown kind in PutIntoJSONObject');
      end;
    end;
  finally
    Enumerator.Free;
  end;
end;

procedure TdwlParams.Resolve(var Str: string);
begin
  var Walker := 1;
  var L_S := length(Str);
  var KeyLead := TdwlResolver.KeyLead;
  var KeyTrail := TdwlResolver.KeyTrail;
  var L_KeyLead := length(KeyLead);
  var L_KeyTrail := length(KeyTrail);
  while Walker<L_S do
  begin
    if (Str[Walker]=KeyLead[1]) and (Copy(Str, Walker, L_KeyLead)=KeyLead) then
    begin  // KeyLead found
      var ReferenceStart := Walker;
      inc(Walker, L_KeyLead);
      while (Walker<L_S) and ((Str[Walker]<>KeyTrail[1]) or (Copy(Str, Walker, L_KeyTrail)<>KeyTrail)) do
        inc(Walker);
      if Walker<=L_S then //KeyTrail found
      begin
        var ReplaceValue: TValue;
        if TryGetValue(Copy(Str, ReferenceStart+L_KeyLead, Walker-ReferenceStart-L_KeyLead).ToLower, ReplaceValue) then
        begin
          Str := Copy(Str, 1, ReferenceStart-1)+ReplaceValue.ToString+Copy(Str, Walker+L_KeyTrail, MaxInt);
          L_S := length(Str);
          Walker := ReferenceStart-1; // evaluate ReplaceStr again
        end
        else
          inc(Walker, L_KeyTrail-1);
      end;
    end;
    inc(Walker);
  end;
end;

function TdwlParams.StrValue(const Key: string; const Default: string): string;
begin
  if not TryGetStrValue(Key, Result) then
    Result := Default;
end;

function TdwlParams.TryGetBareValue(const Key: string; out Value: TValue): boolean;
begin
  Result := FParams.TryGetValue(Key.ToLower, Value);
end;

function TdwlParams.TryGetByteValue(const Key: string; out Value: byte): boolean;
begin
  Result := TryGetValue<byte>(Key, Value);
end;

function TdwlParams.TryGetCardinalValue(const Key: string; out Value: cardinal): boolean;
begin
  var V: TValue;
  Result := TryGetValue(Key, V);
  if not Result then
    Exit;
  Result := V.TryAsType(Value, false);
  if not Result then
    Result := TryStrToUInt(V.ToString, Value);
end;

function TdwlParams.TryGetDoubleValue(const Key: string; out Value: double): boolean;
begin
  var V: TValue;
  Result := TryGetValue(Key, V);
  if Result then
  begin
    Result := V.TryAsType(Value, false);
    if not Result then
      Result := TdwlConvUtils.TryDotStrToFloat(V.ToString, Value);
  end;
end;

function TdwlParams.TryGetInt64Value(const Key: string; out Value: Int64): boolean;
begin
  var V: TValue;
  Result := TryGetValue(Key, V);
  if not Result then
    Exit;
  Result := V.TryAsType(Value, false);
  if not Result then
    Result := TryStrToInt64(V.ToString, Value);
end;

function TdwlParams.TryGetIntValue(const Key: string; out Value: integer): boolean;
begin
  var V: TValue;
  Result := TryGetValue(Key, V);
  if not Result then
    Exit;
  Result := V.TryAsType(Value, false);
  if not Result then
    Result := TryStrToInt(V.ToString, Value);
end;

function TdwlParams.TryGetStrValue(const Key: string; out Value: string): boolean;
var
 V: TValue;
begin
  Result := TryGetValue(Key, V);
  if Result then
    Value := V.ToString;
end;

function TdwlParams.TryGetValue(const Key: string; out Value: TValue): boolean;
begin
  var LowerKey := Key.ToLower;
  Result := FParams.TryGetValue(LowerKey, Value);
  if not Result then
  begin // try to find a metakey and apply the default
    var MetaKey: TdwlMetaKey;
    if (FDomain<>'') and FMetaKeys.TryGetValue(DictKey(FDomain, LowerKey), MetaKey) and
      (not MetaKey.Default.IsEmpty) then
    begin
      Result := true;
      Value := MetaKey.Default;
    end;
  end;
  if Result then
  begin // if it's a string: resolve
    if Value.TypeInfo=TypeInfo(string) then
    begin
      var S := Value.ToString;
      Resolve(S);
      TdwlResolver.Resolve(S);
      Value := S;
    end;
  end;
end;

function TdwlParams.TryGetValue<T>(const Key: string; out Value: T): boolean;
begin
  var V: TValue;
  Result := TryGetValue(Key, V) and V.TryAsType(Value, false);
end;

function TdwlParams.GetValue(const Key: string; Default: TValue): TValue;
begin
  if not TryGetValue(Key, Result) then
    Result := Default;
end;

procedure TdwlParams.WriteJSON(const JSON: string);
begin
  var Obj := TJSONObject(TJSONObject.ParseJSONValue(JSON));
  if not (Obj is TJSONObject) then
    raise Exception.Create('JSON is not an object');
  WriteJSON(Obj);
end;

procedure TdwlParams.WriteJSON(const JSON: TJSONObject);
begin
  var ENum := JSON.GetENumerator;
  try
    while ENum.MoveNext do
    begin
      var Key := ENum.Current.JsonString.GetValue<string>;
      var Val := ENum.Current.JsonValue;
      if Val is TJSONNumber then // first check for number, because a TJSONNumber is also a TJSONString
        WriteValue(Key, TJSONNumber(Val).AsDouble)
      else
      if Val is TJSONString then
        WriteValue(Key, TJSONString(Val).Value)
      else
      if Val is TJSONBool then
        WriteValue(Key, TJSONBool(Val).AsBoolean)
      else
         raise Exception.Create('Unexpected JsonValue in pair');
    end;
  finally
    ENum.Free;
  end;
end;

procedure TdwlParams.WriteNameValueText(const NameValueLines: string);
begin
  var Lines := TStringList.Create;
  try
    Lines.Text := NameValueLines;
    for var i := 0 to Lines.Count-1 do
      WriteValue(Lines.Names[i], TNetEncoding.URL.Decode(Lines.Values[Lines.Names[i]]))
  finally
    Lines.Free;
  end;
end;

procedure TdwlParams.WriteValue(const Key: string; const Value: TValue);
begin
  var Lowerkey := Key.ToLower;
  // If a changecallback procedure or triggers are registered, we're introducing an optimization
  // So that the callbacck will only be called on a real change
  var OldValue: TValue;
  if (Assigned(FChangeMethodCallbackProc) or Assigned(FChangeRegularCallbackProc) or (FTriggers<>nil)) and
    TryGetBareValue(LowerKey, OldValue) and Value.Equals(OldValue) then
    Exit;
  FParams.AddOrSetValue(Lowerkey, Value);
  if Assigned(FChangeMethodCallbackProc) then
    FChangeMethodCallbackProc(Self, LowerKey, Value);
  if Assigned(FChangeRegularCallbackProc) then
    FChangeRegularCallbackProc(Self, LowerKey, Value);
  ExecuteTriggers(LowerKey);
end;

{ TdwlParamsEnumerator }

constructor TdwlParamsEnumerator.Create(Enumerator: TEnumerator<TPair<string, TValue>>);
begin
  inherited Create;
  FEnumerator := Enumerator;
end;

function TdwlParamsEnumerator.CurrentKey: string;
begin
  Result := FEnumerator.Current.Key;
end;

function TdwlParamsEnumerator.CurrentValue: TValue;
begin
  Result := FEnumerator.Current.Value;
end;

destructor TdwlParamsEnumerator.Destroy;
begin
  FEnumerator.Free;
  inherited Destroy;
end;

function TdwlParamsEnumerator.MoveNext: boolean;
begin
  Result := FEnumerator.MoveNext;
end;

{ TdwlParamPair }

constructor TdwlParamPair.Create(const AKey: string; const AValue: TValue);
begin
  Key := AKey;
  Value := AValue;
end;

{ TdwlMetaKey }

constructor TdwlMetaKey.Create(ADefault: TValue);
begin
  inherited Create;
  FDefault := ADefault;
end;

end.
