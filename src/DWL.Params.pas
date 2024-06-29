unit DWL.Params;

interface

uses
  System.Rtti, System.Generics.Collections, DWL.Resolver, System.JSON,
  System.TypInfo, DWL.Types;

type
  IdwlParams=interface;

  TParamTryCalculateProc = function(Params: IdwlParams; const LowerKey: string; var Value: TValue): boolean;

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

  TProvideKeyCallBack = procedure(const LowerKey: string; const Value: TValue) of object;

  TdwlCheckResult = record
    Success: boolean;
    ErrorMessage: string;
  end;

  IdwlParamsPersistHook = interface
    procedure AddOrSetValue(const LowerKey: string; const Value: TValue);
    procedure Clear;
    procedure ClearKey(const LowerKey: string);
    function ContainsKey(const LowerKey: string): boolean;
    function TryGetValue(const LowerKey: string; var Value: TValue): boolean;
    procedure ProvideUnknownKeys(const KnownKeys: TArray<string>);
    procedure Initialize(ProvideProc: TProvideKeyCallBack);
  end;

  IdwlMetaKeyConsulter = interface
    function DefaultValue: TValue;
    function Prettyname: string;
    function TypeInfo: PTypeInfo;
    function MinimumValue: TValue;
    function MaximumValue: TValue;
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
    procedure AssignKeysTo(Params: IdwlParams; Keys: TArray<string>); overload;
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
    ///   Copy all key/value pairs in the store to another store
    ///   but skip the calculated fields! (asdefined in the metadata)
    /// </summary>
    /// <param name="Params">
    ///   The Params that will receive the key/value pairs
    /// </param>
    /// <param name="ExlcudeKeyss">
    ///   These keys will be skipped and not assigned
    /// </param>
    procedure AssignTo(Params: IdwlParams; ExcludeKeys: TArray<string>); overload;
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
    ///   Get the value of the pair, if needed converted to the UnixEpoch type
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
    ///   Please note that: The property Default is an Int64 to make it possible to give it a default value
    ///   EmptyEpoch is not allowed as default value by teh compiler
    /// </remarks>
    function UnixEpochValue(const Key: string; const Default: Int64=0): TUnixEpoch;
    /// <summary>
    ///   function to check of a pair with the give key existst in the store
    /// </summary>
    /// <param name="Key">
    ///   Key to check
    /// </param>
    function ContainsKey(const Key: string): boolean;
    function CheckValue(const Key: string; const Value: TValue): TdwlCheckResult;
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
    ///   Try to get the value of the pair, if needed converted to the TUnixEpoch
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
    function TryGetUnixEpochValue(const Key: string; out Value: TUnixEpoch): boolean;
    /// <summary>
    ///   Try to get the value of the pair, if needed converted to the double
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
    function TryGetDoubleValue(const Key: string; out Value: double): boolean;
    /// <summary>
    ///   Try to get the value of the pair, if needed converted to the double
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
    function TryGetBoolValue(const Key: string; out Value: boolean): boolean;
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
    /// <param name="SkipPersisting">
    ///   default false, if persisting is skipped, the value is not forwarded
    ///   to the persisting handler
    /// </param>
    /// <remarks>
    ///   A very important paradigm is that keys are always enforced to be
    ///   lowercase (No matter how you feed them into the function) This is to
    ///   guarantee a uniform case-insensitive resolving and reading of keys <br />
    /// </remarks>
    procedure WriteValue(const Key: string; const Value: TValue; SkipPersisting: boolean=false);
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
    procedure EnableChangeTracking(CallBackProc: TChangeMethodCallBackProc; FRestrictToConfiguredMetaKeys: boolean=false); overload;
    procedure EnableChangeTracking(CallBackProc: TChangeRegularCallBackProc; FRestrictToConfiguredMetaKeys: boolean=false); overload;
    procedure RegisterPersistHook(PersistHook: IdwlParamsPersistHook);
    procedure UnRegisterPersistHook;
    function MetaKeyConsulter(const Key: string): IdwlMetaKeyConsulter;
  end;

  /// <summary>
  ///   A record structure to represent a key/value pair as one record
  /// </summary>
  TdwlParamPair = record
    Key: string;
    Value: TValue;
    constructor Create(const AKey: string; const AValue: TValue);
  end;

  IdwlMetaKeyBuilder = interface
    function CalculateProc(CalculateProc: TParamTryCalculateProc): IdwlMetaKeyBuilder;
    function ChangeTracking(const ChangeTrackingEnabled: boolean): IdwlMetaKeyBuilder;
    function DefaultValue(DefaultValue: TValue): IdwlMetaKeyBuilder;
    function MaximumValue(MaximumValue: TValue): IdwlMetaKeyBuilder;
    function MinimumValue(MinimumValue: TValue): IdwlMetaKeyBuilder;
    function PrettyName(const Prettyname: string): IdwlMetaKeyBuilder;
    function WriteDefaultValue: IdwlMetaKeyBuilder;
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
function AddParamsMetaKey(const Domain, Key: string; TypeInfo: PTypeInfo): IdwlMetaKeyBuilder;

implementation

uses
  System.SysUtils, System.Generics.Defaults,
  System.Classes, DWL.Rtti.Utils, DWL.ConvUtils,
  System.NetEncoding;

type
  TdwlMetaKey = class
  private
    FCalculators: TList<TParamTryCalculateProc>;
    FChangeTracking: boolean;
    FTypeInfo: PTypeInfo;
    FDefaultValue: TValue;
    FMaximumValue: TValue;
    FMinimumValue: TValue;
    FWriteDefaultValue: boolean;
    FPrettyName: string;
    procedure Check(Value: TValue);
    procedure RegisterCalculateProc(CalcProc: TParamTryCalculateProc);
    function TryCalculateValue(Params: IdwlParams; const LowerKey: string; var Value: TValue): boolean;
  public
    constructor Create(TypeInfo: PTypeInfo);
    destructor Destroy; override;
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
    FPersistHook: IdwlParamsPersistHook;
    FRequestedAllFromPersistHook: boolean;
    procedure ProvideKeyCallBack(const LowerKey: string; const Value: TValue);
    function TryGetFromPersistHook(const LowerKey: string; var Value: TValue): boolean;
    function TryGetMetaKey(const LowerKey: string; out MetaKey: TdwlMetaKey): boolean;
  class var
    FMetaKeys: TObjectDictionary<string, TdwlMetaKey>;
  var
    FParams: TDictionary<string, TValue>;
    FTriggers: TList<TPair<string, string>>;
    FDomain: string;
    FChangeCallBacksRestrictedToConfiguredMetaKeys: boolean;
    FChangeMethodCallbackProc: TChangeMethodCallBackProc;
    FChangeRegularCallbackProc: TChangeRegularCallBackProc;
    class function DictKey(const Domain, Key: string): string;
    function BoolValue(const Key: string; Default: boolean=false): boolean;
    function CardinalValue(const Key: string; Default: cardinal=0): cardinal;
    function IntValue(const Key: string; Default: integer=0): integer;
    function Int64Value(const Key: string; Default: Int64=0): Int64;
    function StrValue(const Key: string; const Default: string=''): string;
    function UnixEpochValue(const Key: string; const Default: Int64=0): TUnixEpoch;
    function DoubleValue(const Key: string; Default: double=0): double;
    function GetBareValue(const Key: string; Default: TValue): TValue;
    function GetValue(const Key: string; Default: TValue): TValue;
    function TryGetBareValue(const Key: string; out Value: TValue): boolean;
    function TryGetValue(const Key: string; out Value: TValue): boolean; overload;
    function TryGetByteValue(const Key: string; out Value: byte): boolean;
    function TryGetIntValue(const Key: string; out Value: integer): boolean;
    function TryGetInt64Value(const Key: string; out Value: Int64): boolean;
    function TryGetCardinalValue(const Key: string; out Value: cardinal): boolean;
    function TryGetStrValue(const Key: string; out Value: string): boolean;
    function TryGetUnixEpochValue(const Key: string; out Value: TUnixEpoch): boolean;
    function TryGetDoubleValue(const Key: string; out Value: double): boolean;
    function TryGetBoolValue(const Key: string; out Value: boolean): boolean;
    procedure AddTrigger(const TriggerKey, DependentKey: string);
    function Clone: IdwlParams;
    procedure ExecuteTriggers(const TriggerKey: string);
    procedure Resolve(var Str: string);
    procedure EnableChangeTracking(CallBackProc: TChangeMethodCallBackProc; FRestrictToConfiguredMetaKeys: boolean=false); overload;
    procedure EnableChangeTracking(CallBackProc: TChangeRegularCallBackProc; FRestrictToConfiguredMetaKeys: boolean=false); overload;
    function CheckValue(const Key: string; const Value: TValue): TdwlCheckResult;
    procedure WriteValue(const Key: string; const Value: TValue; SkipPersisting: boolean=false);
    procedure ClearKey(const Key: string);
    function ContainsKey(const Key: string): boolean;
    procedure AssignKeysTo(Params: IdwlParams; Keys: TArray<string>);
    procedure AssignTo(Params: IdwlParams); overload;
    procedure AssignTo(Params: IdwlParams; ExcludeKeys: TArray<string>); overload;
    function GetAsNameValueText(UrlEncodeValues: boolean=true): string;
    procedure PutIntoJSONObject(JSONObject: TJSONObject);
    function GetEnumerator: IdwlParamsEnumerator;
    procedure WriteNameValueText(const NameValueLines: string);
    procedure WriteJSON(const JSON: string); overload;
    procedure WriteJSON(const JSON: TJSONObject); overload;
    procedure Clear;
    procedure RegisterPersistHook(PersistHook: IdwlParamsPersistHook);
    procedure UnRegisterPersistHook;
    function MetaKeyConsulter(const Key: string): IdwlMetaKeyConsulter;
  public
    class constructor Create;
    class destructor Destroy;
    class function AddMetaKey(const Domain, Key: string; TypeInfo: PTypeInfo): TdwlMetaKey;
    constructor Create(const Domain: string);
    destructor Destroy; override;
  end;

  TdwlMetakeyBuilder = class(TInterfacedObject, IdwlMetaKeyBuilder)
  strict private
    FMetaKey: TdwlMetaKey;
  private
    function CalculateProc(CalculateProc: TParamTryCalculateProc): IdwlMetaKeyBuilder;
    function ChangeTracking(const ChangeTrackingEnabled: boolean): IdwlMetaKeyBuilder;
    function DefaultValue(DefaultValue: TValue): IdwlMetaKeyBuilder;
    function MaximumValue(MaximumValue: TValue): IdwlMetaKeyBuilder;
    function MinimumValue(MinimumValue: TValue): IdwlMetaKeyBuilder;
    function WriteDefaultValue: IdwlMetaKeyBuilder;
    function Prettyname(const PrettyName: string): IdwlMetaKeyBuilder;
  public
    constructor Create(MetaKey: TdwlMetaKey);
  end;

  TdwlMetakeyConsulter = class(TInterfacedObject, IdwlMetaKeyConsulter)
  strict private
    FKey: string;
    FMetaKey: TdwlMetaKey;
  private
    function DefaultValue: TValue;
    function Prettyname: string;
    function TypeInfo: PTypeInfo;
    function MinimumValue: TValue;
    function MaximumValue: TValue;
  public
    constructor Create(const Key: string; MetaKey: TdwlMetaKey);
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

function AddParamsMetaKey(const Domain, Key: string; TypeInfo: PTypeInfo): IdwlMetaKeyBuilder;
begin
  Result := TdwlMetakeyBuilder.Create(TdwlParams.AddMetaKey(Domain, Key, TypeInfo));
end;

{ TdwlParams }

class function TdwlParams.AddMetaKey(const Domain, Key: string; TypeInfo: PTypeInfo): TdwlMetaKey;
begin
  if not FMetaKeys.TryGetValue(DictKey(Domain.ToLower, Key.ToLower), Result) then
  begin
    Result := TdwlMetaKey.Create(TypeInfo);
    FMetaKeys.Add(DictKey(Domain.ToLower, Key.ToLower), Result);
  end;
end;

procedure TdwlParams.AddTrigger(const TriggerKey, DependentKey: string);
begin
  // triggers are not created immediately, but on first use
  // this is efficient and (ftriggers=nil) can be used
  // elsewhere in the object to check if triggering is active
  if FTriggers=nil then
    FTriggers := TList<TPair<string, string>>.Create;
  var Pair := TPair<string, string>.Create(TriggerKey.ToLower, DependentKey.ToLower);
  if not FTriggers.Contains(Pair) then
    FTriggers.Add(Pair);
end;

procedure TdwlParams.AssignTo(Params: IdwlParams);
begin
  var Enumerator := GetEnumerator;
  while Enumerator.MoveNext do
  begin
      // don't assign calculated values
      var MetaKey: TdwlMetaKey;
      if TryGetMetaKey(Enumerator.CurrentKey, MetaKey) and (MetaKey.FCalculators<>nil) then
        Continue;
    Params.WriteValue(Enumerator.CurrentKey, Enumerator.CurrentValue);
  end;
end;

procedure TdwlParams.AssignKeysTo(Params: IdwlParams; Keys: TArray<string>);
begin
  var V: TValue;
  for var Key in Keys do
  begin
    // don't assign calculated values
    var MetaKey: TdwlMetaKey;
    if TryGetMetaKey(Key, MetaKey) and (MetaKey.FCalculators<>nil) then
      Continue;
    if TryGetValue(Key, V) then
      Params.WriteValue(Key, V);
  end;
end;

procedure TdwlParams.AssignTo(Params: IdwlParams; ExcludeKeys: TArray<string>);
begin
  var ExclList := TStringList.Create;
  try
    for var ExclKey in ExcludeKeys do
      ExclList.Add(ExclKey.ToLower);
    ExclList.Sort;
    var Enumerator := GetEnumerator;
    while Enumerator.MoveNext do
    begin
      if ExclList.IndexOf(Enumerator.CurrentKey)>=0 then
        Continue;
      // don't assign calculated values
      var MetaKey: TdwlMetaKey;
      if TryGetMetaKey(Enumerator.CurrentKey, MetaKey) and (MetaKey.FCalculators<>nil) then
        Continue;
      Params.WriteValue(Enumerator.CurrentKey, Enumerator.CurrentValue);
    end;
  finally
    ExclList.Free;
  end;
end;

function TdwlParams.GetAsNameValueText(UrlEncodeValues: boolean=true): string;
begin
  Result := '';
  var Enumerator := GetEnumerator;
  if UrlEncodeValues then
  begin
    while Enumerator.MoveNext do
      Result := Result+Enumerator.CurrentKey+'='+TNetEncoding.URL.Encode(Enumerator.CurrentValue.ToString)+#13#10;
  end
  else
  begin
    while Enumerator.MoveNext do
      Result := Result+Enumerator.CurrentKey+'='+Enumerator.CurrentValue.ToString+#13#10;
  end;
end;

function TdwlParams.BoolValue(const Key: string; Default: boolean=false): boolean;
begin
  if not TryGetBoolValue(Key, Result) then
    Result := Default;
end;

function TdwlParams.CardinalValue(const Key: string;Default: cardinal=0): cardinal;
begin
  if not TryGetCardinalValue(Key, Result) then
    Result := Default;
end;

function TdwlParams.CheckValue(const Key: string; const Value: TValue): TdwlCheckResult;
  function CheckMinValue(ValA, ValB: TValue): boolean;
  begin
    if ValA.IsEmpty then
      Exit(true);
    case ValA.Kind of
    tkInteger: Result := ValA.AsInteger<=valB.AsInteger;
    tkFloat: Result := ValA.AsExtended<=valB.AsExtended;
    tkString: Result := ValA.AsString<=valB.AsString;
    else
      Result := false;
    end;
  end;
  function CheckMaxValue(ValA, ValB: TValue): boolean;
  begin
    if ValA.IsEmpty then
      Exit(true);
    case ValA.Kind of
    tkInteger: Result := ValA.AsInteger>=valB.AsInteger;
    tkFloat: Result := ValA.AsExtended>=valB.AsExtended;
    tkString: Result := ValA.AsString>=valB.AsString;
    else
      Result := false;
    end;
  end;

begin
  Result.Success := false;
  try
    var MetaKey: TdwlMetaKey;
    if not TryGetMetaKey(Key.ToLower, MetaKey) then
    begin
      Result.ErrorMessage := 'unable to check: no metadata found ';
      Exit;
    end;
    if not CheckMinValue(MetaKey.FMinimumValue, Value) then
    begin
      Result.ErrorMessage := 'value is below '+MetaKey.FMinimumValue.ToString;
      Exit;
    end;
    if not CheckMaxValue(MetaKey.FMaximumValue, Value) then
    begin
      Result.ErrorMessage := 'value is above '+MetaKey.FMaximumValue.ToString;
      Exit;
    end;
    Result.Success := true;
  except
    on E: Exception do
    Result.ErrorMessage := E.Message;
  end;
end;

procedure TdwlParams.Clear;
begin
  FParams.Clear;
  if Assigned(FPersistHook) then
    FPersistHook.Clear;
end;

procedure TdwlParams.ClearKey(const Key: string);
begin
  var LowerKey := Key.ToLower;
  if not ContainsKey(LowerKey) then
    Exit;
  FParams.Remove(LowerKey);
  if Assigned(FPersistHook) then
    FPersistHook.ClearKey(LowerKey);
  if Assigned(FChangeMethodCallbackProc) or Assigned(FChangeRegularCallbackProc) then
  begin
    var MetaKey: TdwlMetaKey;
    if (not FChangeCallBacksRestrictedToConfiguredMetaKeys) or (TryGetMetaKey(Key, MetaKey) and MetaKey.FChangeTracking) then
    begin
      if Assigned(FChangeMethodCallbackProc) then
        FChangeMethodCallbackProc(Self, Key, TValue.Empty);
      if Assigned(FChangeRegularCallbackProc) then
        FChangeRegularCallbackProc(Self, Key, TValue.Empty);
    end;
  end;
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
  var LowerKey := Key.ToLower;
  Result := FParams.ContainsKey(LowerKey);
  if (not Result) and (FPersistHook<>nil) then
  begin
    Result := FPersistHook.ContainsKey(LowerKey);
  end;
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

function TdwlParams.DoubleValue(const Key: string; Default: double=0): double;
begin
  if not TryGetDoubleValue(Key, Result) then
    Result := Default
end;

procedure TdwlParams.EnableChangeTracking(CallBackProc: TChangeMethodCallBackProc; FRestrictToConfiguredMetaKeys: boolean=false);
begin
  FChangeCallBacksRestrictedToConfiguredMetaKeys := FRestrictToConfiguredMetaKeys;
  FChangeMethodCallbackProc := CallBackProc;
end;

procedure TdwlParams.EnableChangeTracking(CallBackProc: TChangeRegularCallBackProc; FRestrictToConfiguredMetaKeys: boolean=false);
begin
  FChangeCallBacksRestrictedToConfiguredMetaKeys := FRestrictToConfiguredMetaKeys;
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
  if (FPersistHook<>nil) and (not FRequestedAllFromPersistHook) then
  begin
    var KnownKeys := FParams.Keys.ToArray;
    FPersistHook.ProvideUnknownKeys(KnownKeys);
    FRequestedAllFromPersistHook := true;
  end;
  Result := TdwlParamsEnumerator.Create(FParams.GetEnumerator);
end;

function TdwlParams.TryGetMetaKey(const LowerKey: string; out MetaKey: TdwlMetaKey): boolean;
begin
  Result := (FDomain<>'') and FMetaKeys.TryGetValue(DictKey(FDomain, LowerKey), MetaKey);
end;

function TdwlParams.Int64Value(const Key: string; Default: Int64=0): Int64;
begin
  if not TryGetInt64Value(Key, Result) then
    Result := Default
end;

function TdwlParams.IntValue(const Key: string; Default: integer=0): integer;
begin
  if not TryGetIntValue(Key, Result) then
    Result := Default
end;

function TdwlParams.MetaKeyConsulter(const Key: string): IdwlMetaKeyConsulter;
begin
  var LowerKey := Key.ToLower;
  var MetaKey: TdwlMetaKey;
  if not TryGetMetaKey(LowerKey, MetaKey) then
    MetaKey := nil;
  Result := TdwlMetakeyConsulter.Create(Key, MetaKey);
end;

procedure TdwlParams.ProvideKeyCallBack(const LowerKey: string; const Value: TValue);
begin
  FParams.Add(LowerKey, Value);
end;

procedure TdwlParams.PutIntoJSONObject(JSONObject: TJSONObject);
begin
  var Enumerator := GetEnumerator;
  while Enumerator.MoveNext do
  begin
    var Value := Enumerator.CurrentValue;
    case Value.Kind of
    tkChar,
    tkWChar,
    tkUString,
    tkWString,
    tkString: JSONObject.AddPair(Enumerator.CurrentKey, Value.ToString);
    tkInteger: JSONObject.AddPair(Enumerator.CurrentKey, Value.AsInteger);
    tkInt64: JSONObject.AddPair(Enumerator.CurrentKey, Value.AsInt64);
    tkFloat: JSONObject.AddPair(Enumerator.CurrentKey, Value.AsExtended);
    else
      raise Exception.Create('TValue: Unknown kind in PutIntoJSONObject');
    end;
  end;
end;

procedure TdwlParams.RegisterPersistHook(PersistHook: IdwlParamsPersistHook);
begin
  FPersistHook := PersistHook;
  FRequestedAllFromPersistHook := false;
  FPersistHook.Initialize(ProvideKeyCallBack);
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

function TdwlParams.StrValue(const Key: string; const Default: string=''): string;
begin
  if not TryGetStrValue(Key, Result) then
    Result := Default;
end;

function TdwlParams.TryGetBareValue(const Key: string; out Value: TValue): boolean;
begin
  var LowerKey := Key.ToLower;
  Result := FParams.TryGetValue(LowerKey, Value);
  if not Result then
    Result := TryGetFromPersistHook(LowerKey, Value);
end;

function TdwlParams.TryGetBoolValue(const Key: string; out Value: boolean): boolean;
begin
  var V: TValue;
  Result := TryGetValue(Key, V);
  if not Result then
    Exit;
  Result := V.TryAsType(Value, false);
  if not Result then
    Result := TryStrToBool(V.ToString, Value);
end;

function TdwlParams.TryGetByteValue(const Key: string; out Value: byte): boolean;
begin
  var V: TValue;
  Result := TryGetValue(Key, V);
  if not Result then
    Exit;
  Result := V.TryAsType(Value, false);
  if not Result then
  begin
    var C: cardinal;
    Result := TryStrToUInt(V.ToString, C) and (C<256);
    if Result then
      Value := C;
  end;
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

function TdwlParams.TryGetFromPersistHook(const LowerKey: string; var Value: TValue): boolean;
begin
  Result := Assigned(FPersistHook) and FPersistHook.TryGetValue(LowerKey, Value);
  if Result then
    FParams.Add(Lowerkey, Value);
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
begin
  var V: TValue;
  Result := TryGetValue(Key, V);
  if Result then
    Value := V.ToString;
end;

function TdwlParams.TryGetUnixEpochValue(const Key: string; out Value: TUnixEpoch): boolean;
begin
  var V: TValue;
  Result := TryGetValue(Key, V);
  if not Result then
    Exit;
  var IntVal: Int64;
  Result := V.TryAsType(IntVal, false);
  if Result then
    Value := IntVal;
end;

function TdwlParams.TryGetValue(const Key: string; out Value: TValue): boolean;
begin
  var LowerKey := Key.ToLower;
  Result := TryGetBareValue(LowerKey, Value);
  if not Result then
  begin // try to find a metakey and let the metakey provide a value
    var MetaKey: TdwlMetaKey;
    if TryGetMetaKey(LowerKey, MetaKey) then
    begin
      Result := MetaKey.TryCalculateValue(Self, LowerKey, Value);
      if (not Result) and (not MetaKey.FDefaultValue.IsEmpty) then
      begin
        Result := true;
        Value := MetaKey.FDefaultValue;
      end;
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

function TdwlParams.UnixEpochValue(const Key: string; const Default: Int64=0): TUnixEpoch;
begin
  if not TryGetUnixEpochValue(Key, Result) then
    Result := Default;
end;

procedure TdwlParams.UnRegisterPersistHook;
begin
  FPersistHook := nil;
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

procedure TdwlParams.WriteValue(const Key: string; const Value: TValue; SkipPersisting: boolean=false);
begin
  var Lowerkey := Key.ToLower;
  var OldValue: TValue;
  var MetaKey: TdwlMetaKey;
  var MetaKeyAvailable := TryGetMetaKey(LowerKey, MetaKey);
  var OldValueAvailable := TryGetBareValue(LowerKey, OldValue);
  if OldValueAvailable and Value.Equals(OldValue) then
    Exit;
  // do not write default value
  if MetaKeyAvailable and (not MetaKey.FWriteDefaultValue) and MetaKey.FDefaultValue.Equals(Value) then
  begin // don't write the default value, remove the current
    if OldValueAvailable then
    begin
      FParams.Remove(LowerKey);
      if Assigned(FPersistHook) then
        FPersistHook.ClearKey(LowerKey);
    end
    else
      Exit; // writing the default without old value is no change
  end
  else
  begin // write the value
    if MetaKeyAvailable then
      MetaKey.Check(Value);
    if Assigned(FPersistHook) and (not SkipPersisting) then
      FPersistHook.AddOrSetValue(LowerKey, Value);
    FParams.AddOrSetValue(Lowerkey, Value);
  end;
  if Assigned(FChangeMethodCallbackProc) or Assigned(FChangeRegularCallbackProc) then
  begin
    if (not FChangeCallBacksRestrictedToConfiguredMetaKeys) or (MetaKeyAvailable and MetaKey.FChangeTracking) then
    begin
      if Assigned(FChangeMethodCallbackProc) then
        FChangeMethodCallbackProc(Self, LowerKey, Value);
      if Assigned(FChangeRegularCallbackProc) then
        FChangeRegularCallbackProc(Self, LowerKey, Value);
    end;
  end;
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

procedure TdwlMetaKey.Check(Value: TValue);
begin
  // not yet implemented
end;

constructor TdwlMetaKey.Create(TypeInfo: PTypeInfo);
begin
  inherited Create;
  FTypeInfo := TypeInfo;
  FDefaultValue := TValue.Empty;
  FMinimumValue := TValue.Empty;
  FMaximumValue := TValue.Empty;
end;

destructor TdwlMetaKey.Destroy;
begin
  FCalculators.Free;
  inherited Destroy;
end;

procedure TdwlMetaKey.RegisterCalculateProc(CalcProc: TParamTryCalculateProc);
begin
  if FCalculators=nil then
    FCalculators := TList<TParamTryCalculateProc>.Create;
  FCalculators.Add(CalcProc);
end;

function TdwlMetaKey.TryCalculateValue(Params: IdwlParams; const LowerKey: string; var Value: TValue): boolean;
begin
  Result := false;
  if FCalculators=nil then
    Exit;
  var Idx := 0;
  while (not Result) and (Idx<FCalculators.Count) do
  begin
    Result := FCalculators[Idx](Params, LowerKey, Value);
    inc(Idx);
  end;
  if Result then // caching: write into the params
    Params.WriteValue(LowerKey, Value, true);
end;

{ TdwlMetaKeyBuilder }

function TdwlMetakeyBuilder.CalculateProc(CalculateProc: TParamTryCalculateProc): IdwlMetaKeyBuilder;
begin
  FMetaKey.RegisterCalculateProc(CalculateProc);
  Result := Self;
end;

function TdwlMetakeyBuilder.ChangeTracking(const ChangeTrackingEnabled: boolean): IdwlMetaKeyBuilder;
begin
  FMetaKey.FChangeTracking := ChangeTrackingEnabled;
  Result := Self;
end;

constructor TdwlMetakeyBuilder.Create(MetaKey: TdwlMetaKey);
begin
  FMetaKey := MetaKey;
end;

function TdwlMetaKeyBuilder.DefaultValue(DefaultValue: TValue): IdwlMetaKeyBuilder;
begin
  FMetaKey.FDefaultValue := DefaultValue.Cast(FMetaKey.FTypeInfo);
  Result := Self;
end;

function TdwlMetakeyBuilder.MaximumValue(MaximumValue: TValue): IdwlMetaKeyBuilder;
begin
  FMetaKey.FMaximumValue := MaximumValue.Cast(FMetaKey.FTypeInfo);
  Result := Self;
end;

function TdwlMetakeyBuilder.MinimumValue(MinimumValue: TValue): IdwlMetaKeyBuilder;
begin
  FMetaKey.FMinimumValue := MinimumValue.Cast(FMetaKey.FTypeInfo);
  Result := Self;
end;

function TdwlMetakeyBuilder.Prettyname(const PrettyName: string): IdwlMetaKeyBuilder;
begin
  FMetaKey.FPrettyName := PrettyName;
  Result := Self;
end;

function TdwlMetakeyBuilder.WriteDefaultValue: IdwlMetaKeyBuilder;
begin
  FMetaKey.FWriteDefaultValue := true;
  Result := Self;
end;

{ TdwlMetakeyConsulter }

constructor TdwlMetakeyConsulter.Create(const Key: string; MetaKey: TdwlMetaKey);
begin
  inherited Create;
  FKey := Key;
  FMetaKey := MetaKey;
end;

function TdwlMetakeyConsulter.DefaultValue: TValue;
begin
  if FMetaKey<>nil then
    Result := FMetaKey.FDefaultValue
  else
    Result := TValue.Empty;
end;

function TdwlMetakeyConsulter.MaximumValue: TValue;
begin
  if FMetaKey<>nil then
    Result := FMetaKey.FMaximumValue
  else
    Result := TValue.Empty;
end;

function TdwlMetakeyConsulter.MinimumValue: TValue;
begin
  if FMetaKey<>nil then
    Result := FMetaKey.FMinimumValue
  else
    Result := TValue.Empty;
end;

function TdwlMetakeyConsulter.Prettyname: string;
begin
  if FMetaKey<>nil then
    Result := FMetaKey.FPrettyName
  else
    Result := FKey;
end;

function TdwlMetakeyConsulter.TypeInfo: PTypeInfo;
begin
  if FMetaKey<>nil then
    Result := FMetaKey.FTypeInfo
  else
    Result := nil;
end;

end.
