unit DWL.MySQL;

interface

uses
  DWL.MySQL.API, System.Generics.Collections, System.Classes, DWL.Types,
  DWL.Params, DWL.Params.Consts;

const
  Params_SQLConnection: TArray<String> = [Param_Host, Param_Username, Param_Password,
    Param_Db, Param_CreateDatabase, Param_Port, Param_TestConnection];

type
  TdwlMySQLUseOutputBindingCallback = reference to procedure(const pBuffer: Pointer; const dwDataSize: cardinal; var MemoryOwnerShipTaken: boolean);

type
  TdwlMySQLResultField=class;

  /// <summary>
  ///   class used to bind data from MySQL Driver
  /// </summary>
  /// <remarks>
  ///   Do not create this class directly
  /// </remarks>
  TdwlMySQLDataBinding = class
  protected
    FIs_Null: ByteBool;
    procedure GetData_BlobRef(AStmt: PMYSQL_STMT; AIdx: Integer; cbUseBlob: TdwlMySQLUseOutputBindingCallback); virtual;
    function GetData_DateTime(AStmt: PMYSQL_STMT; AIdx: Integer): TDateTime; virtual;
    function GetData_Double(AStmt: PMYSQL_STMT; AIdx: Integer): double; virtual;
    function GetData_Integer(AStmt: PMYSQL_STMT; AIdx: Integer): integer; virtual;
    function GetData_Int64(AStmt: PMYSQL_STMT; AIdx: Integer): Int64; virtual;
    function GetData_String(AStmt: PMYSQL_STMT; AIdx: Integer): string; virtual;
    function GetDataSizeBuffer: PCardinal; virtual;
    function GetBuffer: Pointer; virtual;
    function GetBufferSize: cardinal; virtual;
    class function BufferType: Enum_Field_Types; virtual; abstract;
  end;

  /// <summary>
  ///   class to represent a MySQL Field in the results from the execution
  /// </summary>
  /// <remarks>
  ///   Do not create this class directly
  /// </remarks>
  TdwlMySQLResultField = class
  strict private
    FIdx: Integer;
    FField: PMySQL_Field;
    FBind: PMySQL_Bind;
    function GetName: string;
    function GetDataType: enum_field_types;
  private
    FBinding: TdwlMySQLDataBinding;
    function Gettable: string;
    function Getcharsetnr: LongWord;
    function Getflags: LongWord;
    function Getlength: LongWord;
    procedure CreateOutputBinding;
  public
    constructor Create(AIdx: Integer; AField: PMySQL_Field; ABinds: PMySQL_Bind);
    destructor Destroy; override;
    /// <summary>
    ///   zerobased Index of the field in the list
    /// </summary>
    property Idx: Integer read FIdx;
    /// <summary>
    ///   Name of column
    /// </summary>
    property Name: string read GetName;
    /// <summary>
    ///   Name of the field
    /// </summary>
    property DataType: enum_field_types read GetDataType;
    /// <summary>
    ///   Table of column if column was a field
    /// </summary>
    property table: string read Gettable;
    /// <summary>
    ///   Width of column
    /// </summary>
    property length: LongWord read Getlength;
    /// <summary>
    ///   Div flags (defined by MySQL Driver)
    /// </summary>
    property flags: LongWord read Getflags;
    /// <summary>
    ///   Character set used in the result
    /// </summary>
    property charsetnr: LongWord read Getcharsetnr;
  end;

  /// <summary>
  ///   Collection that bundles all bindings and holds it together with the
  ///   library-allocated PMYSQL_BIND block. <br />Use it to provide the values
  ///   of the parameters after the MySQl Command has been prepared <br />
  /// </summary>
  /// <remarks>
  ///   Do not create this class directly
  /// </remarks>
  TdwlMySQLDataBindingCollection = class
  strict private
    FBinds: PMySQL_Bind;
    FParams: TObjectList<TdwlMySQLDataBinding>;
    function GetCount: Integer;
    function GetInitialized: Boolean;
  private
    property Binds: PMySQL_Bind read FBinds;
    function SetParameter(AIdx: Integer; AParam: TdwlMySQLDataBinding): TdwlMySQLDataBinding;
  public
    constructor Create(AParamCount: Integer);
    destructor Destroy; override;
    /// <summary>
    ///   set the value to NULL
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    procedure SetNullDataBinding(AIdx: integer);
    /// <summary>
    ///   sets the value of the binding by providing an integer
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="AValue">
    ///   the value to set
    /// </param>
    procedure SetIntegerDataBinding(AIdx: Integer; AValue: Integer);
    /// <summary>
    ///   sets the value of the binding by providing a ref to an integer.
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="AValueBuffer">
    ///   the buffer containing the integer
    /// </param>
    /// <remarks>
    ///   This reference needs to be active until command is executed. Do NOT
    ///   free before
    /// </remarks>
    procedure SetIntegerRefDataBinding(AIdx: Integer; AValueBuffer: PInteger);
    /// <summary>
    ///   sets the value of the binding by providing a Big integer (64 bit)
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="AValue">
    ///   the value to set
    /// </param>
    procedure SetBigIntegerDataBinding(AIdx: Integer; AValue: Int64);
    /// <summary>
    ///   sets the value of the binding by providing a reference to a bit
    ///   integer (64 bit)
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="AValueBuffer">
    ///   the buffer containing the big integer
    /// </param>
    /// <remarks>
    ///   This reference needs to be active until command is executed. Do NOT
    ///   free before
    /// </remarks>
    procedure SetBigIntegerRefDataBinding(AIdx: Integer; AValueBuffer: PInt64);
    /// <summary>
    ///   sets the value of the binding by providing a datetime value
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="AValue">
    ///   the value to set
    /// </param>
    procedure SetDateTimeDataBinding(AIdx: Integer; AValue: TDateTime);
    /// <summary>
    ///   sets the value of the binding by providing a reference to a datetime
    ///   value
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="AValueBuffer">
    ///   the buffer containing the datetime
    /// </param>
    /// <remarks>
    ///   This reference needs to be active until command is executed. Do NOT
    ///   free before
    /// </remarks>
    procedure SetDateTimeRefDataBinding(AIdx: Integer; AValueBuffer: PDateTime);
    /// <summary>
    ///   sets the value of the binding by providing a double
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="AValue">
    ///   the value to set
    /// </param>
    procedure SetDoubleDataBinding(AIdx: Integer; AValue: Double);
    /// <summary>
    ///   sets the value of the binding by providing a reference to a dboule
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="AValueBuffer">
    ///   the buffer containing the double
    /// </param>
    /// <remarks>
    ///   This reference needs to be active until command is executed. Do NOT
    ///   free before
    /// </remarks>
    procedure SetDoubleRefDataBinding(AIdx: Integer; AValueBuffer: PDouble);
    /// <summary>
    ///   sets the value of the binding by providing a buffer
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="Buffer">
    ///   The buffer containing the data
    /// </param>
    /// <param name="BufferSize">
    ///   The size of the buffer
    /// </param>
    procedure SetBinaryDataBinding(AIdx: Integer; Buffer: PByte; BufferSize: cardinal); overload;
    /// <summary>
    ///   sets the value of the binding by providing a stream
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="DataStrm">
    ///   the stream that contains the data
    /// </param>
    procedure SetBinaryDataBinding(AIdx: Integer; DataStrm: TStream); overload;
    /// <summary>
    ///   sets the value of the binding by providing a reference to a buffer
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="Buffer">
    ///   The buffer containing the data
    /// </param>
    /// <param name="BufferSize">
    ///   The size of the buffer
    /// </param>
    /// <remarks>
    ///   This reference needs to be active until command is executed. Do NOT
    ///   free before
    /// </remarks>
    function SetBinaryRefDataBinding(AIdx: Integer; Buffer: PByte; BufferSize: cardinal): TdwlMySQLDataBinding;
    /// <summary>
    ///   sets the value of the binding by providing a string
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="AValue">
    ///   the value to set
    /// </param>
    procedure SetTextDataBinding(AIdx: Integer; const AValue: string);
    /// <summary>
    ///   sets the value of the binding by providing a UUID record
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="AValue">
    ///   the value to set
    /// </param>
    procedure SetUUIDDataBinding(AIdx: Integer; const AValue: TdwlUUID);
    /// <summary>
    ///   sets the value of the binding by providing a reference to a UUID
    ///   record
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <param name="AValue">
    ///   the value to set
    /// </param>
    /// <remarks>
    ///   This reference needs to be active until command is executed. Do NOT
    ///   free before
    /// </remarks>
    procedure SetUUIDRefDataBinding(AIdx: Integer; const AValue: TdwlUUID);
    /// <summary>
    ///   check if all databindings are initialized and ready for execution
    /// </summary>
    property Initialized: Boolean read GetInitialized;
    /// <summary>
    ///   The amount of bindings in the collections
    /// </summary>
    property Count: Integer read GetCount;
  end;

  /// <summary>
  ///   A reader to read the results from an executed command <br /><br />
  /// </summary>
  /// <remarks>
  ///   Do not create this class directly, you can find it in the
  ///   IdwlMySQLCommand interface <br />
  /// </remarks>
  TdwlMySQLDataReader = class
  strict private
    FBinds: PMySQL_Bind;
    FStmt: PMySQL_Stmt;
    FFields: TObjectList<TdwlMySQLResultField>;
    function GetRowsAffected: UInt64;
    function Bind(AIdx: cardinal): PMySQL_Bind;
    function ReturnDefaultOnNull<T>(AIdx: integer; NullReturnsDefault: boolean; Default: T; var RetVal: T): boolean;
  public
    constructor Create(AStmt: PMySQL_Stmt);
    destructor Destroy; override;
    function Read: Boolean;
    /// <summary>
    ///   Check if the field is NULL
    /// </summary>
    /// <param name="AIdx">
    ///   the index of the field
    /// </param>
    /// <returns>
    ///   indicating if the field value is NULL
    /// </returns>
    function IsDbNull(AIdx: Integer): Boolean;
    /// <summary>
    ///   Returns the integer value of a field. In the case the field is NULL
    ///   an error is thrown unless NullReturnsDefault is set to true and an
    ///   actual default is provided
    /// </summary>
    /// <param name="AIdx">
    ///   The index of the field for which the value is retrieved
    /// </param>
    /// <param name="NullReturnsDefault">
    ///   if false an exception will be trown in the case of NULL, if true the
    ///   Default will be returned
    /// </param>
    /// <param name="Default">
    ///   The default to be returned, set NullReturnsDefault to true to
    ///   effectivly use it.
    /// </param>
    function GetInteger(AIdx: Integer; NullReturnsDefault: boolean=false; Default: Integer=0): integer;
    /// <summary>
    ///   Returns the integer64 value of a field. In the case the field is NULL
    ///   an error is thrown unless NullReturnsDefault is set to true and an
    ///   actual default is provided
    /// </summary>
    /// <param name="AIdx">
    ///   The index of the field for which the value is retrieved
    /// </param>
    /// <param name="NullReturnsDefault">
    ///   if false an exception will be trown in the case of NULL, if true the
    ///   Default will be returned
    /// </param>
    /// <param name="Default">
    ///   The default to be returned, set NullReturnsDefault to true to
    ///   effectivly use it.
    /// </param>
    function GetInt64(AIdx: Integer; NullReturnsDefault: boolean=false; Default: Int64=0): Int64;
    /// <summary>
    ///   Returns the double value of a field. In the case the field is NULL an
    ///   error is thrown unless NullReturnsDefault is set to true and an
    ///   actual default is provided
    /// </summary>
    /// <param name="AIdx">
    ///   The index of the field for which the value is retrieved
    /// </param>
    /// <param name="NullReturnsDefault">
    ///   if false an exception will be trown in the case of NULL, if true the
    ///   Default will be returned
    /// </param>
    /// <param name="Default">
    ///   The default to be returned, set NullReturnsDefault to true to
    ///   effectivly use it.
    /// </param>
    function GetDouble(AIdx: Integer; NullReturnsDefault: boolean=false; Default: double=0): double;
    /// <summary>
    ///   Returns the datetime value of a field. In the case the field is NULL
    ///   an error is thrown unless NullReturnsDefault is set to true and an
    ///   actual default is provided
    /// </summary>
    /// <param name="AIdx">
    ///   The index of the field for which the value is retrieved
    /// </param>
    /// <param name="NullReturnsDefault">
    ///   if false an exception will be trown in the case of NULL, if true the
    ///   Default will be returned
    /// </param>
    /// <param name="Default">
    ///   The default to be returned, set NullReturnsDefault to true to
    ///   effectivly use it.
    /// </param>
    function GetDateTime(AIdx: integer; NullReturnsDefault: boolean=false; Default: TDateTime=0): TDateTime;
    /// <summary>
    ///   Returns the textual value of a field. In the case the field is NULL
    ///   an error is thrown unless NullReturnsDefault is set to true and an
    ///   actual default is provided
    /// </summary>
    /// <param name="AIdx">
    ///   The index of the field for which the value is retrieved
    /// </param>
    /// <param name="NullReturnsDefault">
    ///   if false an exception will be trown in the case of NULL, if true the
    ///   Default will be returned
    /// </param>
    /// <param name="Default">
    ///   The default to be returned, set NullReturnsDefault to true to
    ///   effectivly use it.
    /// </param>
    function GetString(AIdx: Integer; NullReturnsDefault: boolean=false; Default: string=''): string;
    /// <summary>
    ///   Returns the UUID value of a field. In the case the field is NULL an
    ///   error is thrown unless NullReturnsDefault is set to true and an
    ///   actual default is provided
    /// </summary>
    /// <param name="AIdx">
    ///   The index of the field for which the value is retrieved
    /// </param>
    /// <param name="NullReturnsDefault">
    ///   if false an exception will be trown in the case of NULL, if true the
    ///   Default will be returned
    /// </param>
    /// <param name="Default">
    ///   The default to be returned, set NullReturnsDefault to true to
    ///   effectivly use it.
    /// </param>
    function GetUUID(AIdx: Integer): TdwlUUID; // providing support for a default is useless here and inefficient
    /// <summary>
    ///   Calls a function where the Blob reference needs to be processed
    /// </summary>
    /// <param name="AIdx">
    ///   The index of the field for which the value is retrieved
    /// </param>
    /// <param name="cbUseBlob">
    ///   the callback function used to access the blob by reference
    /// </param>
    procedure GetBlobRef(AIdx: Integer; cbUseBlob: TdwlMySQLUseOutputBindingCallback);
    /// <summary>
    ///   The amount of rows affected by the execution of the query
    /// </summary>
    property RowsAffected: UInt64 read GetRowsAffected;
    /// <summary>
    ///   The description of the fields applicable for the result set.
    /// </summary>
    property Fields: TObjectList<TdwlMySQLResultField> read FFields;
  end;

  /// <summary>
  ///   an interface to a class representing a prepared MySQl Command
  /// </summary>
  IdwlMySQLCommand = interface
    /// <summary>
    ///   After provided the binding for the parameters, the command can be
    ///   executed
    /// </summary>
    /// <param name="AParamRebind">
    ///   Indicates that this is a secondary execution of the command and the
    ///   parameters need to be rebound
    /// </param>
    /// <param name="StoreResult">
    ///   set StoreResult to false to work with a live result, thus not fetch
    ///   all results immediately. This is efficient for large result sets, not
    ///   for small ones.
    /// </param>
    procedure Execute(AParamRebind: boolean=false; StoreResult: boolean=true);
    /// <summary>
    ///   The MySQL errorstring if execution failed
    /// </summary>
    function ErrorString: string;
    /// <summary>
    ///   The parameters that needs to be provided after a successful
    ///   preparation of the command. See the TdwlMySQLDataBinding
    ///   documentation for details
    /// </summary>
    /// <remarks>
    ///   if execution is done more than once without perparing a new command,
    ///   please indicate rebinding while calling Execute()
    /// </remarks>
    function Parameters: TdwlMySQLDataBindingCollection;
    /// <summary>
    ///   The Reader that can be used to fetch the results. See the
    ///   docementation of TdwlMySQLDataReader for more information
    /// </summary>
    function Reader: TdwlMySQLDataReader;
    /// <summary>
    ///   returns the ID of the last inserted record. Usefull f.e. for Autokey
    ///   Inserts
    /// </summary>
    function LastInsertID: UInt64;
  end;

  /// <summary>
  ///   an interface to a class representing a MySQL Session. This session is
  ///   representing the MySQL connection and can be used to prepare and
  ///   execute commands. Create a new connection by calling
  ///   New_MySQLSession();
  /// </summary>
  /// <remarks>
  ///   sessions are pooled in a connectionpool. Don't change database used in
  ///   the session, this will trigger assertions because it harms connection
  ///   re-use
  /// </remarks>
  IdwlMySQLSession = interface
    /// <summary>
    ///   Function to create a MySQL Command. The command will be prepared.
    ///   After succesfully preparing, the next step is binding the parameters
    ///   and executing the command. After execution you can use the
    ///   IfdlMySQLCommand.Reader to fetch the results
    /// </summary>
    /// <param name="Query">
    ///   The Queyr (including ?) to prepare for execution
    /// </param>
    function CreateCommand(const Query: string): IdwlMySQLCommand;
    /// <summary>
    ///   executeQuery can be used in circumstances where a command is not
    ///   applicable, results are not available.
    /// </summary>
    /// <param name="Query">
    ///   The query to be executed immediately without preparing
    /// </param>
    procedure ExecuteQuery(const Query: string);
  end;

/// <summary>
///   Creates a new IdwlMySQLSession, the basis for all connection based MySQL
///   Operations The library is thread safe, but the session (connection) must
///   be used in one thread only
/// </summary>
/// <param name="Params">
///   The key/value pairs definiing connection propertiesExpected keys:-
///   username- password- dbOptional key: <br />- host, default 127.0.0.1-
///   port, default 3306- createdatabase, default false- testconnection,
///   default false- usessl, default false <br />
/// </param>
function New_MySQLSession(Params: IdwlParams): IdwlMySQLSession; overload;
/// <summary>
///   Creates a new IdwlMySQLSession, the basis for all connection based MySQL
///   Operations The library is thread safe, but the session (connection) must
///   be used in one thread only. This is the 'flat' creating function,
///   preferably use the function based on Params.
/// </summary>
function New_MySQLSession(const Host, UserName, Password, Database: string; CreateDatabase: boolean=false; Port: word=3306; TestConnection: boolean=false): IdwlMySQLSession; overload;

implementation

uses
  Winapi.Windows, System.SyncObjs, DWL.MySQL.Utils, System.Math,
  System.SysUtils;

const
  TIMEOUT_CONNECTION = 300000; //msecs = 5 min

type
  TConnectionProperties = record
    Host: string;
    Port: word;
    UserName: string;
    Database: string;
    UseSSL: boolean;
    function UniqueIdentifier: string;
  end;

type
  TdwlMySQLSession=class;
  TdwlMySQLManager=class;

  TdwlMySQLConnection = class
  strict private
    FPort: cardinal;
    FHost: UTF8String;
    FDatabaseName: UTF8String;
    FUserName: UTF8String;
    FPassword: UTF8String;
    FCreateDatabase: Boolean;
    FUseSSL: boolean;
    procedure DoConnect;
    procedure DoDisConnect;
  private
    FMySQL: PMySQL;
    FBecamePassiveTick: UInt64;
    procedure MySqlChk(Res: longint);
    function PrepareStatement(const Query: UTF8String): PMYSQL_Stmt;
    function IsTimedOut: boolean;
  public
    constructor Create(const AHost: string; APort: cardinal; const ADatabaseName, AUserName, APassword: string; ACreateDatabase: boolean=true; AUseSSL: boolean=false);
    destructor Destroy; override;
    procedure Execute_Query(const Query: string; ProduceResultSet: boolean=true);
  end;

  TConnectionPool = class
  strict private
    FActiveConnections: TList<TdwlMySQLConnection>;
    FPassiveConnections: TList<TdwlMySQLConnection>;
    FPassword: string;
    FAccess: TCriticalSection;
  private
    FConnectionProperties: TConnectionProperties;
    constructor Create(const AConnectionProperties: TConnectionProperties; const APassword: string; CreateDatabase, TestConnection: boolean);
    procedure FreePassiveSessions(TimeOut, LastConnectionTimeout: cardinal);
    function GetConnection: TdwlMySQLConnection;
    procedure ReleaseConnection(Connection: TdwlMySQLConnection);
  public
    destructor Destroy; override;
  end;

  TdwlMySQLCommand = class(TInterfacedObject, IdwlMySQLCommand)
  strict private
    FConnection: TdwlMySQLConnection;
    FStmt: PMYSQL_Stmt;
    FParameters: TdwlMySQLDataBindingCollection;
    FReader: TdwlMySQLDataReader;
    FParamsBound: Boolean;
  private
    procedure Execute(AParamRebind: boolean=false; StoreResult: boolean=true);
    function ErrorString: string;
    function Reader: TdwlMySQLDataReader;
    function Parameters: TdwlMySQLDataBindingCollection;
    function LastInsertID: UInt64;
  public
    constructor Create(Connection: TdwlMySQLConnection; const Query: string);
    destructor Destroy; override;
  end;

  TdwlMySQLSession = class(TInterfacedObject, IdwlMySQLSession)
  strict private
    FConnectionPool: TConnectionPool;
    FConnection: TdwlMySQLConnection;
  private
    function CreateCommand(const Query: string): IdwlMySQLCommand;
    procedure ExecuteQuery(const Query: string);
  public
    constructor Create(AConnectionPool: TConnectionPool);
    destructor Destroy; override;
  end;

  TdwlMySQLManager = class
  private
    class var
      FAccess: TCriticalSection;
      FConnectionPools: TObjectDictionary<string, TConnectionPool>;
    class function CreateSession(Params: IdwlParams): IdwlMySQLSession;
  public
    class constructor Create;
    class destructor Destroy;
 end;

  { Class: TMySQLValueTypeDataBinding
    Description: Base class for implementing Value Type data bindings
  }
  TMySQLValueTypeDataBinding<T> = class(TdwlMySQLDataBinding)
  strict private
  protected
    FValue: T;
    function GetBuffer: Pointer; override;
    function GetBufferSize: cardinal; override;
  public
    constructor CreateInputInstance(AValue: T);
  end;

  TMySQLNullDataBinding = class(TdwlMySQLDataBinding)
   protected
    constructor CreateInputInstance;
    class function BufferType: Enum_Field_Types; override;
  end;

  TMySQLTinyIntDataBinding = class(TMySQLValueTypeDataBinding<ShortInt>)
  protected
    class function BufferType: Enum_Field_Types; override;
    function GetData_Integer(AStmt: PMYSQL_STMT; AIdx: Integer): integer; override;
  end;

  TMySQLSmallIntDataBinding = class(TMySQLValueTypeDataBinding<SmallInt>)
  protected
    class function BufferType: Enum_Field_Types; override;
    function GetData_Integer(AStmt: PMYSQL_STMT; AIdx: Integer): integer; override;
  end;

  TMySQLIntegerDataBinding = class(TMySQLValueTypeDataBinding<Integer>)
  protected
    class function BufferType: Enum_Field_Types; override;
    function GetData_Integer(AStmt: PMYSQL_STMT; AIdx: Integer): integer; override;
  end;

  TMySQLBigIntDataBinding = class(TMySQLValueTypeDataBinding<Int64>)
  protected
    class function BufferType: Enum_Field_Types; override;
    function GetData_Int64(AStmt: PMYSQL_STMT; AIdx: Integer): Int64; override;
  end;

  TMySQLFloatDataBinding = class(TMySQLValueTypeDataBinding<Single>)
  protected
    class function BufferType: Enum_Field_Types; override;
    function GetData_Double(AStmt: PMYSQL_STMT; AIdx: Integer): double; override;
  end;

  TMySQLDoubleDataBinding = class(TMySQLValueTypeDataBinding<Double>)
  protected
    class function BufferType: Enum_Field_Types; override;
    function GetData_Double(AStmt: PMYSQL_STMT; AIdx: Integer): double; override;
  end;

  TMySQLDateTimeDataBinding = class(TMySQLValueTypeDataBinding<TMYSQL_TIME>)
  protected
    class function BufferType: Enum_Field_Types; override;
    function GetData_DateTime(AStmt: PMYSQL_STMT; AIdx: Integer): TDateTime; override;
  public
    constructor CreateInputInstance(const AValue: TDateTime);
  end;

  {* Can target TEXT, CHAR, VARCHAR destination column types *}
  TMySQLTextDataBinding = class(TdwlMySQLDataBinding)
  protected
    FBufferNotOwned: Boolean;
    FDataSize: cardinal;
    FBuffer: pointer;
    FBufferSize: cardinal;
    function GetDataSizeBuffer: PCardinal; override;
    function GetBuffer: Pointer; override;
    function GetBufferSize: cardinal; override;
    class function BufferType: Enum_Field_Types; override;
    function GetData_String(AStmt: PMYSQL_STMT; AIdx: Integer): string; override;
  public
    constructor CreateInputInstance(const AString: string);
    destructor Destroy; override;
  end;

  {* Can target TINYBLOB, BLOB, MEDIUMBLOB, LONGBLOB, BINARY, VARBINARY destination column types *}
  TMySQLBlobDataBinding = class(TMySQLTextDataBinding)
  strict private
  protected
    class function BufferType: Enum_Field_Types; override;
    procedure GetData_BlobRef(AStmt: PMYSQL_STMT; AIdx: Integer; cbUseBlob: TdwlMySQLUseOutputBindingCallback); override;
  public
    constructor CreateInputInstance(const ADataStrm: TStream); overload;
    constructor CreateInputInstance(ABuffer: PByte; ABufferSize: cardinal; ABindByRef: Boolean); overload;
  end;

  TMySQLReferencedValueTypeDataBinding<Q; T: TMySQLValueTypeDataBinding<Q>> = class(TdwlMySQLDataBinding)
  strict private
    FValueBuffer: Pointer;
  protected
    function GetBuffer: Pointer; override;
    class function BufferType: Enum_Field_Types; override;
  public
    constructor CreateInputInstance(AValueBuffer: Pointer);
  end;

  TMySQLIntegerRefDataBinding = class(TMySQLReferencedValueTypeDataBinding<Integer, TMySQLIntegerDataBinding>)
  end;

  TMySQLBigIntRefDataBinding = class(TMySQLReferencedValueTypeDataBinding<Int64, TMySQLBigIntDataBinding>)
  end;

  TMySQLDoubleRefDataBinding = class(TMySQLReferencedValueTypeDataBinding<Double, TMySQLDoubleDataBinding>)
  end;

  TMySQLDateTimeRefDataBinding = class(TMySQLReferencedValueTypeDataBinding<TMYSQL_TIME, TMySQLDateTimeDataBinding>)
  end;

function New_MySQLSession(Params: IdwlParams): IdwlMySQLSession; overload;
begin
  Result := TdwlMySQLManager.CreateSession(Params);
end;

function New_MySQLSession(const Host, UserName, Password, Database: string; CreateDatabase: boolean=false; Port: word=3306; TestConnection: boolean=false): IdwlMySQLSession;
begin
  var Params := New_Params;
  Params.WriteValue(Param_Host, Host);
  Params.WriteValue(Param_Username, UserName);
  Params.WriteValue(Param_Password, Password);
  Params.WriteValue(Param_Db, Database);
  Params.WriteValue(Param_CreateDatabase, CreateDatabase);
  Params.WriteValue(Param_Port, Port);
  Params.WriteValue(Param_TestConnection, TestConnection);
  Result := New_MySQLSession(Params);
end;

{ TdwlMySQLConnection }

procedure TdwlMySQLConnection.DoConnect;
begin
  var ErrorMessage := '';
  var ClientFlag := 0;
  try
    FMySql := mysql_init(nil);
    if FUseSSL then
      ClientFlag := CLIENT_SSL;

    if FPort = 0 then
      FPort := 3306;

    var ConnectResult := mysql_real_connect(FMySql, PUTF8String(FHost), PUTF8String(FUserName), PUTF8String(FPassword), PUTF8String(FDatabaseName), FPort, nil, ClientFlag);
    if (ConnectResult<>FMySQL) and (mysql_errno(FMySql)=1049) and FCreateDatabase then
    begin
      DoDisConnect;
      FMySql := mysql_init(nil);
      ConnectResult := mysql_real_connect(FMySql, PUTF8String(FHost), PUTF8String(FUserName), PUTF8String(FPassword), nil, FPort, nil, ClientFlag);
      if ConnectResult=FMySQL then
      begin
        Execute_Query('CREATE DATABASE ' + TdwlMySQLUtils.BackTickIdentifier(UTF8ToString(FDatabaseName)), false);
        // and just reconnect for easyness, it will tell again if the database could not be created
        DoDisConnect;
        FMySql := mysql_init(nil);
        ConnectResult := mysql_real_connect(FMySql, PUTF8String(FHost), PUTF8String(FUserName), PUTF8String(FPassword), PUTF8String(FDatabaseName), FPort, nil, ClientFlag);
      end;
    end;
    if FMySQL=ConnectResult then
      MySQLChk(mysql_set_character_set(FMySQL, 'utf8'))
    else
      ErrorMessage := UTF8ToString(mysql_error( FMySQL));
  except
    on E: Exception do
    begin
      ErrorMessage := E.Message;
      try
        DoDisconnect;
      except
      end;
    end;
  end;
  if ErrorMessage<>'' then
    raise Exception.Create(ExtractFilename(ParamStr(0)) +  ' mysql connect did not succeeed (host:'+UTF8ToString(FHost)+', user:'+UTF8ToString(FUserName)+', db:'+UTF8ToString(FDatabaseName)+')'#13#10+ErrorMessage);
end;

constructor TdwlMySQLConnection.Create(const AHost: string; APort: cardinal; const ADatabaseName, AUserName, APassword: string; ACreateDatabase: boolean=true; AUseSSL: boolean=false);
begin
  inherited Create;
  FHost := UTF8String(AHost);
  FPort := APort;
  FDatabaseName := UTF8Encode(ADatabaseName);
  FUserName := UTF8Encode(AUserName);
  FPassword := UTF8Encode(APassword);
  FCreateDatabase := ACreateDatabase;
  FUseSSL := AUseSSL;
  DoConnect;
end;

destructor TdwlMySQLConnection.Destroy;
begin
  DoDisConnect;
  inherited Destroy;
end;

procedure TdwlMySQLConnection.DoDisConnect;
begin
  if FMySql<>nil then
    mysql_close(FMySQL);
  FMySQL := nil;
end;

procedure TdwlMySQLConnection.Execute_Query(const Query: string; ProduceResultSet: boolean=true);
begin
  try
    var Query_Utf8 := UTF8Encode(Query);
    var ExecResult := mysql_real_query(FMySQL, PUTF8String(Query_Utf8), Length(Query_Utf8));
    if ExecResult=CR_SERVER_GONE_ERROR then
    begin
      DoDisConnect;
      DoConnect;
      ExecResult := mysql_real_query(FMySQL, PUTF8String(Query_Utf8), Length(Query_Utf8));
    end;
    MySqlChk(Execresult);
  except
  end;
end;

function TdwlMySQLConnection.IsTimedOut: boolean;
begin
  Result := (GetTickCount64-FBecamePassiveTick)>=TIMEOUT_CONNECTION;
end;

procedure TdwlMySQLConnection.MySqlChk(Res: longint);
begin
  if Res<>0 then
    raise Exception.Create(UTF8ToString(mysql_error(FMySQL)));
end;

function TdwlMySQLConnection.PrepareStatement(const Query: UTF8String): PMYSQL_Stmt;
begin
  Result := mysql_stmt_init(FMySQL);
  if Result=nil then
    raise Exception.Create('Out Of Memory');
  var ExecResult := mysql_stmt_prepare(Result, PUTF8String(Query), Length(Query));
  // if server connection timed out, restore it....
  if (ExecResult<>0) and (mysql_stmt_errno(Result)=CR_SERVER_GONE_ERROR) then
  begin
    mysql_stmt_close(Result);
    DoDisconnect;
    DoConnect;
    Result := mysql_stmt_init(FMySQL);
    if Result=nil then
      raise Exception.Create('Out Of Memory');
    ExecResult := mysql_stmt_prepare(Result, PUTF8String(Query), Length(Query));
  end;
  if ExecResult<>0 then
    raise Exception.Create('Failed preparing query: ' +string(Query)+#13#10+string(mysql_stmt_error(Result)));
end;

{ TdwlMySQLCommand }

constructor TdwlMySQLCommand.Create(Connection: TdwlMySQLConnection; const Query: string);
begin
  inherited Create;
  FConnection := Connection;
  FStmt := FConnection.PrepareStatement(UTF8Encode(Query));
  var PrmCnt := mysql_stmt_param_count(FStmt);
  if PrmCnt>0 then
    FParameters := TdwlMySQLDataBindingCollection.Create(PrmCnt);
end;

destructor TdwlMySQLCommand.Destroy;
begin
  FReader.Free;
  if FStmt<>nil then
    mysql_stmt_close(FStmt);  { no need to call mysql_stmt_free_result before }
  FParameters.Free;
  inherited Destroy;
end;

function TdwlMySQLCommand.ErrorString: string;
begin
  try
    Result := UTF8ToString(mysql_error(FConnection.FMySQL));
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

procedure TdwlMySQLCommand.Execute(AParamRebind: boolean=false; StoreResult: boolean=true);
begin
  if (FParameters<>nil) and ((AParamRebind) or (not FParamsBound)) then
  begin
    if not FParameters.Initialized then
      raise Exception.Create('One or more parameter(s) have not been initialized.');
    mysql_stmt_bind_param(FStmt, FParameters.Binds);
    FParamsBound := True;
  end;
  if mysql_stmt_execute(FStmt) <> 0 then
    raise Exception.Create(string(mysql_stmt_error(FStmt)));
  if StoreResult then
    mysql_stmt_store_result(FStmt);
end;

function TdwlMySQLCommand.Parameters: TdwlMySQLDataBindingCollection;
begin
  Result := FParameters;
end;

function TdwlMySQLCommand.LastInsertID: UInt64;
begin
  Result := mysql_insert_id(FConnection.FMySql);
end;

function TdwlMySQLCommand.Reader: TdwlMySQLDataReader;
begin
  if FReader=nil then
    FReader := TdwlMySQLDataReader.Create(FStmt);
  Result := FReader;
end;

{ TdwlMySQLDataReader }

function TdwlMySQLDataReader.Bind(AIdx: cardinal): PMySQL_Bind;
begin
  Result := PMySQL_Bind(PByte(FBinds)+AIdx*SizeOf(TMySQL_Bind));
end;

constructor TdwlMySQLDataReader.Create(AStmt: PMySQL_Stmt);
begin
  FStmt := AStmt;
  FFields := TObjectList<TdwlMySQLResultField>.Create;
  var Metadata := mysql_stmt_result_metadata(AStmt);
  if Metadata<>nil then
  begin //alternatively test: mysql_stmt_field_count(FStmt) > 0
    var FldCnt := mysql_num_fields(Metadata);
    FBinds := mysql_bind_init(FldCnt);
    for var i := 0 to FldCnt-1 do
      FFields.Add(TdwlMySQLResultField.Create(i, mysql_fetch_field_direct(Metadata, i), FBinds));
    mysql_stmt_bind_result(FStmt, FBinds);
  end;
end;

destructor TdwlMySQLDataReader.Destroy;
begin
  FFields.Free;
  FreeMem(FBinds);
  inherited Destroy;
end;

function TdwlMySQLDataReader.Read: Boolean;
var
  ARes: integer;
begin
  ARes := mysql_stmt_fetch(FStmt);
  Result := (ARes=0) or (ARes=MYSQL_DATA_TRUNCATED);
end;

function TdwlMySQLDataReader.ReturnDefaultOnNull<T>(AIdx: integer; NullReturnsDefault: boolean; Default: T; var RetVal: T): boolean;
begin
  if IsDbNull(AIdx) then
  begin
    if NullReturnsDefault then
    begin
      RetVal := Default;
      Exit(true);
    end
    else
      raise Exception.Create('Column is NULL');
  end;
  Result := false;
end;

function TdwlMySQLDataReader.IsDbNull(AIdx: Integer): Boolean;
begin
  Result := Bind(AIdx).is_null^;
end;

function TdwlMySQLDataReader.GetDouble(AIdx: Integer; NullReturnsDefault: boolean; Default: double): double;
begin
  if not ReturnDefaultOnNull<double>(AIdx, NullReturnsDefault, Default, Result) then
    Result := FFields[AIdx].FBinding.GetData_Double(FStmt, AIdx);
end;

function TdwlMySQLDataReader.GetInt64(AIdx: Integer; NullReturnsDefault: boolean; Default: Int64): Int64;
begin
  if not ReturnDefaultOnNull<Int64>(AIdx, NullReturnsDefault, Default, Result) then
    Result := FFields[AIdx].FBinding.GetData_Int64(FStmt, AIdx);
end;

function TdwlMySQLDataReader.GetInteger(AIdx: Integer; NullReturnsDefault: boolean=false; Default: Integer=0): integer;
begin
  if not ReturnDefaultOnNull<integer>(AIdx, NullReturnsDefault, Default, Result) then
    Result := FFields[AIdx].FBinding.GetData_Integer(FStmt, AIdx);
end;

function TdwlMySQLDataReader.GetRowsAffected: UInt64;
begin
  Result := mysql_stmt_affected_rows(FStmt);
end;

function TdwlMySQLDataReader.GetDateTime(AIdx: integer; NullReturnsDefault: boolean=false; Default: TDateTime=0): TDateTime;
begin
  if not ReturnDefaultOnNull<TDateTime>(AIdx, NullReturnsDefault, Default, Result) then
    Result := FFields[AIdx].FBinding.GetData_DateTime(FStmt, AIdx);
end;

function TdwlMySQLDataReader.GetString(AIdx: Integer; NullReturnsDefault: boolean=false; Default: string=''): string;
begin
  if not ReturnDefaultOnNull<string>(AIdx, NullReturnsDefault, Default, Result) then
    Result := FFields[AIdx].FBinding.GetData_String(FStmt, AIdx);
end;

function TdwlMySQLDataReader.GetUUID(AIdx: Integer): TdwlUUID;
begin
  // In the base type we always do type conversion
  var DataSize: cardinal;
  var IsNull: ByteBool;
  var ABind:PMySQL_Bind;
  ABind := mysql_bind_init(1);
  try
    // first get length
    ABind.buffer_type := MYSQL_TYPE_LONG_BLOB;
    ABind.buffer := nil;
    ABind.buffer_length := 0;
    ABind.length := @DataSize;
    ABind.is_null := @IsNull;
    if mysql_stmt_fetch_column(FStmt, ABind, AIdx, 0)<>0 then
      raise Exception.Create('DB Error');
    if ABind.length^<>16 then
      raise Exception.Create('UUID Binaries must be 16 bytes long');
    ABind.buffer_length := ABind.length^;
    ABind.buffer := @Result;
    // and get result
    if mysql_stmt_fetch_column(FStmt, ABind, AIdx, 0)<>0 then
      raise Exception.Create('DB Error');
  finally
    FreeMem(ABind);
  end;
end;

procedure TdwlMySQLDataReader.GetBlobRef(AIdx: Integer; cbUseBlob: TdwlMySQLUseOutputBindingCallback);
begin
  if IsDbNull(AIdx) then
  begin
    var Dummy: boolean;
    cbUseBlob(nil, 0, Dummy)
  end
  else
    FFields[AIdx].FBinding.GetData_BlobRef(FStmt, AIdx, cbUseBlob);
end;

{ TdwlMySQLDataBinding }

function TdwlMySQLDataBinding.GetBuffer: Pointer;
begin
  Result := nil;
end;

function TdwlMySQLDataBinding.GetBufferSize: cardinal;
begin
  Result := 0;
end;

function TdwlMySQLDataBinding.GetDataSizeBuffer: PCardinal;
begin
  Result := nil;
end;

procedure TdwlMySQLDataBinding.GetData_BlobRef(AStmt: PMYSQL_STMT; AIdx: Integer; cbUseBlob: TdwlMySQLUseOutputBindingCallback);
begin
  // In the base type we always do type conversion
  var DataSize: cardinal;
  var IsNull: ByteBool;
  var ABind:PMySQL_Bind;
  ABind := mysql_bind_init(1);
  try
    // first get length
    ABind.buffer_type := MYSQL_TYPE_LONG_BLOB;
    ABind.buffer := nil;
    ABind.buffer_length := 0;
    ABind.length := @DataSize;
    ABind.is_null := @IsNull;
    if mysql_stmt_fetch_column(AStmt, ABind, AIdx, 0)<>0 then
      raise Exception.Create('DB Error');
    var MemoryOwnerShipTaken := false;
    try
      if ABind.length^>0 then
      begin
        // allocate buffer
        ABind.buffer_length := ABind.length^;
        GetMem(ABind.buffer,  ABind.buffer_length);
        // and get result
         if mysql_stmt_fetch_column(AStmt, ABind, AIdx, 0)<>0 then
            raise Exception.Create('DB Error');
      end;
      cbUseBlob(ABind.buffer, aBind.buffer_length, MemoryOwnerShipTaken);
    finally
      if not MemoryOwnerShipTaken then
        FreeMem(ABind.buffer);
    end;
  finally
    FreeMem(ABind);
  end;
end;

function TdwlMySQLDataBinding.GetData_DateTime(AStmt: PMYSQL_STMT; AIdx: Integer): TDateTime;
begin
  // In the base type we always do type conversion
  var IsNull: ByteBool;
  var ABind: PMySQL_Bind;
  var DateTimeBuffer: TMYSQL_TIME;
  ABind := mysql_bind_init(1);
  try
    ABind.buffer_type := MYSQL_TYPE_TIME;
    ABind.buffer := @DateTimeBuffer;
    ABind.is_null := @IsNull;
    if mysql_stmt_fetch_column(AStmt, ABind, AIdx, 0)<>0 then
      raise Exception.Create('DB Error');
  finally
    FreeMem(ABind);
  end;
  Result := mysql_time_to_datetime(@DateTimeBuffer);
end;

function TdwlMySQLDataBinding.GetData_Double(AStmt: PMYSQL_STMT; AIdx: Integer): double;
begin
  // In the base type we always do type conversion
  var IsNull: ByteBool;
  var ABind:PMySQL_Bind;
  ABind := mysql_bind_init(1);
  try
    // first get length
    ABind.buffer_type := MYSQL_TYPE_DOUBLE;
    ABind.buffer := @Result;
    ABind.is_null := @IsNull;
    if mysql_stmt_fetch_column(AStmt, ABind, AIdx, 0)<>0 then
      raise Exception.Create('DB Error');
  finally
    FreeMem(ABind);
  end;
end;

function TdwlMySQLDataBinding.GetData_Int64(AStmt: PMYSQL_STMT; AIdx: Integer): Int64;
begin
  // In the base type we always do type conversion
  var IsNull: ByteBool;
  var ABind:PMySQL_Bind;
  ABind := mysql_bind_init(1);
  try
    // first get length
    ABind.buffer_type := MYSQL_TYPE_LONGLONG;
    ABind.buffer := @Result;
    ABind.is_null := @IsNull;
    if mysql_stmt_fetch_column(AStmt, ABind, AIdx, 0)<>0 then
      raise Exception.Create('DB Error');
  finally
    FreeMem(ABind);
  end;
end;

function TdwlMySQLDataBinding.GetData_Integer(AStmt: PMYSQL_STMT; AIdx: Integer): integer;
begin
  // In the base type we always do type conversion
  var IsNull: ByteBool;
  var ABind:PMySQL_Bind;
  ABind := mysql_bind_init(1);
  try
    // first get length
    ABind.buffer_type := MYSQL_TYPE_LONG;
    ABind.buffer := @Result;
    ABind.is_null := @IsNull;
    if mysql_stmt_fetch_column(AStmt, ABind, AIdx, 0)<>0 then
      raise Exception.Create('DB Error');
  finally
    FreeMem(ABind);
  end;
end;

function TdwlMySQLDataBinding.GetData_String(AStmt: PMYSQL_STMT; AIdx: Integer): string;
begin
  // In the base type we always do type conversion
  var DataSize: cardinal;
  var IsNull: ByteBool;
  var ABind:PMySQL_Bind;
  ABind := mysql_bind_init(1);
  try
    // first get length
    ABind.buffer_type := MYSQL_TYPE_STRING;
    ABind.buffer := nil;
    ABind.buffer_length := 0;
    ABind.length := @DataSize;
    ABind.is_null := @IsNull;
    if mysql_stmt_fetch_column(AStmt, ABind, AIdx, 0)<>0 then
      raise Exception.Create('DB Error');
    if ABind.length^=0 then
      Result := ''
    else
    begin
      // allocate buffer
      ABind.buffer_length := ABind.length^+1;
      ABind.buffer := AllocMem(ABind.buffer_length);
      // and get result
      try
       if mysql_stmt_fetch_column(AStmt, ABind, AIdx, 0)<>0 then
          raise Exception.Create('DB Error');
        // Buffer includes null terminator, so take care here below!
        SetLength(Result, ABind.length^);
        SetLength(Result, MultiByteToWideChar(CP_UTF8, 0, ABind.buffer, ABind.length^, @Result[1], ABind.length^));
      finally
        FreeMem(ABind.buffer);
      end;
    end;
  finally
    FreeMem(ABind);
  end;
end;

{ TdwlMySQLResultField }

constructor TdwlMySQLResultField.Create(AIdx: Integer; AField: PMYSQL_FIELD; ABinds: PMySQL_Bind);
begin
  inherited Create;
  FIdx := AIdx;
  FField := AField;
  FBind := ABinds;
  inc(FBind, AIdx);
  FBind.buffer_type := FField._type;
  CreateOutputBinding;
end;

procedure TdwlMySQLResultField.CreateOutputBinding;
begin
  case FField._type of
    MYSQL_TYPE_TINY: FBinding := TMySQLTinyIntDataBinding.Create;
    MYSQL_TYPE_SHORT: FBinding := TMySQLSmallIntDataBinding.Create;
    MYSQL_TYPE_INT24,
    MYSQL_TYPE_LONG: FBinding := TMySQLIntegerDataBinding.Create;
    MYSQL_TYPE_LONGLONG: FBinding := TMySQLBigIntDataBinding.Create;
    MYSQL_TYPE_FLOAT: FBinding := TMySQLFloatDataBinding.Create;
    MYSQL_TYPE_DOUBLE: FBinding := TMySQLDoubleDataBinding.Create;
    MYSQL_TYPE_NULL: FBinding := TMySQLNullDataBinding.Create;
    MYSQL_TYPE_NEWDECIMAL: FBinding := TMySQLTextDataBinding.Create;
    MYSQL_TYPE_DATE,
    MYSQL_TYPE_TIME,
    MYSQL_TYPE_DATETIME,
    MYSQL_TYPE_TIMESTAMP: FBinding := TMySQLDateTimeDataBinding.Create;
    MYSQL_TYPE_VAR_STRING,
    MYSQL_TYPE_STRING: FBinding := TMySQLTextDataBinding.Create;
    MYSQL_TYPE_TINY_BLOB,
    MYSQL_TYPE_MEDIUM_BLOB,
    MYSQL_TYPE_LONG_BLOB,
    MYSQL_TYPE_BLOB: FBinding := TMySQLBlobDataBinding.Create;
    else
      raise Exception.Create('Cannot create output binding for type id: ' + integer(FField._type).ToString);
  end;
  FBind.buffer := FBinding.GetBuffer;
  FBind.buffer_length := FBinding.GetBufferSize;
  FBind.length := FBinding.GetDataSizeBuffer;
  FBind.is_null := @(FBinding.FIs_Null);
end;

destructor TdwlMySQLResultField.Destroy;
begin
  FreeAndNil(FBinding);
  inherited;
end;

function TdwlMySQLResultField.Getcharsetnr: LongWord;
begin
  Result := FField.charsetnr;
end;

function TdwlMySQLResultField.GetDataType: enum_field_types;
begin
  Result := FField._type;
end;

function TdwlMySQLResultField.Getflags: LongWord;
begin
  Result := FField.flags;
end;

function TdwlMySQLResultField.Getlength: LongWord;
begin
  Result := FField.length;
end;

function TdwlMySQLResultField.GetName: string;
begin
  Result := UTF8ToString(FField.name);
end;

function TdwlMySQLResultField.Gettable: string;
begin
  Result := UTF8ToString(FField.table);
end;

{ TdwlMySQLDataBindingCollection }

constructor TdwlMySQLDataBindingCollection.Create(AParamCount: Integer);
begin
  FParams := TObjectList<TdwlMySQLDataBinding>.Create;
  for var i := 1 to AParamCount do
    FParams.Add(nil);
  FBinds := mysql_bind_init(AParamCount)
end;

destructor TdwlMySQLDataBindingCollection.Destroy;
begin
  FParams.Free;
  FreeMem(FBinds);
  inherited Destroy;
end;

procedure TdwlMySQLDataBindingCollection.SetIntegerDataBinding(AIdx: Integer; AValue: Integer);
begin
  SetParameter(AIdx, TMySQLIntegerDataBinding.CreateInputInstance(AValue));
end;

procedure TdwlMySQLDataBindingCollection.SetIntegerRefDataBinding(AIdx: Integer; AValueBuffer: PInteger);
begin
  SetParameter(AIdx, TMySQLIntegerRefDataBinding.CreateInputInstance(AValueBuffer));
end;

procedure TdwlMySQLDataBindingCollection.SetNullDataBinding(AIdx: integer);
begin
  SetParameter(AIdx, TMySQLNullDataBinding.Create);
end;

procedure TdwlMySQLDataBindingCollection.SetBigIntegerDataBinding(AIdx: Integer; AValue: Int64);
begin
  SetParameter(AIdx, TMySQLBigIntDataBinding.CreateInputInstance(AValue));
end;

procedure TdwlMySQLDataBindingCollection.SetBigIntegerRefDataBinding(AIdx: Integer; AValueBuffer: PInt64);
begin
  SetParameter(AIdx, TMySQLBigIntRefDataBinding.CreateInputInstance(AValueBuffer));
end;

procedure TdwlMySQLDataBindingCollection.SetDateTimeDataBinding(AIdx: Integer; AValue: TDateTime);
begin
  SetParameter(AIdx, TMySQLDateTimeDataBinding.CreateInputInstance(AValue));
end;

procedure TdwlMySQLDataBindingCollection.SetDateTimeRefDataBinding(AIdx: Integer; AValueBuffer: PDateTime);
begin
  SetParameter(AIdx, TMySQLDateTimeRefDataBinding.CreateInputInstance(AValueBuffer));
end;

procedure TdwlMySQLDataBindingCollection.SetDoubleDataBinding(AIdx: Integer; AValue: Double);
begin
  SetParameter(AIdx, TMySQLDoubleDataBinding.CreateInputInstance(AValue));
end;

procedure TdwlMySQLDataBindingCollection.SetDoubleRefDataBinding(AIdx: Integer; AValueBuffer: PDouble);
begin
  SetParameter(AIdx, TMySQLDoubleRefDataBinding.CreateInputInstance(AValueBuffer));
end;

procedure TdwlMySQLDataBindingCollection.SetBinaryDataBinding(AIdx: Integer; Buffer: PByte; BufferSize: cardinal);
begin
  SetParameter(AIdx, TMySQLBlobDataBinding.CreateInputInstance(Buffer, BufferSize, false));
end;

procedure TdwlMySQLDataBindingCollection.SetBinaryDataBinding(AIdx: Integer; DataStrm: TStream);
begin
  SetParameter(AIdx, TMySQLBlobDataBinding.CreateInputInstance(DataStrm));
end;

function TdwlMySQLDataBindingCollection.SetBinaryRefDataBinding(AIdx: Integer; Buffer: PByte; BufferSize: cardinal): TdwlMySQLDataBinding;
begin
  Result := SetParameter(AIdx, TMySQLBlobDataBinding.CreateInputInstance(Buffer, BufferSize, True));
end;

procedure TdwlMySQLDataBindingCollection.SetTextDataBinding(AIdx: Integer; const AValue: string);
begin
  SetParameter(AIdx, TMySQLTextDataBinding.CreateInputInstance(AValue));
end;

procedure TdwlMySQLDataBindingCollection.SetUUIDDataBinding(AIdx: Integer; const AValue: TdwlUUID);
begin
  SetParameter(AIdx, TMySQLBlobDataBinding.CreateInputInstance(@AValue.data, 16, false));
end;

procedure TdwlMySQLDataBindingCollection.SetUUIDRefDataBinding(AIdx: Integer; const AValue: TdwlUUID);
begin
  SetParameter(AIdx, TMySQLBlobDataBinding.CreateInputInstance(@AValue.data, 16, true));
end;

function TdwlMySQLDataBindingCollection.SetParameter(AIdx: Integer; AParam: TdwlMySQLDataBinding): TdwlMySQLDataBinding;
begin
  FParams[AIdx] := AParam;
  var ABind := FBinds;
  inc(ABind, AIdx);
  ABind.buffer_type := AParam.BufferType;
  ABind.buffer := AParam.GetBuffer;
  ABind.buffer_length := AParam.GetBufferSize;
  ABind.length := AParam.GetDataSizeBuffer;
  ABind.is_null := @(AParam.FIs_Null);
  Result := AParam;
end;

function TdwlMySQLDataBindingCollection.GetCount: Integer;
begin
  if FParams=nil then
    Result := 0
  else
    Result := FParams.Count;
end;

function TdwlMySQLDataBindingCollection.GetInitialized: Boolean;
begin
  Result := false;
  for var Param in FParams do
  begin
    if Param=nil then
      Exit;
  end;
  Result := true;
end;

{ TMySQLValueTypeDataBinding }

function TMySQLValueTypeDataBinding<T>.GetBuffer: Pointer;
begin
  Result := @FValue;
end;

function TMySQLValueTypeDataBinding<T>.GetBufferSize: cardinal;
begin
  Result := SizeOf(T);
end;

constructor TMySQLValueTypeDataBinding<T>.CreateInputInstance(AValue: T);
begin
  FValue := AValue;
end;

{ TMySQLReferencedValueTypeDataBinding<Q,T> }

class function TMySQLReferencedValueTypeDataBinding<Q,T>.BufferType: Enum_Field_Types;
begin
  Result := T.BufferType;
end;

function TMySQLReferencedValueTypeDataBinding<Q,T>.GetBuffer: Pointer;
begin
  Result := Pointer(FValueBuffer);
end;

constructor TMySQLReferencedValueTypeDataBinding<Q,T>.CreateInputInstance(AValueBuffer: Pointer);
begin
  FValueBuffer := AValueBuffer;
end;

{ TMySQLTinyIntDataBinding }

class function TMySQLTinyIntDataBinding.BufferType: Enum_Field_Types;
begin
  Result := MYSQL_TYPE_TINY;
end;

function TMySQLTinyIntDataBinding.GetData_Integer(AStmt: PMYSQL_STMT;
  AIdx: Integer): integer;
begin
  Result := FValue;
end;

{ TMySQLSmallIntDataBinding }

class function TMySQLSmallIntDataBinding.BufferType: Enum_Field_Types;
begin
  Result := MYSQL_TYPE_SHORT;
end;

function TMySQLSmallIntDataBinding.GetData_Integer(AStmt: PMYSQL_STMT;
  AIdx: Integer): integer;
begin
  Result := FValue;
end;

{ TMySQLIntegerDataBinding }

class function TMySQLIntegerDataBinding.BufferType: Enum_Field_Types;
begin
  Result := MYSQL_TYPE_LONG;
end;

function TMySQLIntegerDataBinding.GetData_Integer(AStmt: PMYSQL_STMT;
  AIdx: Integer): integer;
begin
  Result := FValue;
end;

{ TMySQLBigIntDataBinding }

class function TMySQLBigIntDataBinding.BufferType: Enum_Field_Types;
begin
  Result := MYSQL_TYPE_LONGLONG;
end;

function TMySQLBigIntDataBinding.GetData_Int64(AStmt: PMYSQL_STMT; AIdx: Integer): Int64;
begin
  Result := FValue;
end;

{ TMySQLFloatDataBinding }

class function TMySQLFloatDataBinding.BufferType: Enum_Field_Types;
begin
  Result := MYSQL_TYPE_FLOAT;
end;

function TMySQLFloatDataBinding.GetData_Double(AStmt: PMYSQL_STMT;
  AIdx: Integer): double;
begin
  Result := FValue;
end;

{ TMySQLDoubleDataBinding }

class function TMySQLDoubleDataBinding.BufferType: Enum_Field_Types;
begin
  Result := MYSQL_TYPE_DOUBLE;
end;

function TMySQLDoubleDataBinding.GetData_Double(AStmt: PMYSQL_STMT;
  AIdx: Integer): double;
begin
  Result := FValue;
end;

{ TMySQLDateTimeDataBinding }

constructor TMySQLDateTimeDataBinding.CreateInputInstance(const AValue: TDateTime);
begin
  datetime_to_mysql_time(AValue, MYSQL_TIMESTAMP_DATETIME, @FValue)
end;

function TMySQLDateTimeDataBinding.GetData_DateTime(AStmt: PMYSQL_STMT; AIdx: Integer): TDateTime;
begin
  Result := mysql_time_to_datetime(@FValue);
end;

class function TMySQLDateTimeDataBinding.BufferType: Enum_Field_Types;
begin
  Result := MYSQL_TYPE_DATETIME;
end;

{ TMySQLTextDataBinding }

function TMySQLTextDataBinding.GetBuffer: Pointer;
begin
  Result := FBuffer;
end;

function TMySQLTextDataBinding.GetBufferSize: cardinal;
begin
  Result := FBufferSize;
end;

function TMySQLTextDataBinding.GetDataSizeBuffer: PCardinal;
begin
  Result := @FDataSize;
end;

function TMySQLTextDataBinding.GetData_String(AStmt: PMYSQL_STMT; AIdx: Integer): string;
begin
  if FDataSize=0 then
    Exit('');
  var ABind: PMySQL_Bind;
  ABind := mysql_bind_init(1);
  try
    ABind.buffer_type := MYSQL_TYPE_STRING;
    ABind.buffer_length := FDataSize+1;
    ABind.buffer := AllocMem(ABind.buffer_length);
    // and get result
    try
     if mysql_stmt_fetch_column(AStmt, ABind, AIdx, 0)<>0 then
        raise Exception.Create('DB Error');
      // Buffer includes null terminator, so take care here below!
      SetLength(Result, ABind.length^);
      SetLength(Result, MultiByteToWideChar(CP_UTF8, 0, ABind.buffer, ABind.length^, @Result[1], ABind.length^));
    finally
      FreeMem(ABind.buffer);
    end;
  finally
    FreeMem(ABind);
  end;
end;

constructor TMySQLTextDataBinding.CreateInputInstance(const AString: string);
begin
  if AString<>'' then
  begin
    FDataSize := WideCharToMultiByte(CP_UTF8, 0, @AString[1], AString.Length, nil, 0, nil, nil);
    FBufferSize := FDataSize+1;
    FBuffer := AllocMem(FBufferSize);
    WideCharToMultiByte(CP_UTF8, 0, @AString[1], AString.Length, FBuffer, FBufferSize, nil, nil);
    PByte(PByte(FBuffer)+FDataSize)^:= 0; // set null-terminator
  end
end;

destructor TMySQLTextDataBinding.Destroy;
begin
  if (not FBufferNotOwned) then
    FreeMem(FBuffer);
  inherited Destroy;
end;

class function TMySQLTextDataBinding.BufferType: Enum_Field_Types;
begin
  Result := MYSQL_TYPE_STRING;
end;

{ TMySQLBlobDataBinding }

constructor TMySQLBlobDataBinding.CreateInputInstance(const ADataStrm: TStream);
begin
  FDataSize := ADataStrm.Size - ADataStrm.Position;
  FBuffer := AllocMem(FDataSize);
  FBufferSize := FDataSize;
  ADataStrm.ReadData(FBuffer, FDataSize);
end;

constructor TMySQLBlobDataBinding.CreateInputInstance(ABuffer: PByte; ABufferSize: cardinal; ABindByRef: Boolean);
begin
  FDataSize := ABufferSize;
  if ABindByRef then
    FBuffer := ABuffer
  else
  begin
    FBuffer := AllocMem(ABufferSize);
    Move(ABuffer^, FBuffer^, ABufferSize);
  end;
  FBufferNotOwned := ABindByRef;
  FBufferSize := ABufferSize;
end;

procedure TMySQLBlobDataBinding.GetData_BlobRef(AStmt: PMYSQL_STMT; AIdx: Integer; cbUseBlob: TdwlMySQLUseOutputBindingCallback);
begin
  var ABind: PMySQL_Bind;
  ABind := mysql_bind_init(1);
  try
    ABind.buffer_type := MYSQL_TYPE_LONG_BLOB;
    var MemoryOwnerShipTaken := false;
    try
      if FDataSize>0 then
      begin
        // allocate buffer
        ABind.buffer_length := FDataSize;
        GetMem(ABind.buffer, FDataSize);
        // and get result
         if mysql_stmt_fetch_column(AStmt, ABind, AIdx, 0)<>0 then
            raise Exception.Create('DB Error');
      end;
      cbUseBlob(ABind.buffer, aBind.buffer_length, MemoryOwnerShipTaken);
    finally
      if not MemoryOwnerShipTaken then
        FreeMem(ABind.buffer);
    end;
  finally
    FreeMem(ABind);
  end;
end;

class function TMySQLBlobDataBinding.BufferType: Enum_Field_Types;
begin
  Result := MYSQL_TYPE_BLOB;
end;

{ TdwlMySQLManager }

// Connection pool cleaning is not implemented yet
// The expectation is that the amount of connection pools will be very low
// but if you want to make the manager perfect: go and do your job

class constructor TdwlMySQLManager.Create;
begin
  FAccess := TCriticalSection.Create;
  FConnectionPools := TObjectDictionary<string, TConnectionPool>.Create([doOwnsValues]);
end;

class function TdwlMySQLManager.CreateSession(Params: IdwlParams): IdwlMySQLSession;
begin
  var TestConnection := Params.BoolValue(Param_TestConnection, ParamDef_TestConnection);
  var CreateDatabase := Params.BoolValue(Param_CreateDatabase, ParamDef_CreateDatabase);
  Assert(TestConnection or (not CreateDatabase), 'Creating of databases without a test connection is not possible');
  var ConnectionProperties: TConnectionProperties;
  ConnectionProperties.Host := Params.StrValue(Param_Host, ParamDef_Host);
  ConnectionProperties.Port := Params.IntValue(Param_Port, ParamDef_Port);
  ConnectionProperties.UserName := Params.StrValue(Param_Username);
  ConnectionProperties.Database := Params.StrValue(Param_Db);
  ConnectionProperties.UseSSL := Params.BoolValue(Param_UseSSL, ParamDef_UseSSL);
  var ConnectionPool: TConnectionPool;
  FAccess.Enter;
  try
    if not FConnectionPools.TryGetValue(ConnectionProperties.UniqueIdentifier, ConnectionPool) then
      ConnectionPool := TConnectionPool.Create(ConnectionProperties, Params.StrValue(Param_Password), CreateDataBase, TestConnection);
  finally
    FAccess.Leave;
  end;
  Result := TdwlMySQLSession.Create(ConnectionPool);
end;

class destructor TdwlMySQLManager.Destroy;
begin
  FConnectionPools.Free;
  FAccess.Free;
end;

{ TdwlConnectionPool }

constructor TConnectionPool.Create(const AConnectionProperties: TConnectionProperties; const APassword: string; CreateDatabase, TestConnection: boolean);
begin
  inherited Create;
  FActiveConnections := TList<TdwlMySQLConnection>.Create;
  FPassiveConnections := TList<TdwlMySQLConnection>.Create;
  FAccess := TCriticalSection.Create;
  FConnectionProperties := AConnectionProperties;
  FPassword := APassword;

  if TestConnection then
  begin
    var Conn := nil;
    try
      Conn := TdwlMySQLConnection.Create(FConnectionProperties.Host, FConnectionProperties.Port, FConnectionProperties.Database, FConnectionProperties.UserName, FPassword, CreateDatabase, FConnectionProperties.UseSSL);
    finally
      if Conn<>nil then
       ReleaseConnection(Conn);
    end;
  end;

  // no need to do thread protection here
  // this create is only called from within a critical section in TdwlMySQLManager.CreateSession
  TdwlMySQLManager.FConnectionPools.Add(FConnectionProperties.UniqueIdentifier, Self);
end;

destructor TConnectionPool.Destroy;
begin
  FreePassiveSessions(0, 0);
  FActiveConnections.Free;
  FPassiveConnections.Free;
  FAccess.Free;
  inherited Destroy;
end;

procedure TConnectionPool.FreePassiveSessions(TimeOut, LastConnectionTimeout: cardinal);
begin
  FAccess.Enter;
  try
    var TickNow := GetTickCount64;
    for var i := FPassiveConnections.Count-1 downto 0 do
    begin
      var Conn := FPassiveConnections[i];
      if (TickNow-Conn.FBecamePassiveTick)>=IfThen(FPassiveConnections.Count=1, LastConnectionTimeout, Timeout) then
      begin
        FPassiveConnections.Delete(i);
        Conn.Free;
      end;
    end;
  finally
    FAccess.Leave;
  end;
end;

function TConnectionPool.GetConnection: TdwlMySQLConnection;
begin
   Result := nil;
  // try to get a passive one
  while Result=nil do
  begin
    FAccess.Enter;
    try
      if FPassiveConnections.Count=0 then
        Break;
      Result := FPassiveConnections[0];
      FPassiveConnections.Delete(0);
    finally
      FAccess.Leave;
    end;
    if Result.IsTimedOut then
      FreeAndNil(Result);
  end;
  // feed result (and create one if needed)
  FAccess.Enter;
  try
    if Result=nil then
      Result := TdwlMySQLConnection.Create(FConnectionProperties.Host, FConnectionProperties.Port, FConnectionProperties.Database, FConnectionProperties.UserName, FPassword, false);
    FActiveConnections.Add(Result);
  finally
    FAccess.Leave;
  end;
end;

procedure TConnectionPool.ReleaseConnection(Connection: TdwlMySQLConnection);
begin
  Connection.FBecamePassiveTick := GetTickCount64;
  FAccess.Enter;
  try
    FActiveConnections.Remove(Connection);
    FPassiveConnections.Add(Connection);
  finally
    FAccess.Leave;
  end;
end;

{ TdwlMySQLSession }

constructor TdwlMySQLSession.Create(AConnectionPool: TConnectionPool);
begin
  inherited Create;
  FConnectionPool := AConnectionPool;
  FConnection := FConnectionPool.GetConnection;
end;

function TdwlMySQLSession.CreateCommand(const Query: string): IdwlMySQLCommand;
begin
  // The following check is to prevent database changing in connections from the pool
  // This would raise issues when the connection is re-used and the originally
  // selected database went out of scope.
  // It's an assertion, so released functionality goes without this check
  Assert((FConnectionPool.FConnectionProperties.Database='')  or (Pos('USE ', Query.ToUpper)=0), 'USE <database> is not allowed when database is provided on session initiate');
  Result := TdwlMySQLCommand.Create(FConnection, Query);
end;

destructor TdwlMySQLSession.Destroy;
begin
  if FConnection<>nil then
    FConnectionPool.ReleaseConnection(FConnection);
  inherited Destroy;
end;

procedure TdwlMySQLSession.ExecuteQuery(const Query: string);
begin
  // The following check is to prevent database changing in connections from the pool
  // This would raise issues when the connection is re-used and the originally
  // selected database went out of scope.
  // It's an assertion, so released functionality goes without this check
  Assert((FConnectionPool.FConnectionProperties.Database='')  or (Pos('USE ', Query.ToUpper)=0), 'USE <database> is not allowed when database is provided on session initiate');
  FConnection.Execute_Query(Query, false);
end;

{ TConnectionProperties }

function TConnectionProperties.UniqueIdentifier: string;
begin
  // better is a proper hash, but I'm lazy...
  Result := Host.ToLower+'##'+UserName+'##'+Database+'##'+Port.ToString+'##'+UseSSL.ToString;
end;

{ TMySQLNullDataBinding }

constructor TMySQLNullDataBinding.CreateInputInstance;
begin
  FIs_Null := false;
end;

class function TMySQLNullDataBinding.BufferType: Enum_Field_Types;
begin
  Result := MYSQL_TYPE_NULL;
end;

initialization

end.

