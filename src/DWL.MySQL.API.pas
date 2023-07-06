unit DWL.MySQL.API;

{
  Some words about character sets and mysql.
  This unit set the connection character encoding to utf8
  So all strings which are used in the api needs to be encoded as utf8

  functions and procedures just take the string and encode them inside the framework
  we do not offer overloaded ones that can take strings as UTF8, this would be
  a source of errors and the performance gain would be minimal

  Some research (2019-07-20) Revealed that the preferred method in Delphi is
  to use the native compiler type UTF8String as variable type
  Be warned for behaviour of Length() f.e. Length(UTF8Encode('€'))=3

  Conversion can be done through the latest advised functions UTF8Encode and UTF8ToString
}

interface

uses
  Winapi.Windows;

const
  {$IFDEF WIN64}
  libmysql  = 'libmysql64.dll';
  {$ELSE}
  libmysql  = 'libmysql.dll';
  {$ENDIF}

const
  NOT_NULL_FLAG = 1;      // Field can't be NULL
  PRI_KEY_FLAG = 2;       // Field is part of a primary key
  UNIQUE_KEY_FLAG = 4;    // Field is part of a unique key
  MULTIPLE_KEY_FLAG = 8;  // Field is part of a key
  BLOB_FLAG = 16;         // Field is a blob
  UNSIGNED_FLAG = 32;     // Field is unsigned
  ZEROFILL_FLAG = 64;     // Field is zerofill
  BINARY_FLAG = 128;
  ENUM_FLAG = 256;
  AUTO_INCREMENT_FLAG = 512;
  TIMESTAMP_FLAG = 1024;
  SET_FLAG = 2048;
  NO_DEFAULT_VALUE_FLAG = 4096;
  ON_UPDATE_NOW_FLAG = 8192;
  NUM_FLAG = 32768;
  PART_KEY_FLAG = 16384;
  GROUP_FLAG = 32768;
  UNIQUE_FLAG = 65536;
  BINCMP_FLAG = 131072;

const
  BINLOG_DUMP_NEVER_STOP = 0;
  BINLOG_DUMP_NON_BLOCK = 1;
  BINLOG_THROUGH_POSITION = 2;
  BINLOG_THROUGH_GTID = 4;

const
  //client flags
  CLIENT_LONG_PASSWORD                  = $00000001;    // new more secure passwords
  CLIENT_FOUND_ROWS                     = $00000002;    // Found instead of affected rows
  CLIENT_LONG_FLAG                      = $00000004;    // Get all column flags
  CLIENT_CONNECT_WITH_DB                = $00000008;    // One can specify db on connect
  CLIENT_NO_SCHEMA                      = $00000010;    // Don't allow database.table.column
  CLIENT_COMPRESS                       = $00000020;    // Can use compression protocol
  CLIENT_ODBC                           = $00000040;    // Odbc client
  CLIENT_LOCAL_FILES                    = $00000080;    // Can use LOAD DATA LOCAL
  CLIENT_IGNORE_SPACE                   = $00000100;    // Ignore spaces before '('
  CLIENT_PROTOCOL_41                    = $00000200;    // New 4.1 protocol
  CLIENT_INTERACTIVE                    = $00000400;    // This is an interactive client
  CLIENT_SSL                            = $00000800;    // Switch to SSL after handshake
  CLIENT_IGNORE_SIGPIPE                 = $00001000;    // IGNORE sigpipes
  CLIENT_TRANSACTIONS                   = $00002000;    // Client knows about transactions
  CLIENT_RESERVED                       = $00004000;    // Old flag for 4.1 protocol
  CLIENT_SECURE_CONNECTION              = $00008000;    // New 4.1 authentication
  CLIENT_MULTI_STATEMENTS               = $00010000;    // Enable/disable multi-stmt support
  CLIENT_MULTI_RESULTS                  = $00020000;    // Enable/disable multi-results
  CLIENT_PS_MULTI_RESULTS               = $00040000;
  CLIENT_PLUGIN_AUTH                    = $00080000;
  CLIENT_CONNECT_ATTRS                  = $00100000;
  CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA = $00200000;
  CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS   = $00400000;
  CLIENT_SESSION_TRACK                  = $00800000;
  CLIENT_DEPRECATE_EOF                  = $01000000;
  CLIENT_SSL_VERIFY_SERVER_CERT         = $04000000;
  CLIENT_REMEMBER_OPTIONS               = $80000000;

const
  MYSQL_DATA_TRUNCATED = 101;

const
  CR_SERVER_GONE_ERROR = 2006;
  CR_SERVER_LOST = 2013;

type
  PMySQL = pointer;
  PMySQL_Res = pointer;
  PMySQL_Stmt = pointer;

  Enum_Field_Types = (MYSQL_TYPE_DECIMAL, MYSQL_TYPE_TINY,
                      MYSQL_TYPE_SHORT, MYSQL_TYPE_LONG,
                      MYSQL_TYPE_FLOAT, MYSQL_TYPE_DOUBLE,
                      MYSQL_TYPE_NULL, MYSQL_TYPE_TIMESTAMP,
                      MYSQL_TYPE_LONGLONG, MYSQL_TYPE_INT24,
                      MYSQL_TYPE_DATE, MYSQL_TYPE_TIME,
                      MYSQL_TYPE_DATETIME, MYSQL_TYPE_YEAR,
                      MYSQL_TYPE_NEWDATE, MYSQL_TYPE_VARCHAR,
                      MYSQL_TYPE_BIT,
                      MYSQL_TYPE_NEWDECIMAL=246,
                      MYSQL_TYPE_ENUM=247,
                      MYSQL_TYPE_SET=248,
                      MYSQL_TYPE_TINY_BLOB=249,
                      MYSQL_TYPE_MEDIUM_BLOB=250,
                      MYSQL_TYPE_LONG_BLOB=251,
                      MYSQL_TYPE_BLOB=252,
                      MYSQL_TYPE_VAR_STRING=253,
                      MYSQL_TYPE_STRING=254,
                      MYSQL_TYPE_GEOMETRY=255);

  enum_session_state_type = (
    SESSION_TRACK_SYSTEM_VARIABLES,    // Session system variables
    SESSION_TRACK_SCHEMA,              // Current schema
    SESSION_TRACK_STATE_CHANGE,        // track session state changes
    SESSION_TRACK_GTIDS                // track GTIDs
  );

  enum_server_command = (
    COM_SLEEP               = $00,  // (default, e.g. SHOW PROCESSLIST)
    COM_QUIT                = $01,  // mysql_close
    COM_INIT_DB             = $02,  // mysql_select_db
    COM_QUERY               = $03,  // mysql_real_query
    COM_FIELD_LIST          = $04,  // mysql_list_fields
    COM_CREATE_DB           = $05,  // mysql_create_db
    COM_DROP_DB             = $06,  // mysql_drop_db
    COM_REFRESH             = $07,  // mysql_refresh
    COM_SHUTDOWN            = $08,  // mysql_shutdown
    COM_STATISTICS          = $09,  // mysql_stat
    COM_PROCESS_INFO        = $0A,  // mysql_list_processes
    COM_CONNECT             = $0B,  // (during authentication handshake)
    COM_PROCESS_KILL        = $0C,  // mysql_kill
    COM_DEBUG               = $0D,  //
    COM_PING                = $0E,  // mysql_ping
    COM_TIME                = $0F,  // (special value for slow logs?)
    COM_DELAYED_INSERT      = $10,  //
    COM_CHANGE_USER         = $11,  // mysql_change_user
    COM_BINLOG_DUMP         = $12,  // (used by slave server / mysqlbinlog)
    COM_TABLE_DUMP          = $13,  // (used by slave server to get master table)
    COM_CONNECT_OUT         = $14,  // (used by slave to log connection to master)
    COM_REGISTER_SLAVE      = $15,  // (reports slave location to master)
    COM_STMT_PREPARE        = $16,  // see description of Prepare Packet
    COM_STMT_EXECUTE        = $17,  // see description of Execute Packet
    COM_STMT_SEND_LONG_DATA = $18,  // see description of Long Data Packet
    COM_STMT_CLOSE          = $19,  // new, for closing statement
    COM_STMT_RESET          = $1A,  //
    COM_SET_OPTION          = $1B,  // mysql_set_option
    COM_STMT_FETCH          = $1C,  //
    COM_DAEMON              = $1D,
    COM_BINLOG_DUMP_GTID    = $1E,
    COM_RESET_CONNECTION    = $1F
  );

  PMySQL_Field = ^TMySQL_Field;
  TMySQL_Field{501} = record
    name: PAnsiChar;          // Name of column
    org_name: PAnsiChar;      // Original column name, if an alias
    table: PAnsiChar;         // Table of column if column was a field
    org_table: PAnsiChar;     // Org table name if table was an alias
    db: PAnsiChar;            // Database for table
    catalog: PAnsiChar;       // Catalog for table
    def: PAnsiChar;           // Default value (set by mysql_list_fields)
    length: UInt32;         // Width of column
    max_length: UInt32;     // Max width of selected set
    name_length: UInt32;
    org_name_length: UInt32;
    table_length: UInt32;
    org_table_length: UInt32;
    db_length: UInt32;
    catalog_length: UInt32;
    def_length: UInt32;
    flags: UInt32;          // Div flags
    decimals: UInt32;       // Number of decimals in field
    charsetnr: UInt32;      // Character set
    _type: Enum_Field_Types;  // Type of field. Se mysql_com.h for types
    extension: Pointer;
  end;

  PByteBool = ^ByteBool;

  TMySql_Bind{501} = record    // Version>=50100
    length: PCardinal;          // output length pointer
    is_null: PByteBool;       // Pointer to null indicator
    buffer: Pointer;         // buffer to get/put data
//  set this if you want to track data truncations happened during fetch
    error: PByteBool;
    row_ptr: PAnsiChar;      // for the current data position
    store_param_func: FARPROC;
    fetch_result: FARPROC;
    skip_result: FARPROC;
    buffer_length: DWORD;    // buffer length, must be set for string/binary
    offset: DWORD;           // offset position for char/binary fetch
    length_value: DWORD;     // Used if length is nil
    param_number: DWORD;     // For null count and error messages
    pack_length: DWORD;      // Internal length for packed data
    buffer_type: enum_field_types;
    error_value: ByteBool;    // used if error is nil
    is_unsigned: ByteBool;    // set if integer type is unsigned
    long_data_used: ByteBool; // If used with mysql_send_long_data
    is_null_value: ByteBool;  // Used if is_null is nil
    extension: Pointer;
  end;
  PMySQL_Bind = ^TMySql_Bind;

  PMySQL_Row = ^TMySQL_Row;
  TMySQL_Row = array[0..MaxInt div SizeOf(PUTF8String) - 1] of PAnsiChar;

  PMySQL_Lengths = ^TMySQL_Lengths;
  TMySQL_Lengths = array[0..MaxInt div SizeOf(UInt32) - 1] of UInt32;

  enum_mysql_timestamp_type=(
    MYSQL_TIMESTAMP_NONE = -2, MYSQL_TIMESTAMP_ERROR = -1,
    MYSQL_TIMESTAMP_DATE = 0, MYSQL_TIMESTAMP_DATETIME = 1, MYSQL_TIMESTAMP_TIME = 2, MYSQL_TIMESTAMP_DATETIME_TZ = 3);

  PMYSQL_TIME = ^TMYSQL_TIME;
  TMYSQL_TIME = record
    year, month, day, hour, minute, second: UInt32;
    second_part: UInt32;
    neg: UInt32;
    time_type: enum_mysql_timestamp_type;
  end;

{$WARN SYMBOl_PLATFORM OFF}
function mysql_errno(_mysql: PMySQL): UInt32; stdcall  external libmysql delayed;
function mysql_error(_mysql: PMySQL): PAnsiChar; stdcall external libmysql delayed;
function mysql_fetch_field_direct(res: PMySQL_Res; fieldnr: UInt32): PMySQL_Field; stdcall external libmysql delayed;
function mysql_fetch_lengths(_mysql_res: PMySQL_Res): PMySQL_Lengths; stdcall external libmysql delayed;
function mysql_fetch_row(_mysql_res: PMySQL_Res): PMySQL_Row; stdcall external libmysql delayed;
function mysql_get_client_version: UInt32; stdcall external libmysql delayed;
function mysql_init(_mysql: PMySQL): PMySQL; stdcall external libmysql delayed;
function mysql_insert_id(_mysql: PMYSQL): UInt64; stdcall external libmysql delayed;
function mysql_list_dbs(_mysql: PMYSQL; wild: PAnsiChar): PMySql_Res; stdcall external libmysql delayed;
function mysql_list_tables(_mysql: PMYSQL; wild: PAnsiChar): PMySql_Res; stdcall external libmysql delayed;
function mysql_num_fields(res: PMySQL_Res): UInt32; stdcall external libmysql delayed;
function mysql_num_rows(res: PMySQL_Res): UInt64; stdcall external libmysql delayed;
function mysql_real_connect(_mysql: PMySQL; host, user, passwd, db: PUTF8String; port: UInt32; unix_socket: PAnsiChar; clientflag: UInt32): PMySQL; stdcall external libmysql delayed;
function mysql_real_query(_mysql: PMySQL; Query: PUTF8String; length: UInt32): longint; stdcall external libmysql delayed;
function mysql_set_character_set(_mysql: PMySQL; csname: PAnsiChar): Int32; stdcall external libmysql delayed;
function mysql_stmt_affected_rows(stmt: PMySQL_Stmt): UInt64; stdcall external libmysql delayed;
function mysql_stmt_bind_param(stmt: PMySQL_Stmt; bind: PMySQL_Bind): ByteBool; stdcall external libmysql delayed;
function mysql_stmt_bind_result(stmt: PMySQL_Stmt; bind: PMySQL_Bind): ByteBool; stdcall external libmysql delayed;
function mysql_stmt_close(stmt: PMySQL_Stmt): ByteBool; stdcall external libmysql delayed;
function mysql_stmt_errno(stmt: PMySQL_Stmt): UInt32; stdcall external libmysql delayed;
function mysql_stmt_error(stmt: PMySQL_Stmt): PAnsiChar; stdcall external libmysql delayed;
function mysql_stmt_execute(stmt: PMySQL_Stmt): Int32; stdcall external libmysql delayed;
function mysql_stmt_fetch(stmt: PMySQL_Stmt): Int32; stdcall external libmysql delayed;
function mysql_stmt_fetch_column(stmt: PMySQL_Stmt; bind: PMySQL_Bind; column, offset: cardinal): Int32; stdcall external libmysql delayed;
function mysql_stmt_init(_mysql: PMySQL): PMYSQL_Stmt; stdcall external libmysql delayed;
function mysql_stmt_param_count(stmt: PMySQL_Stmt): cardinal; stdcall external libmysql delayed;
function mysql_stmt_prepare(stmt: PMySQL_Stmt; query: PUTF8String; length: UInt32): Int32; stdcall external libmysql delayed;
function mysql_stmt_result_metadata(stmt: PMySQL_Stmt): PMySQL_Res; stdcall external libmysql delayed;
function mysql_stmt_store_result(stmt: PMySQL_Stmt): Int32; stdcall external libmysql delayed;
function mysql_store_result(_mysql: PMySQL): PMySQL_Res; stdcall external libmysql delayed;
function mysql_use_result(_mysql: PMySQL): PMySQL_Res; stdcall external libmysql delayed;
procedure mysql_close(sock: PMySQL); stdcall external libmysql delayed;
procedure mysql_free_result(_mysql_res: PMySQL_Res); stdcall external libmysql delayed;
{$IFDEF GTID}
function mysql_session_track_get_first(_mysql: PMYSQL; state_type: enum_session_state_type; out data: PAnsiChar; out length: SIZE_T): LongInt; stdcall external libmysql delayed;
function mysql_session_track_get_next(_mysql: PMYSQL; state_type: enum_session_state_type; out data: PAnsiChar; out length: SIZE_T): LongInt; stdcall external libmysql delayed;
{$ENDIF}
{$WARN SYMBOl_PLATFORM ON}

function mysql_time_to_datetime(Mysql_Time:PMYSQL_TIME): TDateTime;
procedure datetime_to_mysql_time(fromdatetime: TDateTime; time_type: enum_mysql_timestamp_type; tomysql_time:PMYSQL_TIME);
function mysql_bind_init(Count: Int32): PMySQL_Bind;

implementation

uses
  System.SysUtils, System.DateUtils;

function mysql_time_to_datetime(Mysql_Time:PMYSQL_TIME): TDateTime;
begin
  case Mysql_Time.time_type of
  MYSQL_TIMESTAMP_DATE: Result :=  EncodeDate(Mysql_Time.Year, Mysql_Time.month, Mysql_Time.day);
  MYSQL_TIMESTAMP_DATETIME: Result := EncodeDateTime(Mysql_Time.Year, Mysql_Time.month, Mysql_Time.day, Mysql_Time.hour, Mysql_Time.minute, Mysql_Time.second, round(Mysql_Time.second_part/1000));
  MYSQL_TIMESTAMP_TIME: Result := EncodeTime(Mysql_Time.hour, Mysql_Time.minute, Mysql_Time.second, round(Mysql_Time.second_part/1000));
  else
    Result := 0;
  end;
end;

procedure datetime_to_mysql_time(fromdatetime: TDateTime; time_type: enum_mysql_timestamp_type; tomysql_time:PMYSQL_TIME);
var
  Year: word;
  Month: word;
  Day: word;
  Hour: word;
  Minute: word;
  Second: word;
  MSec: word;
begin
  DecodeDateTime(fromdatetime, Year, Month, Day, Hour, Minute, Second, MSec);
  tomysql_time.time_type := time_type;
  tomysql_time.year := Year;
  tomysql_time.month := Month;
  tomysql_time.day := Day;
  tomysql_time.hour := Hour;
  tomysql_time.minute := Minute;
  tomysql_time.second := Second;
  tomysql_time.second_part := MSec*1000;
end;

//Create initialized memory block for Bindings
function mysql_bind_init(Count: Int32): PMySQL_Bind;
begin
  if Count>0 then
    Result := AllocMem(Count*SizeOf(TMySql_Bind))
  else
    Result := nil;
end;

var
  _PrevNotifyHook: TDelayedLoadHook=nil;
  _PrevFailureHook: TDelayedLoadHook=nil;

function DelayedLoadHook(dliNotify: dliNotification; pdli: PDelayLoadInfo): Pointer; stdcall;
begin
  if SameText(string(pdli.szDll), libmysql) then
  begin
    case dliNotify of
      dliNoteEndProcessing:
        if (mysql_get_client_version<50100) then
          raise Exception.Create('Please upgrade '+libmysql+' to at least version 5.01');
      dliFailLoadLibrary:
        raise exception.Create('failed to load '+pdli.szDll+' make sure it is present in the executable directory');
    end;
  end;
  if dliNotify in [dliFailLoadLibrary, dliFailGetProcAddress] then
  begin
    if Assigned(_PrevFailureHook) then
      _PrevNotifyHook(dliNotify, pdli);
  end
  else
  begin
    if Assigned(_PrevNotifyHook) then
      _PrevNotifyHook(dliNotify, pdli);
  end;
  Result := nil;
end;

initialization
  _PrevNotifyHook := SetDliNotifyHook2(DelayedLoadHook);
  _PrevFailureHook := SetDliFailureHook2(DelayedLoadHook);

end.
