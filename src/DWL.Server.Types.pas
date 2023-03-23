/// <summary>
///   THESE STRUCTURES ARE SHARED BETWEEN SERVER AND DLL's <br />YOU CANNOT
///   CHANGE THESE UNLESS YOU RECOMPILE THE SERVER AND ALL DLL's <br /><br />
/// </summary>
unit DWL.Server.Types;

interface

type
  PdwlHTTPHandlingState = ^TdwlHTTPHandlingState;

  TdwlHTTPWebSocket_OnData = procedure(State: PdwlHTTPHandlingState; const Data: pointer; DataSize: cardinal; DataIsText: boolean);
  TdwlArrangeContentBufferProc =procedure(const State: PdwlHTTPHandlingState; var ContentBuffer: pointer; const ContentLength: cardinal); stdcall;
  TdwlGetRequestParamProc = function(const State: PdwlHTTPHandlingState; const Key: PWideChar; const Value: PWideChar; var ValueCharCnt: integer): integer; stdcall;
  TdwlGetHeaderValueProc = function(const State: PdwlHTTPHandlingState; const Key: PWideChar; const Value: PWideChar; var ValueCharCnt: integer): integer; stdcall;
  TdwlGetPayloadPtrProc = function(State: PdwlHTTPHandlingState; out Data: pointer; out DataSize: Int64): boolean; stdcall;
  TdwlSetHeaderValueProc = procedure(const State: PdwlHTTPHandlingState; const HeaderKey, Value: PWideChar); stdcall;
  TdwlActivateWebSocketProc = function(const State: PdwlHTTPHandlingState; ReceiveProc: TdwlHTTPWebSocket_OnData): TdwlHTTPWebSocket_OnData;

  /// <summary>
  ///   <para>
  ///     internal structures, one for each side of the ''medal'
  ///   </para>
  ///   <para>
  ///     WARNING: don't change this handling structure. All communication
  ///     with the DLL's f.e. are using this structure. This structure only
  ///     contains C compatible base types, so DLL's can be written in other
  ///     programming languages <br />
  ///   </para>
  /// </summary>
  TdwlHTTPHandlingState = record
    /// <summary>
    ///   available serverside to keep track of internal server resources
    /// </summary>
    _InternalServerStructure: pointer;
    /// <summary>
    ///   available to keep track of specific internal handling resources
    /// </summary>
    _InternalHandlingStructure: pointer;
    /// <summary>
    ///   URI (part after endpoint), provided from server side, do not change <br />
    /// </summary>
    URI: PWideChar;
    /// <summary>
    ///   The HTTP Request method, provided from server side, do not change <br />
    ///   This is a byte, definition of values can be found int DWL.HTTP.Consts f.e. 'GET'=dwlhttpGET=1
    /// </summary>
    RequestMethod: byte;
    /// <summary>
    ///   bi directional flags, not used by now <br />
    /// </summary>
    Flags: integer;
    /// <summary>
    ///   Statuscode can be changed by handler, but is default 200
    ///   a server fault situation detected by the server (as f.e. result=false)
    ///   will be assigned the right status code serverside
    /// </summary>
    StatusCode: word;
  end;

  PdwlCallBackProcs = ^TdwlCallBackProcs;
  TdwlCallBackProcs = record
    /// <summary>
    ///   a callback function that must be used to define the ContentBuffer to be used
    ///   if ContentBuffer=nil, the server will allocate ContentLength bytes and take care of disposing, the pointer to the buffer will be returned
    ///   if the ContentBuffer is given, it is allocated handling side and
    ///   must be disposed in a FinalizeProc that of course also must be assigend to the state
    /// </summary>
    ArrangeContentBufferProc: TdwlArrangeContentBufferProc;
    /// <summary>
    ///   a callback function that can be used to get Request Params
    /// </summary>
    GetRequestParamProc: TdwlGetRequestParamProc;
    /// <summary>
    ///   a callback function that can be used to get a Header Value from
    ///   request
    /// </summary>
    GetHeaderValueProc: TdwlGetHeaderValueProc;
    /// <summary>
    ///   a callback function tant can be use to get the pointer to the payload
    ///   data memory location
    /// </summary>
    GetPayloadPtrProc: TdwlGetPayloadPtrProc;
    /// <summary>
    ///   a callback function to set a header in the response
    /// </summary>
    SetHeaderValueProc: TdwlSetHeaderValueProc;
    /// <summary>
    ///   a callback function to create a websocket, if nil returned request
    ///   did not succeed
    /// </summary>
    ActivateWebSocketproc: TdwlActivateWebSocketProc;
  end;


  /// <summary>
  ///   The Authorize proc is used to to give the DLL the possibility to authorize the caller of a request
  /// </summary>
  TDLL_AuthorizeProc = function(const State: PdwlHTTPHandlingState): boolean; stdcall;
  /// <summary>
  ///   The configure proc is used to to give the DLL the possibility to configure itself
  ///   parameters are provided a key/value pairs in TStringlist.Text layout.
  /// </summary>
  TDLL_ConfigureProc = function(const CallBackProcs: PdwlCallBackProcs; const Params: PWideChar): string; stdcall;
  /// <summary>
  ///   The wrapup proc is used to to give the DLL the possibility to clean up
  ///   resources. if state is given, the request has finished and resources like contentbuffer can be cleared
  ///   is state is nil, it is the final wrapup call for global resources
  /// </summary>
  TdwlWrapupProc = procedure(const State: PdwlHTTPHandlingState); stdcall;
  /// <summary>
  ///   The processrequest function implemented in the DLL processes the actual request
  /// </summary>
  TDLL_ProcessRequestProc = function(const State: PdwlHTTPHandlingState): boolean; stdcall;

implementation

end.
