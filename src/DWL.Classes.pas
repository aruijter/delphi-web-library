unit DWL.Classes;

interface

uses
  DWL.Params, System.Classes;

type
  IdwlResult_Messages = interface
    procedure AddErrorMsg(const ErrorMsgToAdd: string);
    function ErrorMsg: string;
  end;

  /// <summary>
  ///   A record defining a generic multi function structure for methods that
  ///   must provide a non simple signal to the caller. The calling method can
  ///   choose how to handle the result in the interface.
  /// </summary>
  /// <remarks>
  ///   The default initialization is Success without message and an empty
  ///   key/value params
  /// </remarks>
  TdwlResult = record
  private
    FParams: IdwlParams;
    FSuccess: boolean;
    FMessages: IdwlResult_Messages;
  public
    class operator Initialize(out Dest: TdwlResult);
    /// <summary>
    ///   Call AddErrorMsg during processing when a fault condition is
    ///   encoutered. The Msg will be added to the Msg result and success is
    ///   set to false
    /// </summary>
    /// <param name="ErrorMsgToAdd">
    ///   The Msg describing the error condition
    /// </param>
    procedure AddErrorMsg(const ErrorMsgToAdd: string);
    /// <summary>
    ///   A Params object to transfer key/value pairs
    /// </summary>
    function Params: IdwlParams;
    /// <summary>
    ///   A function to merge another TdwlResults into this one
    /// </summary>
    /// <param name="ResultToMerge">
    ///   The other TdwlResult object to take information from
    /// </param>
    procedure Merge(ResultToMerge: TdwlResult);
    /// <summary>
    ///   Get the message present
    /// </summary>
    function ErrorMsg: string;
    /// <summary>
    ///   Indicates of the result was a success or not
    /// </summary>
    function Success: boolean;
  end;

  TdwlReadOnlyBufferStream = class(TCustomMemoryStream)
  public
    constructor Create(ContentBuffer: pointer; ContentSize: NativeInt);
  end;

  TdwlResultEvent = function: TdwlResult of object;

implementation

uses
  System.SysUtils;

type
  TdwlResult_ErrorMessages = class(TInterfacedObject, IdwlResult_Messages)
  strict private
    FErrorMessages: TStringList;
  private
    procedure AddErrorMsg(const ErrorMsgToAdd: string);
    function ErrorMsg: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TdwlResult }

procedure TdwlResult.AddErrorMsg(const ErrorMsgToAdd: string);
begin
  if FMessages=nil then
    FMessages := TdwlResult_ErrorMessages.Create;
  FMessages.AddErrorMsg(ErrorMsgToAdd);
  FSuccess := false;
end;

class operator TdwlResult.Initialize(out Dest: TdwlResult);
begin
  Dest.FSuccess := true;
end;

procedure TdwlResult.Merge(ResultToMerge: TdwlResult);
begin
  if ResultToMerge.ErrorMsg<>'' then
    AddErrorMsg(ResultToMerge.ErrorMsg);
  if ResultToMerge.FParams{!}<>nil then
    ResultToMerge.FParams.AssignTo(Params);
end;

function TdwlResult.ErrorMsg: string;
begin
  if FMessages=nil then
    Result := ''
  else
    Result := FMessages.ErrorMsg;
end;

function TdwlResult.Params: IdwlParams;
begin
  if FParams=nil then
    FParams := New_Params;
  Result := FParams;
end;

function TdwlResult.Success: boolean;
begin
  Result := FSuccess;
end;

{ TdwlReadOnlyBufferStream }

constructor TdwlReadOnlyBufferStream.Create(ContentBuffer: pointer; ContentSize: NativeInt);
begin
  inherited Create;
  SetPointer(ContentBuffer, ContentSize);
end;

{ TdwlResult_ErrorMessages }

procedure TdwlResult_ErrorMessages.AddErrorMsg(const ErrorMsgToAdd: string);
begin
  if FErrorMessages.IndexOf(ErrorMsgToAdd)<0 then
    FErrorMessages.Add(ErrorMsgToAdd);
end;

constructor TdwlResult_ErrorMessages.Create;
begin
  inherited Create;
  FErrorMessages := TStringList.Create;
  FErrorMessages.CaseSensitive := false;
end;

destructor TdwlResult_ErrorMessages.Destroy;
begin
  FErrorMessages.Free;
  inherited Destroy;
end;

function TdwlResult_ErrorMessages.ErrorMsg: string;
begin
  Result := FErrorMessages.Text;
end;

end.
