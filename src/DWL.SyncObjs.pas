unit DWL.SyncObjs;

interface

uses
  System.Classes, System.Generics.Collections, System.SyncObjs;

type
  // Reasons to use this TdwlThreadList/Queue instead of f.e. a TThreadList
  // - uses new SRW Locking which is superior to critical sections
  // - more lightweight
  // - multiread/exclusivewrite
  // - no default suppressduplicates
  /// <summary>
  ///   A Custom threaded list as a basis for TdwlThreadList and
  ///   TdwlThreadQueue implementations, not meant for direct use
  /// </summary>
  /// <remarks>
  ///   Reasons to use this TdwlThreadList/Queue instead of f.e. a TThreadList <br />
  ///   - uses new SRW Locking which is superior to critical sections <br />-
  ///   more lightweight <br />- multiread/exclusivewrite <br />- no default
  ///   suppressduplicates <br />
  /// </remarks>
  TdwlCustomThreadList<T> = class
  private
    FList: TList<T>;
    FMREW: TLightweightMREW;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
  end;

  /// <summary>
  ///   A generic thread safe multi read / exclusive write list
  /// </summary>
  TdwlThreadList<T> = class(TdwlCustomThreadList<T>)
  public
    /// <summary>
    ///   Add and item to the list
    /// </summary>
    function Add(Item: T): T;
    /// <summary>
    ///   Lock the list for direct reading. When ready call UnlocklistRead
    ///   (preferably by using a try..finally block)
    /// </summary>
    /// <returns>
    ///   A TList object to be able to modify the items directly
    /// </returns>
    function LockListRead: TList<T>;
    /// <summary>
    ///   Lock the list for direct writing. When ready call unlocklist
    ///   (preferably by using a try..finally block)
    /// </summary>
    /// <returns>
    ///   A TList object to be able to modify the items directly
    /// </returns>
    function LockList: TList<T>;
    /// <summary>
    ///   Thread safe removal of an item
    /// </summary>
    procedure Remove(Item: T); inline;
    /// <summary>
    ///   Unlock the list that has been lock before by LockListRead
    /// </summary>
    procedure UnlockListRead; inline;
    /// <summary>
    ///   Unlock the list that has been lock before by LockList
    /// </summary>
    procedure UnlockList; inline;
    /// <summary>
    ///   Try to pop an item from the list
    /// </summary>
    /// <param name="Item">
    ///   The retrieved item
    /// </param>
    /// <param name="Index">
    ///   The index, where to pop for, defaults to the first item
    /// </param>
    /// <returns>
    ///   indicating if an item was retrieved
    /// </returns>
    function TryPop(var Item:T; Index: integer=0): boolean;
  end;

  /// <summary>
  ///   A thread safe Queue, building on the TdwlThreadList by introducingQueue
  ///   specific functions <br />
  /// </summary>
  TdwlThreadQueue<T> = class(TdwlCustomThreadList<T>)
  public
    /// <summary>
    ///   Add an item at the end of the Queue
    /// </summary>
    /// <param name="Item">
    ///   the item to add
    /// </param>
    function Push(Item: T): T; virtual;
    /// <summary>
    ///   Add an item with high priority to the list (at the first position)
    /// </summary>
    /// <param name="Item">
    ///   the item to add
    /// </param>
    function PushWithPriority(Item: T): T; virtual;
    /// <summary>
    ///   Try to pop the first item from the list
    /// </summary>
    /// <param name="Item">
    ///   The item retrieved
    /// </param>
    /// <returns>
    ///   indicates if an item was retrieved
    /// </returns>
    function TryPop(var Item: T): boolean;
  end;

  /// <summary>
  ///   A specialized version of the htreaded queue with the option to wait for
  ///   an item to be added from another thread
  /// </summary>
  TdwlThreadQueue_Evented<T> = class(TdwlThreadQueue<T>)
  strict private
    FEvent: THandle;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Add an item at the end of the Queue
    /// </summary>
    /// <param name="Item">
    ///   the item to add
    /// </param>
    function Push(Item: T): T; override;
    /// <summary>
    ///   Add an item with high priority to the list (at the first position)
    /// </summary>
    /// <param name="Item">
    ///   the item to add
    /// </param>
    function PushWithPriority(Item: T): T; override;
    /// <summary>
    ///   Pops an item from the list if available, if not wait a maximum
    ///   timeout ms for the item to become available
    /// </summary>
    /// <param name="Item">
    ///   The item retrieved
    /// </param>
    /// <param name="Timeout">
    ///   The duration in ms to wait for an item to become available
    /// </param>
    /// <returns>
    ///   indicating is an item was retrieved
    /// </returns>
    function WaitForPop(var Item:T; Timeout: cardinal): boolean;
    /// <summary>
    ///   Wait a maximum timeout ms for an item to become available, but does
    ///   not Pop It.
    /// </summary>
    /// <param name="Timeout">
    ///   The duration in ms to wait for an item to become available
    /// </param>
    /// <returns>
    ///   indicating is an item became available
    /// </returns>
    function WaitForListNotEmpty(Timeout: cardinal): boolean;
    /// <summary>
    ///   Manually Set the Event forcing the waiting functions to return
    /// </summary>
    procedure Set_Event;
  end;

  /// <summary>
  ///   A TThread to be used by a descendant. The descendents then has a
  ///   FWorkToDoEventHandle avalable to be able to wait (in the Execute
  ///   function) to be signalled immediately when there is work to do (set in another way) or (automatically) the thread is terminated. <br />
  /// </summary>
  /// <example>
  ///   procedure TdwlThreadX.Execute; <br />begin <br />while not Terminated
  ///   do <br />begin <br />.......... <br />
  ///   WaitForSingleObject(FWorkToDoEventHandle, 1000); <br />end; <br />
  ///   end; <br />
  /// </example>
  TdwlThread = class(TThread)
  protected
    FWorkToDoEventHandle: THandle;
    procedure TerminatedSet; override;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

implementation

uses
  Winapi.Windows;

{ TdwlCustomThreadList<T> }

procedure TdwlCustomThreadList<T>.Clear;
begin
  FMREW.BeginWrite;
  try
    FList.Clear;
  finally
    FMREW.EndWrite;
  end;
end;

function TdwlCustomThreadList<T>.Count: integer;
begin
  FMREW.BeginRead;
  try
    Result := FList.Count;
  finally
    FMREW.EndRead;
  end;
end;

constructor TdwlCustomThreadList<T>.Create;
begin
  inherited Create;
  FList := TList<T>.Create;
end;

destructor TdwlCustomThreadList<T>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{ TdwlThreadList<T> }

function TdwlThreadList<T>.Add(Item: T): T;
begin
  FMREW.BeginWrite;
  try
    FList.Add(Item);
  finally
    FMREW.EndWrite;
  end;
  Result := Item;
end;

function TdwlThreadList<T>.LockList: TList<T>;
begin
  FMREW.BeginWrite;
  Result := FList;
end;

function TdwlThreadList<T>.LockListRead: TList<T>;
begin
  FMREW.BeginRead;
  Result := FList;
end;

procedure TdwlThreadList<T>.Remove(Item: T);
begin
  FMREW.BeginWrite;
  try
    FList.Remove(Item);
  finally
    FMREW.EndWrite;
  end;
end;

function TdwlThreadList<T>.TryPop(var Item: T; Index: integer): boolean;
begin
  FMREW.BeginWrite;
  try
    Result := FList.Count>Index;
    if Result then
    begin
      Item := FList[Index];
      FList.Delete(Index);
    end;
  finally
    FMREW.EndWrite;
  end;
end;

procedure TdwlThreadList<T>.UnlockList;
begin
  FMREW.EndWrite;
end;

procedure TdwlThreadList<T>.UnlockListRead;
begin
  FMREW.EndRead;
end;

{ TdwlThreadQueue<T> }

function TdwlThreadQueue<T>.Push(Item: T): T;
begin
  FMREW.BeginWrite;
  try
    FList.Add(Item);
  finally
    FMREW.EndWrite;
  end;
  Result := Item;
end;

function TdwlThreadQueue<T>.PushWithPriority(Item: T): T;
begin
  FMREW.BeginWrite;
  try
    FList.Insert(0, Item);
  finally
    FMREW.EndWrite;
  end;
  Result := Item;
end;

function TdwlThreadQueue<T>.TryPop(var Item: T): boolean;
begin
  // first getting a locked reading seems an optimization (as queue is propbably mostly empty)
  // but we're the only reader in a queue, so directly getting
  // a write lock is the way to go. (unless a lot of multithreading popping is
  // done, but I guess this will not be the case, most usage will be push by one thread
  // and trypop by another
  FMREW.BeginWrite;
  try
    Result := FList.Count>0;
    if Result then
    begin
      Item := FList[0];
      FList.Delete(0);
    end;
  finally
    FMREW.EndWrite;
  end;
end;

{ TdwlThread }

procedure TdwlThread.AfterConstruction;
begin
  inherited AfterConstruction;
  FWorkToDoEventHandle := CreateEvent(nil, true, false, nil);
end;

destructor TdwlThread.Destroy;
begin
  inherited Destroy;
  CloseHandle(FWorkToDoEventHandle);
end;

procedure TdwlThread.TerminatedSet;
begin
  inherited TerminatedSet;
  SetEvent(FWorkToDoEventHandle);
end;

{ TdwlThreadQueue_Evented<T> }

constructor TdwlThreadQueue_Evented<T>.Create;
begin
  inherited Create;
  FEvent := CreateEvent(nil, false, false, nil);
end;

destructor TdwlThreadQueue_Evented<T>.Destroy;
begin
  inherited Destroy;
  CloseHandle(FEvent);
end;

function TdwlThreadQueue_Evented<T>.Push(Item: T): T;
begin
  inherited Push(Item);
  SetEvent(FEvent);
end;

function TdwlThreadQueue_Evented<T>.PushWithPriority(Item: T): T;
begin
  inherited PushWithPriority(Item);
  SetEvent(FEvent)
end;

procedure TdwlThreadQueue_Evented<T>.Set_Event;
begin
  SetEvent(FEvent);
end;

function TdwlThreadQueue_Evented<T>.WaitForListNotEmpty(Timeout: cardinal): boolean;
begin
  Result := (Count>0) or ((WaitForSingleObject(FEvent, TimeOut)=WAIT_OBJECT_0) and (Count>0));
end;

function TdwlThreadQueue_Evented<T>.WaitForPop(var Item: T; Timeout: cardinal): boolean;
begin
  Result := TryPop(Item) or ((WaitForSingleObject(FEvent, TimeOut)=WAIT_OBJECT_0) and TryPop(Item));
end;

end.
