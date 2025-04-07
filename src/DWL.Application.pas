unit DWL.Application;

interface

uses
  System.Generics.Collections, System.SysUtils;

type
  TdwlApplication = record
  strict private
    class var FOwnedObjects: TList<TObject>;
    class var FFinalProcs: TList<TProcedure>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure Finalize; static;
    class procedure RegisterFinalExitProc(Proc: TProcedure); static;
    class procedure RegisterOwnedObject(OwnedObject: TObject); static;
  end;

implementation

uses
  Vcl.Dialogs;

class constructor TdwlApplication.Create;
begin
  // An exit proc is usable only for executables, not DLL's and packages
  // then try to detect packages, this is not straighforward
  // IsLibrary only works for DLL's , so an additional test is needed
  // by looking extension of modulename
  if (not IsLibrary) and Sametext(ExtractFileExt(GetModuleName(HInstance)), '.exe') then
    AddExitProc(Finalize);
end;

class destructor TdwlApplication.Destroy;
begin
  Finalize;
  inherited;
end;

class procedure TdwlApplication.Finalize;
begin
  if FOwnedObjects<>nil then
  begin
    for var OwnedObject in FOwnedObjects do
      OwnedObject.Free;
    FreeAndNil(FOwnedObjects);
  end;
  if FFinalProcs<>nil then
  begin
    for var Proc in FFinalProcs do
      TProcedure(Proc);
    FreeAndNil(FFinalProcs);
  end;
end;

{ TgsbApplication }

class procedure TdwlApplication.RegisterFinalExitProc(Proc: TProcedure);
begin
  if FFinalProcs=nil then
    FFinalProcs := TList<TProcedure>.Create;
  FFinalProcs.Insert(0, Proc);
end;

class procedure TdwlApplication.RegisterOwnedObject(OwnedObject: TObject);
begin
  if FOwnedObjects=nil then
    FOwnedObjects := TList<TObject>.Create;
  FOwnedObjects.Insert(0, OwnedObject);
end;

end.
