unit DWL.Application;

interface

uses
  System.Generics.Collections, System.SysUtils;

type
  TdwlApplication = record
  strict private
    class var FOwnedObjects: TList<TObject>;
    class var FFinalProcs: TList<TProcedure>;
    class procedure Init; static;
    class procedure MyExitProc; static;
  public
    class procedure RegisterFinalExitProc(Proc: TProcedure); static;
    class procedure RegisterOwnedObject(OwnedObject: TObject); static;
  end;

implementation

{ TgsbApplication }

class procedure TdwlApplication.Init;
begin
  if FOwnedObjects<>nil then
    Exit;
  FOwnedObjects := TList<TObject>.Create;
  FFinalProcs := TList<TProcedure>.Create;
  AddExitProc(MyExitProc);
end;

class procedure TdwlApplication.MyExitProc;
begin
  for var OwnedObject in FOwnedObjects do
    OwnedObject.Free;
  for var Proc in FFinalProcs do
    TProcedure(Proc);
  FOwnedObjects.Free;
  FFinalProcs.Free;
end;

class procedure TdwlApplication.RegisterFinalExitProc(Proc: TProcedure);
begin
  Init;
  FFinalProcs.Insert(0, Proc);
end;

class procedure TdwlApplication.RegisterOwnedObject(OwnedObject: TObject);
begin
  Init;
  FOwnedObjects.Insert(0, OwnedObject);
end;

end.
