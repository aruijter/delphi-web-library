unit DisCoIde.Publish;

interface

uses
  System.Generics.Collections, Vcl.Menus;

type
  TDisCoIde_Publish = class
  strict private
    class var
      FMenuItems: TObjectList<TMenuItem>;
    class procedure DoPublish(Sender: TObject);
  public
    class constructor Create;
    class destructor Destroy;
  end;

implementation

uses
  ToolsAPI, fPublish;

{ TDisCoIde_Publish }

class constructor TDisCoIde_Publish.Create;
begin
  inherited;
  FMenuItems := TObjectList<TMenuItem>.Create(true);
  var MainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  var ProjectMenu := MainMenu.Items.Find('Project');
  if ProjectMenu<>nil then
  begin
    var MI := TMenuItem.Create(ProjectMenu);
    FMenuItems.Add(MI); // this will destroy the item when finalizing
    MI.Caption := 'Publish to DisCo';
    MI.Name := 'DisCoIde_MenuItem_Publish';
    MI.OnClick := DoPublish;
    ProjectMenu.Insert(0, MI);
  end;
end;

class destructor TDisCoIde_Publish.Destroy;
begin
  FMenuItems.Free;
  inherited;
end;

class procedure TDisCoIde_Publish.DoPublish(Sender: TObject);
begin
  var Frm := TPublishForm.Create(nil);
  try
    Frm.ShowModal;
  finally
    Frm.Free;
  end;
end;

end.
