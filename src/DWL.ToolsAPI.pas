unit DWL.ToolsAPI;

interface

uses
  ToolsAPI, Vcl.Menus, System.Classes;

type
  TdwlToolsAPI = record
    class function ActiveProject: IOTAProject; static;
    class procedure SetProjectOption(Project: IOTAProject; const Name: string; Value: variant); static;
  end;

  TdwlToolsAPI_KeyBinding_MenuItem = class(TNotifierObject, IOTAKeyboardBinding)
  strict private
    FMenuItem: TMenuItem;
    procedure KeyBindingProc(const Context: IOTAKeyContext; KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
  public
    constructor Create(MenuItem: TMenuItem);
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  end;

implementation

uses
  Vcl.Dialogs, System.SysUtils;

{ TdwlToolsAPI_KeyBinding_MenuItem }

procedure TdwlToolsAPI_KeyBinding_MenuItem.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([FMenuItem.ShortCut], KeyBindingProc, nil, 0);
end;

constructor TdwlToolsAPI_KeyBinding_MenuItem.Create(MenuItem: TMenuItem);
begin
  inherited Create;
  FMenuItem := MenuItem;
end;

function TdwlToolsAPI_KeyBinding_MenuItem.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TdwlToolsAPI_KeyBinding_MenuItem.GetDisplayName: string;
begin
  Result := FMenuItem.Caption;
end;

function TdwlToolsAPI_KeyBinding_MenuItem.GetName: string;
begin
  Result := 'DWL_Keybinding.'+FMenuItem.Name;
end;

procedure TdwlToolsAPI_KeyBinding_MenuItem.KeyBindingProc(const Context: IOTAKeyContext; KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  if FMenuItem.Enabled then
    FMenuItem.Click;
  BindingResult := krHandled;
end;

{ TdwlToolsAPI }

class function TdwlToolsAPI.ActiveProject: IOTAProject;
begin
  Result := nil;
  var Services := BorlandIDEServices as IOTAModuleServices;
  for var i := 0 to Services.ModuleCount-1 do
  begin
    var Module := Services.Modules[i];
    var ProjectGroup: IOTAProjectGroup;
    if (Module.QueryInterface(IOTAProjectGroup, ProjectGroup) = S_OK) then
    begin
      Result := ProjectGroup.ActiveProject;
      Exit;
    end;
    var Project: IOTAProject;
    if (Module.QueryInterface(IOTAProject, Project) = S_OK) then
    begin
      Result := Project;
      Exit;
    end;
  end;
end;

class procedure TdwlToolsAPI.SetProjectOption(Project: IOTAProject; const Name: string; Value: variant);
begin
  if Project=nil then
    Project := ActiveProject;
  if Project=nil then
    Exit;
  var CurrentValue := Project.ProjectOptions.Values[Name];
  if CurrentValue<>Value then
  begin
    Project.ProjectOptions.Values[Name] := Value;
    Project.MarkModified;
  end;
end;

end.
