unit DisCoIde.General;

interface

uses
  DWL.Params;

type
  TDisCoIde_General = class
  strict private
    class function ConfigFileName: string;
  public
    class var
      FConfigParams: IdwlParams;
    class constructor Create;
    class function ModuleDirectory: string;
  end;

implementation

{$R SPLASH.RES}

uses
  Vcl.Forms, Vcl.Graphics, ToolsAPI, System.SysUtils, System.IOUtils,
  Vcl.Dialogs;

{ TDisCoIde_General }

class function TDisCoIde_General.ConfigFileName: string;
begin
  Result := ModuleDirectory+'\DisCoIde.config';
end;

class constructor TDisCoIde_General.Create;
begin
  inherited;
  FConfigParams := New_Params;
  if FileExists(ConfigFileName) then
    FConfigParams.WriteJSON(TFile.ReadAllText(ConfigFileName));

  // Disable the irritating ScreenSnap of the IDE Mainform
  Application.MainForm.ScreenSnap := false;
  // Add myself to SplashScreen
  var Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, 'IDEE');
    SplashScreenServices.AddPluginBitmap('DisCo IDE - Configure and distribute applications', Bitmap.Handle, false, 'For free!');
  finally
    Bitmap.Free;
  end;
end;

class function TDisCoIde_General.ModuleDirectory: string;
begin
  Result := ExtractFileDir(GetModuleName(HInstance));
end;

end.

