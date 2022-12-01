program CoCon;

{$R 'versioninfo.res' 'versioninfo.rc'}

uses
  System.SysUtils,
  uCocon in 'Units\uCocon.pas';

begin
  try
    Install_New_Exe(ChangeFileExt(ParamStr(2), '')+'_old.exe', ParamStr(2), ParamStr(1));
  except
  end;
end.

