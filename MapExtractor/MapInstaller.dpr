program MapInstaller;
{$I ..\KP_MapTools.inc}

uses
  Vcl.Forms,
  KP_MapInstaller_Main in 'KP_MapInstaller_Main.pas' {KP_MapInstaller_MainForm},
  KP_ToolUtils in '..\src\KP_ToolUtils.pas',
  KP_ToolCommonTypes in '..\src\KP_ToolCommonTypes.pas',
  LibTar in '..\src\LibTar.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Knights Province Map Installer';
  Application.CreateForm(TKP_MapInstaller_MainForm, KP_MapInstaller_MainForm);
  Application.Run;
end.
