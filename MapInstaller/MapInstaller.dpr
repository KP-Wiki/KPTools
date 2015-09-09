program MapInstaller;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  KP_MapInstaller_Main in 'KP_MapInstaller_Main.pas' {KP_MapInstaller_MainForm},
  KP_ToolUtils in '..\src\KP_ToolUtils.pas',
  LibTar in '..\src\LibTar.pas',
  Gzip in '..\src\Gzip.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Knights Province Map Installer';
  Application.CreateForm(TKP_MapInstaller_MainForm, KP_MapInstaller_MainForm);
  Application.Run;
end.
