program MapExporter;

uses
  Vcl.Forms,
  KP_MapExporter_Main in 'KP_MapExporter_Main.pas' {KP_MapExporter_MainForm},
  LibTar in '..\src\LibTar.pas',
  KP_ToolUtils in '..\src\KP_ToolUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Knights Province Map Exporter';
  Application.CreateForm(TKP_MapExporter_MainForm, KP_MapExporter_MainForm);
  Application.Run;
end.
