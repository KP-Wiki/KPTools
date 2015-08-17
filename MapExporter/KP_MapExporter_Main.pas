unit KP_MapExporter_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, FileCtrl,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, StrUtils,
  KP_ToolUtils, LibTar;

type
  TKP_MapExporter_MainForm = class(TForm)
    Label1: TLabel;
    edtMapFolderPath: TEdit;
    btnMapFolderSelect: TButton;
    btnCancel: TButton;
    btnExport: TButton;
    lblNotice: TLabel;
    lblFileName: TLabel;
    Label2: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnMapFolderSelectClick(Sender: TObject);
  private
    fMapPath, fMapName: String;
  end;

var
  KP_MapExporter_MainForm: TKP_MapExporter_MainForm;

implementation

{$R *.dfm}

procedure TKP_MapExporter_MainForm.btnCancelClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TKP_MapExporter_MainForm.btnExportClick(Sender: TObject);
var
  i: Integer;
  FileList, DirList: TStringList;
  FTarWriter: TTarWriter;
begin
  FileList := TStringList.Create;
  DirList := TStringList.Create;
  if GetFiles(fMapPath, FileList, DirList) then // Put all files into a stringList and check if result is true
  begin
    try
      // As Pascal hates to create directories by itself, check if it is there, if not make it.
      if not DirectoryExists(ExtractFilePath(Application.ExeName) + PathDelim + 'Exported maps') then
        mkDir(ExtractFilePath(Application.ExeName) + PathDelim + 'Exported maps');
      // Create .kpmap tarball in write-mode.
      FTarWriter := TTarWriter.Create(ExtractFilePath(Application.ExeName) + PathDelim + 'Exported maps' + PathDelim + fMapName + '.kpmap');
      for i := 0 to DirList.Count - 1 do
        if DirList[i] = '.' then // Also a Unix character, means current Directory. Change it to MapName.
          FTarWriter.AddDir(fMapName, Now)
        else
          FTarWriter.AddDir(fMapName + '/' + DirList[i], Now);
      for i := 0 to FileList.Count - 1 do // Add all files in stringList to the tarball
        FTarWriter.AddFile(fMapPath + FileList[i], fMapName + PathDelim + FileList[i]);
    finally // Cleanup and exit.
      FreeAndNil(DirList);
      FreeAndNil(FileList);
      FTarWriter.Finalize;
      FreeAndNil(FTarWriter);
    end;
    Application.Terminate;
  end;
end;

procedure TKP_MapExporter_MainForm.btnMapFolderSelectClick(Sender: TObject);
var
  OutPutList: TStringList;
begin
  OutPutList := TStringList.Create;
  if SelectDirectory('Select a directory', ExtractFilePath(Application.ExeName), fMapPath) then
  begin
    fMapPath := fMapPath + PathDelim;
    edtMapFolderPath.Text := fMapPath;
    Split(PathDelim, ExcludeTrailingPathDelimiter(fMapPath), OutPutList);
    fMapName := OutPutList[OutPutList.Count - 1];
    lblFileName.Caption := fMapName;
  end;
  FreeAndNil(OutPutList);
end;

procedure TKP_MapExporter_MainForm.FormCreate(Sender: TObject);
begin
  fMapPath := ExtractFilePath(Application.ExeName);
  edtMapFolderPath.Text := fMapPath;
  lblNotice.Caption := 'Notice:' + sLineBreak + 'Exported map installer files get stored in' + sLineBreak + fMapPath + 'Exported maps';
end;

end.
