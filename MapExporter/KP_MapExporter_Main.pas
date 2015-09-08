Unit KP_MapExporter_Main;

Interface
{$WARNINGS ON}
{$HINTS ON}
{$WARN UNIT_PLATFORM OFF}

Uses
  SysUtils, Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.FileCtrl,
  KP_ToolUtils, LibTar;

Type
  TKP_MapExporter_MainForm = Class(TForm)
    Label1: TLabel;
    edtMapFolderPath: TEdit;
    btnMapFolderSelect: TButton;
    btnCancel: TButton;
    btnExport: TButton;
    lblNotice: TLabel;
    lblFileName: TLabel;
    Label2: TLabel;
    Procedure btnCancelClick(Sender: TObject);
    Procedure btnExportClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure btnMapFolderSelectClick(Sender: TObject);
  Private
    fMapPath, fMapName: String;
  end;

Var
  KP_MapExporter_MainForm: TKP_MapExporter_MainForm;

Implementation

{$R *.dfm}

Procedure TKP_MapExporter_MainForm.btnCancelClick(Sender: TObject);
Begin
  Application.Terminate;
end;

Procedure TKP_MapExporter_MainForm.btnExportClick(Sender: TObject);
Var
  i: Integer;
  FileList, DirList: TStringList;
  TarFileWriter: TTarWriter;
Begin
  Screen.Cursor := crHourGlass;
  FileList := TStringList.Create;
  DirList := TStringList.Create;

  if GetFiles(fMapPath, FileList, DirList) then // Put all files into a stringList and check if result is true
  Begin
    Try
      if not SysUtils.DirectoryExists(ExtractFilePath(Application.ExeName) + PathDelim + 'Exported maps') then
        mkDir(ExtractFilePath(Application.ExeName) + PathDelim + 'Exported maps');

      // Create .kpmap tarball in write-mode.
      TarFileWriter := TTarWriter.Create(ExtractFilePath(Application.ExeName) + PathDelim + 'Exported maps'
                                       + PathDelim + fMapName + '.kpmap');
      For i := 0 to DirList.Count - 1 do
        if DirList[i] = '.' then // A Unix character, means current Directory. Change it to MapName.
          TarFileWriter.AddDir(AnsiString(fMapName), Now)
        else
          TarFileWriter.AddDir(AnsiString(fMapName + '/' + DirList[i]), Now);

      For i := 0 to FileList.Count - 1 do // Add all files in stringList to the tarball
        TarFileWriter.AddFile(fMapPath + FileList[i], AnsiString(fMapName + PathDelim + FileList[i]));
    Finally // Cleanup and exit.
      FreeAndNil(DirList);
      FreeAndNil(FileList);
      TarFileWriter.Finalize;
      FreeAndNil(TarFileWriter);
    end;

    Screen.Cursor := crDefault;
    Application.Terminate;
  end;
end;

Procedure TKP_MapExporter_MainForm.btnMapFolderSelectClick(Sender: TObject);
Var
  OutPutList: TStringList;
Begin
  OutPutList := TStringList.Create;
  OutPutList.Clear;
  OutPutList.Delimiter := PathDelim;
  OutPutList.StrictDelimiter := True; // Requires Delphi 2006 or newer.

  if SelectDirectory('Select a directory', ExtractFilePath(Application.ExeName), fMapPath) then
  Begin
    fMapPath := fMapPath + PathDelim;
    edtMapFolderPath.Text := fMapPath;
    OutPutList.DelimitedText := ExcludeTrailingPathDelimiter(fMapPath);
    fMapName := OutPutList[OutPutList.Count - 1];
    lblFileName.Caption := fMapName;
  end;

  FreeAndNil(OutPutList);
end;

Procedure TKP_MapExporter_MainForm.FormCreate(Sender: TObject);
Begin
  fMapPath := ExtractFilePath(Application.ExeName);
  edtMapFolderPath.Text := fMapPath;
  lblNotice.Caption := 'Notice:' + sLineBreak + 'Exported map installer files get stored in'
                     + sLineBreak + fMapPath + 'Exported maps' + PathDelim;
end;

end.
