Unit KP_MapInstaller_Main;

Interface

Uses
  Windows, Messages, SysUtils, Variants, Classes, Registry,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  KP_ToolUtils, LibTar, Vcl.ComCtrls;

Type
  TKP_MapInstaller_MainForm = Class(TForm)
    Label1: TLabel;
    lblMapName: TLabel;
    btnCancel: TButton;
    btnInstall: TButton;
    lvMapItems: TListView;
    Label2: TLabel;
    Label6: TLabel;
    lblFileSize: TLabel;
    rbSP: TRadioButton;
    rbMP: TRadioButton;
    rbCamp: TRadioButton;
    Procedure FormCreate(Sender: TObject);
    Procedure btnCancelClick(Sender: TObject);
    Procedure btnInstallClick(Sender: TObject);
    Procedure rbClick(Sender: TObject);
  Private
    fKpmapFile, fMapDir: String;
  end;

Var
  KP_MapInstaller_MainForm: TKP_MapInstaller_MainForm;

Implementation

{$R *.dfm}

Procedure TKP_MapInstaller_MainForm.btnCancelClick(Sender: TObject);
Begin
  Application.Terminate;
end;

Procedure TKP_MapInstaller_MainForm.btnInstallClick(Sender: TObject);
Var
  Item: TListItem;
  Filename, TopDir: String;
  LastSlash: PChar;
  TarFileArchive: TTarArchive;
  DirRec: TTarDirRec;
  i: Integer;
Begin
  Screen.Cursor := crHourGlass;
  For i := 0 to lvMapItems.Items.Count - 1 do
  Begin
    Item := lvMapItems.Items[i];
    Filename := Item.Caption;
    LastSlash := StrRScan(PChar(Filename), '/');
    if LastSlash <> nil then
      Filename := String(LastSlash+1);
    {
      Tar is Unix, in Unix a Directory is a file type.
      Since Delphi does not create the Directories via filestream we must create the directory by ourselfs.
      This comes in handy for campaign support.
      This looks messy and I wish I could do it better, needs a review.
    }
    if (Item.SubItems[3] = 'Directory')then
    Begin
      if i = 0 then
      Begin
        TopDir := FileName + PathDelim;
        if not DirectoryExists(ExtractFilePath(Application.ExeName) + fMapDir + FileName) then
          MkDir(ExtractFilePath(Application.ExeName) + fMapDir + FileName);
      end else
        if not DirectoryExists(ExtractFilePath(Application.ExeName) + fMapDir + TopDir + FileName) then
          MkDir(ExtractFilePath(Application.ExeName) + fMapDir + TopDir + FileName);
    end else
    Begin
      TarFileArchive := TTarArchive.Create(fKpmapFile);
      Try
        TarFileArchive.Reset; // Always reset, this must be done for Tar (Else you might find your files being a mess)
        TarFileArchive.SetFilePos(Integer(Item.Data)); // We need to set file pos correctly or our files will be corrupt.
        TarFileArchive.FindNext(DirRec); // To set file as active.
        if string(DirRec.Name) <> Item.Caption then
        Begin // Archive has been modified whilst performing actions, that is a big no-no.
          ShowMessage('Filename mismatch.');
          Exit;
        end;
        Try
          TarFileArchive.ReadFile(ExtractFilePath(Application.ExeName) + fMapDir +
                                  StringReplace(Item.Caption, '/', PathDelim, [rfReplaceAll])); // Save file to location.
        Finally
          Screen.Cursor := crDefault;
        end;
      Finally
        FreeAndNil(TarFileArchive);
      end;
    end;
  end;
  Application.Terminate;
end;

Procedure TKP_MapInstaller_MainForm.FormCreate(Sender: TObject);
Var
  TarFileArchive: TTarArchive;
  Item: TListItem;
  DirRec: TTarDirRec;
  Pos, Size, Bytes: Int64;
Begin
  Screen.Cursor := crHourGlass;
  Bytes := 0;
  RegisterFileType; // Always update regestry during Alpha as it might change a lot.
  if (ParamStr(1) = '') then // Terminate installer if it is started without a kpmap file.
  Begin
    Screen.Cursor := crDefault;
    Application.Terminate
  end else
  Begin // Store param just to be sure and set startup values
    fKpmapFile := ParamStr(1);
    rbCamp.Checked := false;
    rbMP.Checked := false;
    rbSP.Checked := true;
    fMapDir := PathDelim + 'maps' + PathDelim;
    {
      Add all files to the listbox.
      Listbox is used so the user knows what's inside the file and it makes our lifes easier during extraction.
    }
    lvMapItems.Items.BeginUpdate;
    Try
      lvMapItems.Items.Clear;
      TarFileArchive := TTarArchive.Create(fKpmapFile);
      Try
        TarFileArchive.Reset; // Reset is a must.
        Bytes := 0;
        While TarFileArchive.FindNext(DirRec) do
        Begin // Add items and info to the listBox, some extra info just because.
          Item := lvMapItems.Items.Add;
          Item.Caption := String(DirRec.Name);
          Item.SubItems.Add(convertUnits(DirRec.Size, CONVERTBYTEVAL));
          Item.SubItems.Add(FormatDateTime('dd/mm/yyyy HH:NN:SS', DirRec.DateTime));
          Item.SubItems.Add(PermissionString(DirRec.Permissions));
          Item.SubItems.Add(FILETYPE_NAME[DirRec.FileType]);
          Item.Data := Pointer(DirRec.FilePos); // the data holds the pointer for the file pos within the tarball.
          TarFileArchive.GetFilePos(Pos, Size);
          Bytes := Bytes + DirRec.Size;
        end;
      Finally
        TarFileArchive.Free;
        lblFileSize.Caption := convertUnits(Bytes, CONVERTBYTEVAL);
      end;
    Finally
      lvMapItems.Items.EndUpdate;
      lblMapName.Caption := lvMapItems.Items[0].Caption;
    end;
  end;
  Screen.Cursor := crDefault;
end;

Procedure TKP_MapInstaller_MainForm.rbClick(Sender: TObject);
Begin
  rbCamp.Checked := (Sender = rbCamp);
  rbMP.Checked := (Sender = rbMP);
  rbSP.Checked := (Sender = rbSP);
  if Sender = rbCamp then
    fMapDir := PathDelim + 'campaigns' + PathDelim
  else
    if Sender = rbMP then
      fMapDir := PathDelim + 'mapsmp' + PathDelim
    else
      if Sender = rbSP then
        fMapDir := PathDelim + 'maps' + PathDelim
      else
      Begin
        ShowMessage('Something went wrong.');
        fMapDir := PathDelim + 'maps' + PathDelim;
      end;
end;

end.
