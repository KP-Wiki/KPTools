unit KP_MapInstaller_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Registry,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  KP_ToolUtils, LibTar, Vcl.ComCtrls;

type
  TKP_MapInstaller_MainForm = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure rbSPClick(Sender: TObject);
    procedure rbMPClick(Sender: TObject);
  private
    fKpmapFile, fMapDir: String;
  end;

var
  KP_MapInstaller_MainForm: TKP_MapInstaller_MainForm;

implementation

{$R *.dfm}

procedure TKP_MapInstaller_MainForm.btnCancelClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TKP_MapInstaller_MainForm.btnInstallClick(Sender: TObject);
var
  Item: TListItem;
  Filename: String;
  LastSlash: PChar;
  TA: TTarArchive;
  DirRec: TTarDirRec;
  i: Integer;
begin
  for i := 0 to lvMapItems.Items.Count - 1 do
  begin
    Item := lvMapItems.Items[i];
    Filename := Item.Caption;
    LastSlash := StrRScan(PChar(Filename), '/');
    if LastSlash <> nil then
      Filename := String(LastSlash+1);
    if i = 0 then // Map dir will always be 0.
    begin
     if not DirectoryExists(ExtractFilePath(Application.ExeName) + fMapDir + FileName) then
       MkDir(ExtractFilePath(Application.ExeName) + fMapDir + FileName);
    end
    else
    begin
      TA := TTarArchive.Create(fKpmapFile);
      try
        TA.Reset; // Always reset, this must be done for Tar (Else you might find your files being a mess)
        TA.SetFilePos(Integer(Item.Data)); // We need to set file pos correctly or our files will be corrupt.
        TA.FindNext(DirRec); // To set file as active.
        if string(DirRec.Name) <> Item.Caption then
        begin // Archive has been modified whilst performing actions, that is a big no-no.
          ShowMessage('Filename mismatch.');
          Exit;
        end;
        Screen.Cursor := crHourGlass;
        try
          TA.ReadFile(ExtractFilePath(Application.ExeName) + fMapDir +
                      StringReplace(Item.Caption, '/', PathDelim, [rfReplaceAll])); // Save file to location.
        finally
          Screen.Cursor := crDefault;
        end;
      finally
        TA.Free;
      end;
    end;
  end;
  Application.Terminate;
end;

procedure TKP_MapInstaller_MainForm.FormCreate(Sender: TObject);
var
  TA: TTarArchive;
  Item: TListItem;
  DirRec: TTarDirRec;
  Pos, Size, Bytes: Int64;
begin
  RegisterFileType; // Always update regestry during Alpha as it might change a lot.
  if (ParamStr(1) = '') then // Terminate installer if it is started without a kpmap file.
    Application.Terminate
  else
  begin // Store param just to be sure and set startup values
    fKpmapFile := ParamStr(1);
    rbMP.Checked := false;
    rbSP.Checked := true;
    fMapDir := PathDelim + 'maps' + PathDelim;
    {
      Add all files to the listbox.
      Listbox is used so the user knows what's inside the file and it makes our lifes easier during extraction.
    }
    lvMapItems.Items.BeginUpdate;
    try
      lvMapItems.Items.Clear;
      TA := TTarArchive.Create(fKpmapFile);
      try
        TA.Reset; // Reset is a must.
        Bytes := 0;
        while TA.FindNext(DirRec) do
        begin // Add items and info to the listBox, some extra info just because.
          Item := lvMapItems.Items.Add;
          Item.Caption := string(DirRec.Name);
          Item.SubItems.Add(convertUnits(DirRec.Size, CONVERTBYTEVAL));
          Item.SubItems.Add(FormatDateTime('dd/mm/yyyy HH:NN:SS', DirRec.DateTime));
          Item.SubItems.Add(PermissionString(DirRec.Permissions) + ' ' + FILETYPE_NAME[DirRec.FileType]);
          Item.SubItems.Add(IntToStr(DirRec.UID) + '-' + string(DirRec.UserName) + ' / ' +
                            IntToStr(DirRec.GID) + '-' + string(DirRec.GroupName));
          Item.SubItems.Add(string(DirRec.LinkName));
          Item.Data := Pointer(DirRec.FilePos); // the data holds the pointer for the file pos within the tarball.
          TA.GetFilePos(Pos, Size);
          Bytes := Bytes + DirRec.Size;
        end;
      finally
        TA.Free;
        lblFileSize.Caption := convertUnits(Bytes, CONVERTBYTEVAL);
      end;
    finally
      lvMapItems.Items.EndUpdate;
      lblMapName.Caption := lvMapItems.Items[0].Caption;
    end;
  end;
end;

procedure TKP_MapInstaller_MainForm.rbMPClick(Sender: TObject);
begin
  rbMP.Checked := true;
  rbSP.Checked := false;
  fMapDir := PathDelim + 'mapsmp' + PathDelim;
end;

procedure TKP_MapInstaller_MainForm.rbSPClick(Sender: TObject);
begin
  rbMP.Checked := false;
  rbSP.Checked := true;
  fMapDir := PathDelim + 'maps' + PathDelim;
end;

end.
