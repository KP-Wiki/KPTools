{
===============================================================================================
Name    : LibTar
===============================================================================================
Subject : Handling of "tar" files
===============================================================================================
Author  : Stefan Heymann
          Eschenweg 3
          72076 Tübingen
          GERMANY

E-Mail:   stefan@destructor.de
Web:      www.destructor.de

===============================================================================================
TTarArchive Usage
-----------------
- Choose a constructor
- Make an instance of TTarArchive                  TA := TTarArchive.Create (Filename);
- Scan through the archive                         TA.Reset;
                                                   while TA.FindNext (DirRec) do begin
- Evaluate the DirRec for each file                  ListBox.Items.Add (DirRec.Name);
- Read out the current file                          TA.ReadFile (DestFilename);
  (You can ommit this if you want to
  read in the directory only)                        end;
- You're done                                      TA.Free;

TTarWriter Usage
----------------
- Choose a constructor
- Make an instance of TTarWriter                   TW := TTarWriter.Create ('my.tar');
- Add a file to the tar archive                    TW.AddFile ('foobar.txt');
- Add a string as a file                           TW.AddString (SL.Text, 'joe.txt', Now);
- Destroy TarWriter instance                       TW.Free;
- Now your tar file is ready.

Source, Legals ("Licence")
--------------------------
The official site to get this code is http://www.destructor.de/

Usage and Distribution of this Source Code is ruled by the
"Destructor.de Source code Licence" (DSL) which comes with this file or
can be downloaded at http://www.destructor.de/

IN SHORT: Usage and distribution of this source code is free.
          You use it completely on your own risk.
===============================================================================================
!!!  All parts of this code which are not finished or known to be buggy
     are marked with three exclamation marks
===============================================================================================
}
Unit LibTar;
{$I ..\KP_MapTools.inc}

Interface

Uses
{$IFDEF LINUX}
   Libc,
{$ENDIF}
{$IFDEF WIN32}
  {$DEFINE MSWINDOWS} // predefined for D6+/BCB6+, in Delphi 5 MSWINDOWS is not defined
{$ENDIF}
{$IFDEF MSWINDOWS}
   Windows,
{$ENDIF}
{$IFDEF WDC250PLUS}
  System.AnsiStrings,
{$ENDIF}
  SysUtils, Classes;


Type
  (*$IFNDEF UNICODE *)
  RawByteString = AnsiString;
  (*$ENDIF *)
  // --- File Access Permissions
  TTarPermission  = (tpReadByOwner, tpWriteByOwner, tpExecuteByOwner,
                     tpReadByGroup, tpWriteByGroup, tpExecuteByGroup,
                     tpReadByOther, tpWriteByOther, tpExecuteByOther);
  TTarPermissions = Set of TTarPermission;
  // --- Type of File
  TFileType = (ftNormal,          // Regular file
               ftLink,            // Link to another, previously archived, file (LinkName)
               ftSymbolicLink,    // Symbolic link to another file              (LinkName)
               ftCharacter,       // Character special files
               ftBlock,           // Block special files
               ftDirectory,       // Directory entry. Size is zero (unlimited) or max. number of bytes
               ftFifo,            // FIFO special file. No data stored in the archive.
               ftContiguous,      // Contiguous file, if supported by OS
               ftDumpDir,         // List of files
               ftMultiVolume,     // Multi-volume file part
               ftVolumeHeader);   // Volume header. Can appear only as first record in the archive
  // --- Mode
  TTarMode  = (tmSetUid, tmSetGid, tmSaveText);
  TTarModes = Set of TTarMode;
  // --- Record for a Directory Entry
  //     Adjust the ClearDirRec procedure when this record changes!
  TTarDirRec  = Record
                  Name       : AnsiString;        // File path and name
                  Size       : Int64;             // File size in Bytes
                  DateTime   : TDateTime;         // Last modification date and time
                  Permissions: TTarPermissions;   // Access permissions
                  FileType   : TFileType;         // Type of file
                  LinkName   : AnsiString;        // Name of linked file (for ftLink, ftSymbolicLink)
                  UID        : Integer;           // User ID
                  GID        : Integer;           // Group ID
                  UserName   : AnsiString;        // User name
                  GroupName  : AnsiString;        // Group name
                  ChecksumOK : Boolean;           // Checksum was OK
                  Mode       : TTarModes;         // Mode
                  Magic      : AnsiString;        // Contents of the "Magic" field
                  MajorDevNo : Integer;           // Major Device No. for ftCharacter and ftBlock
                  MinorDevNo : Integer;           // Minor Device No. for ftCharacter and ftBlock
                  FilePos    : Int64;             // Position in TAR file
                end;
  // --- The TAR Archive CLASS
  TTarArchive = Class
                Protected
                  FStream:     TStream; // Internal Stream
                  FOwnsStream: Boolean; // True if FStream is owned by the TTarArchive instance
                  FBytesToGo:  Int64;   // Bytes until the next Header Record
                Public
                  Constructor Create(Stream: TStream); Overload;
                  Constructor Create(Filename: String; FileMode: Word = fmOpenRead or fmShareDenyWrite); Overload;
                  Destructor Destroy; Override;
                  Procedure Reset;                                     // Reset File Pointer
                  Function FindNext(Var DirRec: TTarDirRec): Boolean; // Reads next Directory Info Record. FALSE if EOF reached
                  Procedure ReadFile(Buffer: Pointer); Overload;       // Reads file data for last Directory Record
                  Procedure ReadFile(Stream: TStream); Overload;       // -;-
                  Procedure ReadFile(Filename: String); Overload;      // -;-
                  Function ReadFile: RawByteString; Overload;         // -;-
                  Procedure GetFilePos(Var Current, Size : Int64);     // Current File Position
                  Procedure SetFilePos(NewPos : Int64);                // Set new Current File Position
                end;
  // --- The TAR Archive Writer CLASS
  TTarWriter = Class
               Protected
                 FStream     : TStream;
                 FOwnsStream : Boolean;
                 FFinalized  : Boolean;         // --- Used at the next "Add" method call: ---
                 FPermissions: TTarPermissions; // Access permissions
                 FUID        : Integer;         // User ID
                 FGID        : Integer;         // Group ID
                 FUserName   : AnsiString;      // User name
                 FGroupName  : AnsiString;      // Group name
                 FMode       : TTarModes;       // Mode
                 FMagic      : AnsiString;      // Contents of the "Magic" field
                 Constructor CreateEmpty;
               Public
                 Constructor Create(TargetStream: TStream); Overload;
                 Constructor Create(TargetFilename: String; Mode: Integer = fmCreate); Overload;
                 Destructor Destroy; Override; // Writes End-Of-File Tag
                 Procedure AddFile(Filename: String; TarFilename: AnsiString = '');
                 Procedure AddStream(Stream: TStream; TarFilename: AnsiString; FileDateGmt: TDateTime);
                 Procedure AddString(Contents: RawByteString; TarFilename: AnsiString; FileDateGmt: TDateTime);
                 Procedure AddDir(Dirname: AnsiString; DateGmt: TDateTime; MaxDirSize: Int64 = 0);
                 Procedure AddSymbolicLink(Filename, Linkname: AnsiString; DateGmt: TDateTime);
                 Procedure AddLink(Filename, Linkname: AnsiString; DateGmt: TDateTime);
                 Procedure AddVolumeHeader(VolumeId: AnsiString; DateGmt: TDateTime);
                 Procedure Finalize;
                 Property Permissions: TTarPermissions Read FPermissions Write FPermissions; // Access permissions
                 Property UID: Integer Read FUID Write FUID;                                 // User ID
                 Property GID: Integer Read FGID Write FGID;                                 // Group ID
                 Property UserName: AnsiString Read FUserName Write FUserName;               // User name
                 Property GroupName: AnsiString Read FGroupName Write FGroupName;            // Group name
                 Property Mode: TTarModes Read FMode Write FMode;                            // Mode
                 Property Magic: AnsiString Read FMagic Write FMagic;                        // Contents of the "Magic" field
               end;


// --- Some useful constants
Const
  FILETYPE_NAME: Array [TFileType] of String =
                 (
                   'Regular', 'Link', 'Symbolic Link', 'Char File', 'Block File',
                   'Directory', 'FIFO File', 'Contiguous', 'Dir Dump', 'Multivol', 'Volume Header'
                 );
  ALL_PERMISSIONS     = [tpReadByOwner, tpWriteByOwner, tpExecuteByOwner,
                         tpReadByGroup, tpWriteByGroup, tpExecuteByGroup,
                         tpReadByOther, tpWriteByOther, tpExecuteByOther];
  READ_PERMISSIONS    = [tpReadByOwner, tpReadByGroup,  tpReadByOther];
  WRITE_PERMISSIONS   = [tpWriteByOwner, tpWriteByGroup, tpWriteByOther];
  EXECUTE_PERMISSIONS = [tpExecuteByOwner, tpExecuteByGroup, tpExecuteByOther];


Function PermissionString(Permissions: TTarPermissions): String;
Function ConvertFilename(Filename: String): String;
Function FileTimeGMT(FileName: String): TDateTime;  Overload;
Function FileTimeGMT(SearchRec: TSearchRec): TDateTime; Overload;
Procedure ClearDirRec(Var DirRec: TTarDirRec);


{
===============================================================================================
IMPLEMENTATION <- You don't say? :D
===============================================================================================
}
Implementation

Function PermissionString(Permissions : TTarPermissions) : String;
Begin
  Result := '';
  if tpReadByOwner    in Permissions then Result := Result + 'r' else Result := Result + '-';
  if tpWriteByOwner   in Permissions then Result := Result + 'w' else Result := Result + '-';
  if tpExecuteByOwner in Permissions then Result := Result + 'x' else Result := Result + '-';
  if tpReadByGroup    in Permissions then Result := Result + 'r' else Result := Result + '-';
  if tpWriteByGroup   in Permissions then Result := Result + 'w' else Result := Result + '-';
  if tpExecuteByGroup in Permissions then Result := Result + 'x' else Result := Result + '-';
  if tpReadByOther    in Permissions then Result := Result + 'r' else Result := Result + '-';
  if tpWriteByOther   in Permissions then Result := Result + 'w' else Result := Result + '-';
  if tpExecuteByOther in Permissions then Result := Result + 'x' else Result := Result + '-';
end;


Function ConvertFilename(Filename: String): String; // Converts the filename to Unix conventions
Begin
  (*$IFDEF LINUX *)
  Result := Filename;
  (*$ELSE *)
  Result := StringReplace(Filename, '\', '/', [rfReplaceAll]);
  (*$ENDIF *)
end;


Function FileTimeGMT(FileName: String): TDateTime;
{
  Returns the Date and Time of the last modification of the given File
  The Result is zero if the file could not be found
  The Result is given in UTC (GMT) time zone
}
Var
  SR: TSearchRec;
Begin
  Result := 0.0;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
    Result := FileTimeGMT(SR);
  FindClose(SR);
end;


Function FileTimeGMT(SearchRec: TSearchRec): TDateTime;
Var
(*$IFDEF MSWINDOWS *)
  SystemFileTime: TSystemTime;
(*$ENDIF *)
(*$IFDEF LINUX *)
  TimeVal: TTimeVal;
  TimeZone: TTimeZone;
(*$ENDIF *)
Begin
  Result := 0.0;
  (*$IFDEF MSWINDOWS *) (*$WARNINGS OFF *)
    if (SearchRec.FindData.dwFileAttributes and faDirectory) = 0 then
      if FileTimeToSystemTime(SearchRec.FindData.ftLastWriteTime, SystemFileTime) then
        Result := EncodeDate(SystemFileTime.wYear, SystemFileTime.wMonth, SystemFileTime.wDay)
                + EncodeTime(SystemFileTime.wHour, SystemFileTime.wMinute, SystemFileTime.wSecond, SystemFileTime.wMilliseconds);
  (*$ENDIF *) (*$WARNINGS ON *)
  (*$IFDEF LINUX *)
    if SearchRec.Attr and faDirectory = 0 then
    Begin
      Result := FileDateToDateTime(SearchRec.Time);
      GetTimeOfDay(TimeVal, TimeZone);
      Result := Result + TimeZone.tz_minuteswest / (60 * 24);
    end;
  (*$ENDIF *)
end;


Procedure ClearDirRec(Var DirRec: TTarDirRec);
{
  This is included because a FillChar (DirRec, SizeOf (DirRec), 0)
  will destroy the long string pointers, leading to strange bugs
}
Begin
  With DirRec do
  Begin
    Name        := '';
    Size        := 0;
    DateTime    := 0.0;
    Permissions := [];
    FileType    := TFileType(0);
    LinkName    := '';
    UID         := 0;
    GID         := 0;
    UserName    := '';
    GroupName   := '';
    ChecksumOK  := False;
    Mode        := [];
    Magic       := '';
    MajorDevNo  := 0;
    MinorDevNo  := 0;
    FilePos     := 0;
  end;
end;


{
===============================================================================================
TAR format
===============================================================================================
}
Const
  RECORDSIZE = 512;
  NAMSIZ     = 100;
  TUNMLEN    =  32;
  TGNMLEN    =  32;
  CHKBLANKS  = #32#32#32#32#32#32#32#32;

Type
  TTarHeader = Packed Record
                 Name     : Array[0..NAMSIZ-1] of AnsiChar;
                 Mode     : Array[0..7]  of AnsiChar;
                 UID      : Array[0..7]  of AnsiChar;
                 GID      : Array[0..7]  of AnsiChar;
                 Size     : Array[0..11] of AnsiChar;
                 MTime    : Array[0..11] of AnsiChar;
                 ChkSum   : Array[0..7]  of AnsiChar;
                 LinkFlag : AnsiChar;
                 LinkName : Array[0..NAMSIZ-1] of AnsiChar;
                 Magic    : Array[0..7] of AnsiChar;
                 UName    : Array[0..TUNMLEN-1] of AnsiChar;
                 GName    : Array[0..TGNMLEN-1] of AnsiChar;
                 DevMajor : Array[0..7] of AnsiChar;
                 DevMinor : Array[0..7] of AnsiChar;
               end;


Function ExtractText(P: PAnsiChar): AnsiString;
Begin
  Result := AnsiString(P);
end;


Function ExtractNumber(P: PAnsiChar): Integer; Overload;
Var
  Strg: AnsiString;
Begin
  Strg := AnsiString(Trim(String(P)));
  P := PAnsiChar(Strg);
  Result := 0;
  While (P^ <> #32) and (P^ <> #0) do
  Begin
    Result := (Ord(P^) - Ord('0')) or (Result Shl 3);
    Inc(P);
  end;
end;


Function ExtractNumber64(P: PAnsiChar): Int64; Overload;
Var
  Strg: AnsiString;
Begin
  Strg := AnsiString(Trim(String(P)));
  P := PAnsiChar(Strg);
  Result := 0;
  While (P^ <> #32) and (P^ <> #0) do
  Begin
    Result := (Ord(P^) - Ord('0')) or (Result Shl 3);
    Inc(P);
  end;
end;


Function ExtractNumber(P: PAnsiChar; MaxLen: Integer): Integer; Overload;
Var
  S0: Array[0..255] of AnsiChar;
  Strg: AnsiString;
Begin
  {$IFDEF WDC250PLUS}
    System.AnsiStrings.StrLCopy(S0, P, MaxLen);
  {$ELSE}
    StrLCopy(S0, P, MaxLen);
  {$ENDIF}

  Strg := AnsiString(Trim(String(S0)));
  P := PAnsiChar(Strg);
  Result := 0;
  While (P^ <> #32) and (P^ <> #0) do
  Begin
    Result := (Ord(P^) - Ord('0')) or (Result Shl 3);
    Inc(P);
  end;
end;


Function ExtractNumber64(P: PAnsiChar; MaxLen: Integer): Int64; Overload;
Var
  S0: Array[0..255] of AnsiChar;
  Strg: AnsiString;
Begin
  {$IFDEF WDC250PLUS}
    System.AnsiStrings.StrLCopy(S0, P, MaxLen);
  {$ELSE}
    StrLCopy(S0, P, MaxLen);
  {$ENDIF}
  Strg := AnsiString(Trim(String(S0)));
  P := PAnsiChar(Strg);
  Result := 0;
  While (P^ <> #32) and (P^ <> #0) do
  Begin
    Result := (Ord(P^) - Ord('0')) or (Result Shl 3);
    Inc(P);
  end;
end;


Function Records(Bytes: Int64): Int64;
Begin
  Result := Bytes Div RECORDSIZE;
  if Bytes Mod RECORDSIZE > 0 then
    Inc(Result);
end;


Procedure Octal(N: Integer; P: PAnsiChar; Len: Integer);
{
  Makes a string of octal digits
  The string will always be "Len" characters long
}
Var
  I: Integer;
Begin
  For I := Len-2 downto 0 do
  Begin
    (P+I)^ := AnsiChar(Ord('0') + Ord(N and $07));
    N := N Shr 3;
  end;
  For I := 0 to Len-3 do
    if (P+I)^ = '0' then (P+I)^ := #32 else Break;
  (P+Len-1)^ := #32;
end;


Procedure Octal64(N: Int64; P: PAnsiChar; Len: Integer);
{
  Makes a string of octal digits
  The string will always be "Len" characters long
}
Var
  I: Integer;
Begin
  For I := Len-2 downto 0 do
  Begin
    (P+I)^ := AnsiChar(Ord('0') + Ord(N and $07));
    N := N Shr 3;
  end;
  For I := 0 to Len-3 do
    if (P+I)^ = '0' then (P+I)^ := #32 else Break;
  (P+Len-1)^ := #32;
end;


Procedure OctalN(N: Integer; P: PAnsiChar; Len: Integer);
Begin
  Octal(N, P, Len-1);
  (P+Len-1)^ := #0;
end;


Procedure WriteTarHeader(Dest: TStream; DirRec: TTarDirRec);
Var
  Rec      : Array[0..RECORDSIZE-1] of AnsiChar;
  TH       : TTarHeader Absolute Rec;
  Mode     : Integer;
  NullDate : TDateTime;
  Checksum : Cardinal;
  I        : Integer;
Begin
  FillChar(Rec, RECORDSIZE, 0);
  {$IFDEF WDC250PLUS}
    System.AnsiStrings.StrLCopy(TH.Name, PAnsiChar(DirRec.Name), NAMSIZ);
  {$ELSE}
    StrLCopy(TH.Name, PAnsiChar(DirRec.Name), NAMSIZ);
  {$ENDIF}
  Case DirRec.FileType of
    ftNormal, ftLink: Mode := $08000;
    ftSymbolicLink  : Mode := $0A000;
    ftDirectory     : Mode := $04000;
    else              Mode := 0;
  end;
  if tmSaveText       in DirRec.Mode        then Mode := Mode or $0200;
  if tmSetGid         in DirRec.Mode        then Mode := Mode or $0400;
  if tmSetUid         in DirRec.Mode        then Mode := Mode or $0800;
  if tpReadByOwner    in DirRec.Permissions then Mode := Mode or $0100;
  if tpWriteByOwner   in DirRec.Permissions then Mode := Mode or $0080;
  if tpExecuteByOwner in DirRec.Permissions then Mode := Mode or $0040;
  if tpReadByGroup    in DirRec.Permissions then Mode := Mode or $0020;
  if tpWriteByGroup   in DirRec.Permissions then Mode := Mode or $0010;
  if tpExecuteByGroup in DirRec.Permissions then Mode := Mode or $0008;
  if tpReadByOther    in DirRec.Permissions then Mode := Mode or $0004;
  if tpWriteByOther   in DirRec.Permissions then Mode := Mode or $0002;
  if tpExecuteByOther in DirRec.Permissions then Mode := Mode or $0001;
  OctalN(Mode, @TH.Mode, 8);
  OctalN(DirRec.UID, @TH.UID, 8);
  OctalN(DirRec.GID, @TH.GID, 8);
  Octal64(DirRec.Size, @TH.Size, 12);
  NullDate := EncodeDate(1970, 1, 1);
  if DirRec.DateTime >= NullDate then
    Octal(Trunc((DirRec.DateTime - NullDate) * 86400.0), @TH.MTime, 12)
  else
    Octal(Trunc(NullDate * 86400.0), @TH.MTime, 12);
  Case DirRec.FileType of
    ftNormal      : TH.LinkFlag := '0';
    ftLink        : TH.LinkFlag := '1';
    ftSymbolicLink: TH.LinkFlag := '2';
    ftCharacter   : TH.LinkFlag := '3';
    ftBlock       : TH.LinkFlag := '4';
    ftDirectory   : TH.LinkFlag := '5';
    ftFifo        : TH.LinkFlag := '6';
    ftContiguous  : TH.LinkFlag := '7';
    ftDumpDir     : TH.LinkFlag := 'D';
    ftMultiVolume : TH.LinkFlag := 'M';
    ftVolumeHeader: TH.LinkFlag := 'V';
  end;
  {$IFDEF WDC250PLUS}
    System.AnsiStrings.StrLCopy(TH.LinkName, PAnsiChar(DirRec.LinkName), NAMSIZ);
    System.AnsiStrings.StrLCopy(TH.Magic, PAnsiChar(DirRec.Magic + #32#32#32#32#32#32#32#32), 8);
    System.AnsiStrings.StrLCopy(TH.UName, PAnsiChar(DirRec.UserName), TUNMLEN);
    System.AnsiStrings.StrLCopy(TH.GName, PAnsiChar(DirRec.GroupName), TGNMLEN);
  {$ELSE}
    StrLCopy(TH.LinkName, PAnsiChar(DirRec.LinkName), NAMSIZ);
    StrLCopy(TH.Magic, PAnsiChar(DirRec.Magic + #32#32#32#32#32#32#32#32), 8);
    StrLCopy(TH.UName, PAnsiChar(DirRec.UserName), TUNMLEN);
    StrLCopy(TH.GName, PAnsiChar(DirRec.GroupName), TGNMLEN);
  {$ENDIF}
  OctalN(DirRec.MajorDevNo, @TH.DevMajor, 8);
  OctalN(DirRec.MinorDevNo, @TH.DevMinor, 8);
  {$IFDEF WDC250PLUS}
    System.AnsiStrings.StrMove(TH.ChkSum, CHKBLANKS, 8);
  {$ELSE}
    StrMove(TH.ChkSum, CHKBLANKS, 8);
  {$ENDIF}
  CheckSum := 0;
  For I := 0 to SizeOf(TTarHeader)-1 do
    Inc(CheckSum, Integer(Ord(Rec[I])));
  OctalN(CheckSum, @TH.ChkSum, 8);
  Dest.Write(TH, RECORDSIZE);
end;


{
===============================================================================================
TTarArchive
===============================================================================================
}
Constructor TTarArchive.Create(Stream: TStream);
Begin
  Inherited Create;
  FStream := Stream;
  FOwnsStream := FALSE;
  Reset;
end;


Constructor TTarArchive.Create(Filename: String; FileMode: Word);
Begin
  Inherited Create;
  FStream := TFileStream.Create(Filename, FileMode);
  FOwnsStream := TRUE;
  Reset;
end;


Destructor TTarArchive.Destroy;
Begin
  if FOwnsStream then
    FreeAndNil(FStream);
  Inherited Destroy;
end;


Procedure TTarArchive.Reset; // Reset File Pointer
Begin
  FStream.Position := 0;
  FBytesToGo := 0;
end;


Function TTarArchive.FindNext(Var DirRec: TTarDirRec): Boolean;
{
  Reads next Directory Info Record
  The Stream pointer must point to the first byte of the tar header
}
Var
  Rec         : Array[0..RECORDSIZE-1] of Char;
  CurFilePos  : Int64;
  Header      : TTarHeader Absolute Rec;
  I           : Integer;
  HeaderChkSum: Word;
  Checksum    : Cardinal;
Begin
  // --- Scan until next pointer
  if FBytesToGo > 0 then
    FStream.Seek(Records(FBytesToGo) * RECORDSIZE, soCurrent);
  // --- EOF reached?
  Result := False;
  CurFilePos := FStream.Position;
  Try
    FStream.ReadBuffer(Rec, RECORDSIZE);
    if Rec[0] = #0 then Exit; // EOF reached
  Except
    Exit; // EOF reached, too
  end;
  Result := True;
  ClearDirRec(DirRec);
  DirRec.FilePos := CurFilePos;
  DirRec.Name := ExtractText(Header.Name);
  DirRec.Size := ExtractNumber64(@Header.Size, 12);
  DirRec.DateTime := EncodeDate(1970, 1, 1) + (ExtractNumber(@Header.MTime, 12) / 86400.0);
  I := ExtractNumber(@Header.Mode);
  if I and $0100 <> 0 then Include(DirRec.Permissions, tpReadByOwner);
  if I and $0080 <> 0 then Include(DirRec.Permissions, tpWriteByOwner);
  if I and $0040 <> 0 then Include(DirRec.Permissions, tpExecuteByOwner);
  if I and $0020 <> 0 then Include(DirRec.Permissions, tpReadByGroup);
  if I and $0010 <> 0 then Include(DirRec.Permissions, tpWriteByGroup);
  if I and $0008 <> 0 then Include(DirRec.Permissions, tpExecuteByGroup);
  if I and $0004 <> 0 then Include(DirRec.Permissions, tpReadByOther);
  if I and $0002 <> 0 then Include(DirRec.Permissions, tpWriteByOther);
  if I and $0001 <> 0 then Include(DirRec.Permissions, tpExecuteByOther);
  if I and $0200 <> 0 then Include(DirRec.Mode, tmSaveText);
  if I and $0400 <> 0 then Include(DirRec.Mode, tmSetGid);
  if I and $0800 <> 0 then Include(DirRec.Mode, tmSetUid);
  Case Header.LinkFlag of
    #0, '0': DirRec.FileType := ftNormal;
    '1'    : DirRec.FileType := ftLink;
    '2'    : DirRec.FileType := ftSymbolicLink;
    '3'    : DirRec.FileType := ftCharacter;
    '4'    : DirRec.FileType := ftBlock;
    '5'    : DirRec.FileType := ftDirectory;
    '6'    : DirRec.FileType := ftFifo;
    '7'    : DirRec.FileType := ftContiguous;
    'D'    : DirRec.FileType := ftDumpDir;
    'M'    : DirRec.FileType := ftMultiVolume;
    'V'    : DirRec.FileType := ftVolumeHeader;
  end;
  DirRec.LinkName   := ExtractText(Header.LinkName);
  DirRec.UID        := ExtractNumber(@Header.UID);
  DirRec.GID        := ExtractNumber(@Header.GID);
  DirRec.UserName   := ExtractText(Header.UName);
  DirRec.GroupName  := ExtractText(Header.GName);
  DirRec.Magic      := AnsiString(Trim(String(Header.Magic)));
  DirRec.MajorDevNo := ExtractNumber(@Header.DevMajor);
  DirRec.MinorDevNo := ExtractNumber(@Header.DevMinor);
  HeaderChkSum := ExtractNumber(@Header.ChkSum);   // Calc Checksum
  CheckSum := 0;
  {$IFDEF WDC250PLUS}
    System.AnsiStrings.StrMove(Header.ChkSum, CHKBLANKS, 8);
  {$ELSE}
    StrMove(Header.ChkSum, CHKBLANKS, 8);
  {$ENDIF}
  For I := 0 to SizeOf(TTarHeader)-1 do
    Inc(CheckSum, Integer(Ord(Rec[I])));
  DirRec.CheckSumOK := Word(CheckSum) = Word(HeaderChkSum);
  if DirRec.FileType in [ftLink, ftSymbolicLink, ftDirectory, ftFifo, ftVolumeHeader] then
    FBytesToGo := 0
  else
    FBytesToGo := DirRec.Size;
end;


Procedure TTarArchive.ReadFile(Buffer: Pointer);
{
  Reads file data for the last Directory Record. The entire file is read into the buffer.
  The buffer must be large enough to take up the whole file.
}
Var
  RestBytes: Integer;
Begin
  if FBytesToGo = 0 then Exit;
  RestBytes := Records(FBytesToGo) * RECORDSIZE - FBytesToGo;
  FStream.ReadBuffer(Buffer^, FBytesToGo);
  FStream.Seek(RestBytes, soCurrent);
  FBytesToGo := 0;
end;


Procedure TTarArchive.ReadFile(Stream: TStream);
{
  Reads file data for the last Directory Record.
  The entire file is written out to the stream.
  The stream is left at its current position prior to writing
}
Var
  RestBytes: Integer;
Begin
  if FBytesToGo = 0 then Exit;
  RestBytes := Records(FBytesToGo) * RECORDSIZE - FBytesToGo;
  Stream.CopyFrom(FStream, FBytesToGo);
  FStream.Seek(RestBytes, soCurrent);
  FBytesToGo := 0;
end;


Procedure TTarArchive.ReadFile(Filename: String);
{
  Reads file data for the last Directory Record.
  The entire file is saved in the given Filename
}
Var
  FS: TFileStream;
Begin
  if FileExists(Filename) then
    FS := TFileStream.Create(Filename, fmOpenReadWrite or fmShareDenyWrite)
  else
    FS := TFileStream.Create(Filename, fmCreate or fmShareDenyWrite);
  Try
    ReadFile(FS);
  Finally
    FreeAndNil(FS);
  end;
end;


Function TTarArchive.ReadFile: RawByteString;
{
  Reads file data for the last Directory Record. The entire file is returned
  ss a large ANSI string.
}
Var
  RestBytes: Integer;
Begin
  if FBytesToGo = 0 then Exit;
  RestBytes := Records(FBytesToGo) * RECORDSIZE - FBytesToGo;
  SetLength(Result, FBytesToGo);
  FStream.ReadBuffer(PAnsiChar(Result)^, FBytesToGo);
  FStream.Seek(RestBytes, soCurrent);
  FBytesToGo := 0;
end;


Procedure TTarArchive.GetFilePos(Var Current, Size: Int64); // Returns the Current Position in the TAR stream
Begin
  Current := FStream.Position;
  Size := FStream.Size;
end;


Procedure TTarArchive.SetFilePos(NewPos: Int64); // Set new Current File Position
Begin
  if NewPos < FStream.Size then FStream.Seek(NewPos, soBeginning);
end;


{
===============================================================================================
TTarWriter
===============================================================================================
}
Constructor TTarWriter.CreateEmpty;
Var
  TP: TTarPermission;
Begin
  Inherited Create;
  FOwnsStream := False;
  FFinalized := False;
  FPermissions := [];
  For TP := Low(TP) to High(TP) do
    Include(FPermissions, TP);
  FUID := 0;
  FGID := 0;
  FUserName := '';
  FGroupName := '';
  FMode := [];
  FMagic := 'ustar';
end;


Constructor TTarWriter.Create(TargetStream: TStream);
Begin
  CreateEmpty;
  FStream := TargetStream;
  FOwnsStream := False;
end;


Constructor TTarWriter.Create(TargetFilename: String; Mode: Integer = fmCreate);
Begin
  CreateEmpty;
  FStream := TFileStream.Create(TargetFilename, Mode);
  FOwnsStream := True;
end;


Destructor TTarWriter.Destroy;
Begin
  if not FFinalized then
  Begin
    Finalize;
    FFinalized := True;
  end;
  if FOwnsStream then
    FreeAndNil(FStream);
  Inherited Destroy;
end;


Procedure TTarWriter.AddFile(Filename: String; TarFilename: AnsiString = '');
Var
  S   : TFileStream;
  Date: TDateTime;
Begin
  Date := FileTimeGMT(Filename);
  if TarFilename = '' then
    TarFilename := AnsiString(ConvertFilename(Filename))
  else
    TarFilename := AnsiString(ConvertFilename(String(TarFilename)));
  S := TFileStream.Create(Filename, fmOpenRead OR fmShareDenyWrite);
  Try
    AddStream(S, TarFilename, Date);
  Finally
    FreeAndNil(S);
  end;
end;


Procedure TTarWriter.AddStream(Stream: TStream; TarFilename: AnsiString; FileDateGmt: TDateTime);
Var
  DirRec     : TTarDirRec;
  Rec        : Array[0..RECORDSIZE-1] of Char;
  BytesToRead: Int64;      // Bytes to read from the Source Stream
  BlockSize  : Int64;      // Bytes to write out for the current record
Begin
  ClearDirRec(DirRec);
  DirRec.Name := TarFilename;
  DirRec.Size := Stream.Size - Stream.Position;
  DirRec.DateTime := FileDateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType := ftNormal;
  DirRec.LinkName := '';
  DirRec.UID := FUID;
  DirRec.GID := FGID;
  DirRec.UserName := FUserName;
  DirRec.GroupName := FGroupName;
  DirRec.ChecksumOK := True;
  DirRec.Mode := FMode;
  DirRec.Magic := FMagic;
  DirRec.MajorDevNo := 0;
  DirRec.MinorDevNo := 0;
  WriteTarHeader(FStream, DirRec);
  BytesToRead := DirRec.Size;
  While BytesToRead > 0 do
  Begin
    BlockSize := BytesToRead;
    if BlockSize > RECORDSIZE then BlockSize := RECORDSIZE;
    FillChar(Rec, RECORDSIZE, 0);
    Stream.Read(Rec, BlockSize);
    FStream.Write(Rec, RECORDSIZE);
    Dec(BytesToRead, BlockSize);
  end;
end;


Procedure TTarWriter.AddString(Contents: RawByteString; TarFilename: AnsiString; FileDateGmt: TDateTime);
Var
  S: TStringStream;
Begin
  S := TStringStream.Create(Contents);
  Try
    AddStream(S, TarFilename, FileDateGmt);
  Finally
    FreeAndNil(S);
  end;
end;


Procedure TTarWriter.AddDir(Dirname: AnsiString; DateGmt: TDateTime; MaxDirSize: Int64 = 0);
Var
  DirRec: TTarDirRec;
Begin
  ClearDirRec(DirRec);
  DirRec.Name := Dirname;
  DirRec.Size := MaxDirSize;
  DirRec.DateTime := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType := ftDirectory;
  DirRec.LinkName := '';
  DirRec.UID := FUID;
  DirRec.GID := FGID;
  DirRec.UserName := FUserName;
  DirRec.GroupName := FGroupName;
  DirRec.ChecksumOK := True;
  DirRec.Mode := FMode;
  DirRec.Magic := FMagic;
  DirRec.MajorDevNo := 0;
  DirRec.MinorDevNo := 0;
  WriteTarHeader(FStream, DirRec);
end;


Procedure TTarWriter.AddSymbolicLink(Filename, Linkname: AnsiString; DateGmt: TDateTime);
Var
  DirRec: TTarDirRec;
Begin
  ClearDirRec(DirRec);
  DirRec.Name := Filename;
  DirRec.Size := 0;
  DirRec.DateTime := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType := ftSymbolicLink;
  DirRec.LinkName := Linkname;
  DirRec.UID := FUID;
  DirRec.GID := FGID;
  DirRec.UserName := FUserName;
  DirRec.GroupName := FGroupName;
  DirRec.ChecksumOK := True;
  DirRec.Mode := FMode;
  DirRec.Magic := FMagic;
  DirRec.MajorDevNo := 0;
  DirRec.MinorDevNo := 0;
  WriteTarHeader(FStream, DirRec);
end;


Procedure TTarWriter.AddLink(Filename, Linkname: AnsiString; DateGmt: TDateTime);
Var
  DirRec: TTarDirRec;
Begin
  ClearDirRec(DirRec);
  DirRec.Name := Filename;
  DirRec.Size := 0;
  DirRec.DateTime := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType := ftLink;
  DirRec.LinkName := Linkname;
  DirRec.UID := FUID;
  DirRec.GID := FGID;
  DirRec.UserName := FUserName;
  DirRec.GroupName := FGroupName;
  DirRec.ChecksumOK := True;
  DirRec.Mode := FMode;
  DirRec.Magic := FMagic;
  DirRec.MajorDevNo := 0;
  DirRec.MinorDevNo := 0;
  WriteTarHeader(FStream, DirRec);
end;


Procedure TTarWriter.AddVolumeHeader(VolumeId: AnsiString; DateGmt: TDateTime);
Var
  DirRec: TTarDirRec;
Begin
  ClearDirRec(DirRec);
  DirRec.Name := VolumeId;
  DirRec.Size := 0;
  DirRec.DateTime := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType := ftVolumeHeader;
  DirRec.LinkName := '';
  DirRec.UID := FUID;
  DirRec.GID := FGID;
  DirRec.UserName := FUserName;
  DirRec.GroupName := FGroupName;
  DirRec.ChecksumOK := True;
  DirRec.Mode := FMode;
  DirRec.Magic := FMagic;
  DirRec.MajorDevNo := 0;
  DirRec.MinorDevNo := 0;
  WriteTarHeader(FStream, DirRec);
end;


Procedure TTarWriter.Finalize;
{
  Writes the End-Of-File Tag
  Data after this tag will be ignored
  The destructor calls this automatically if you didn't do it before
}
Var
  Rec: Array[0..RECORDSIZE-1] of Char;
Begin
  FillChar(Rec, SizeOf(Rec), 0);
  FStream.Write(Rec, RECORDSIZE);
  FFinalized := True;
end;

end.

