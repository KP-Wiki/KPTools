Unit KP_ToolUtils;

Interface

Uses
  Registry, ShlObj, Windows, vcl.Forms, StrUtils, SysUtils, Classes, Math;

Const
  UNITSUFFIXBYTE: Array[0..7] of String = ( 'B', 'KB', 'MB', 'GB', 'TB', 'EB', 'ZB', 'YB' );
  UNITSUFFIX: Array[0..7] of String = ( '', 'K', 'M', 'G', 'T', 'E', 'Z', 'Y' );
  CONVERTBYTEVAL: Word = 1024;


Procedure RegisterFileType;
Procedure Split(Delimiter: Char; Str: String; ListOfStrings: TStrings) ;
Function GetFiles(Const StartDir: String; Const FileList, DirList: TStrings): Boolean;
Function convertUnits(aValueToConvert: Int64; aBaseValue: Integer): String;

Implementation

Procedure RegisterFileType;
Var
  Reg: TRegistry;
Begin
  Reg := TRegistry.Create;
  Try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey('.kpmap', True); // Create a new key named to our file extention
    Reg.WriteString('', 'MapInstaller.kpmap'); // This adds HKEY_CLASSES_ROOT\.kpmap\(Default) = 'MapInstaller.kpmap'
    Reg.CloseKey;
    Reg.OpenKey('MapInstaller.kpmap', True); // This adds HKEY_CLASSES_ROOT\MapInstaller.kpmap\(Default) = 'MapInstaller File'
    Reg.WriteString('', 'Knights Province Map Installer File');
    Reg.CloseKey;
    Reg.OpenKey('MapInstaller.kpmap\DefaultIcon', True); // This adds HKEY_CLASSES_ROOT\MapInstaller.kpmap\DefaultIcon\(Default) = 'Application Dir\MapInstaller.exe,0'
    Reg.WriteString('', Application.ExeName + ',0');
    Reg.CloseKey;
    Reg.OpenKey('MapInstaller.kpmap\Shell\Open', True); // Now write the open action in explorer
    Reg.WriteString('', '&Open');
    Reg.CloseKey;
    Reg.OpenKey('MapInstaller.kpmap\Shell\Open\Command', True); // This adds HKEY_CLASSES_ROOT\MapInstaller.kpmap\Shell\Open\Command (Default) = '"Application Dir\Project1.exe" "%1"'
    Reg.WriteString('', '"' + Application.ExeName + '" "%1"');
    Reg.CloseKey;
    // Finally, we want the Windows Explorer to realize we added our file type by using the SHChangeNotify API.
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  Finally
    FreeAndNil(Reg);
  end;
end;

// Split text to string list
Procedure Split(Delimiter: Char; Str: String; ListOfStrings: TStrings) ;
Begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter       := Delimiter;
  ListOfStrings.StrictDelimiter := True; // Requires Delphi 2006 or newer.
  ListOfStrings.DelimitedText   := Str;
end;

Function GetFiles(Const StartDir: String; Const FileList, DirList: TStrings): Boolean;
Var
  SRec, SubDirSRec: TSearchRec;
  Res, SDRes: Integer;
Begin
  if not Assigned(FileList) then // List gets written to, therefor must exist.
  Begin
    Result := False;
    Exit;
  end;
  Res := FindFirst(StartDir + '*.*', faAnyfile, SRec);
  if Res = 0 then
  Try
    While Res = 0 do
    Begin
      if (SRec.Attr and faDirectory <> faDirectory) then
        FileList.Add(SRec.Name) // We only need the filename
      else if (SRec.Attr and faDirectory = faDirectory) and not (SRec.Name = '..') then // Who needs parent dirs anyways? We create a new file structure in Tar.
      Begin
        DirList.Add(SRec.Name);
        if not (SRec.Name = '.') then // Search for more files in sub-Directories.
        Begin
          SDRes := FindFirst(StartDir + SRec.Name + PathDelim + '*.*', faAnyfile, SubDirSRec);
          if SDRes = 0 then
          Try
            While SDRes = 0 do
            Begin
              if (SubDirSRec.Attr and faDirectory <> faDirectory)  then
                FileList.Add(SRec.Name + '/' + SubDirSRec.Name); // We only need the filename
              SDRes := FindNext(SubDirSRec);
            end;
          Finally
            FindClose(SubDirSRec)
          end;
        end;
      end;
      Res := FindNext(SRec);
    end;
  finally
    FindClose(SRec)
  end;
  Result := (FileList.Count > 0);
end;

Function convertUnits(aValueToConvert: Int64; aBaseValue: Integer): string;
Var
  fResult: String;
  ConvertedValue: Integer;
Begin
  if (aBaseValue = CONVERTBYTEVAL) then // Byte conversion
  Begin
    if (aValueToConvert < aBaseValue) then // This fixes a syntax error(Zero division)
      fResult := FloatToStr(RoundTo(aValueToConvert, -2)) + ' ' + UNITSUFFIXBYTE[0]
    else
    Begin
      ConvertedValue := round(min(logn(aBaseValue, aValueToConvert), High(UNITSUFFIXBYTE)));
      fResult := FloatToStr(RoundTo((aValueToConvert / power(aBaseValue, ConvertedValue)), -2)) + ' ' + UNITSUFFIXBYTE[ConvertedValue];
    end;
  end else // All others (Metric)
  Begin
    if (aValueToConvert < aBaseValue) then // This fixes a syntax error(Zero division)
      fResult := FloatToStr(RoundTo(aValueToConvert, -2)) + ' ' + UNITSUFFIX[0]
    else
    Begin
      ConvertedValue := round(min(logn(aBaseValue, aValueToConvert), High(UNITSUFFIX)));
      fResult := FloatToStr(RoundTo((aValueToConvert / power(aBaseValue, ConvertedValue)), -2)) + ' ' + UNITSUFFIX[ConvertedValue];
    end;
  end;
  result := fResult;
end;

end.