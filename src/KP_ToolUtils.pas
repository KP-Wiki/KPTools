unit KP_ToolUtils;

interface
uses
  Registry, ShlObj, Windows, vcl.Forms, StrUtils, SysUtils, Classes, Math;

const
  UNITSUFFIXBYTE: array[0..7] of string = ( 'B', 'KB', 'MB', 'GB', 'TB', 'EB', 'ZB', 'YB' );
  UNITSUFFIX: array[0..7] of string = ( '', 'K', 'M', 'G', 'T', 'E', 'Z', 'Y' );
  CONVERTBYTEVAL: Word = 1024;


procedure RegisterFileType;
procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
function  GetFiles(const StartDir: String; const List: TStrings): Boolean;
function convertUnits(aValueToConvert: Int64; aBaseValue: Integer): string;

implementation

procedure RegisterFileType;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
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
  finally
    FreeAndNil(Reg);
  end;
end;

// Split text to string list
procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter       := Delimiter;
  ListOfStrings.StrictDelimiter := True; // Requires Delphi 2006 or newer.
  ListOfStrings.DelimitedText   := Str;
end;

function  GetFiles(const StartDir: String; const List: TStrings): Boolean;
var
  SRec: TSearchRec;
  Res: Integer;
begin
  if not Assigned(List) then // List gets written to, therefor must exist.
  begin
    Result := False;
    Exit;
  end;
  Res := FindFirst(StartDir + '*.*', faAnyfile, SRec);
  if Res = 0 then
  try
    while res = 0 do
    begin
      if (SRec.Attr and faDirectory <> faDirectory) then
        List.Add(SRec.Name); // We only need the filename
      Res := FindNext(SRec);
    end;
  finally
    FindClose(SRec)
  end;
  Result := (List.Count > 0);
end;

function convertUnits(aValueToConvert: Int64; aBaseValue: Integer): string;
var
  fResult: string;
  ConvertedValue: Integer;
begin
  if (aBaseValue = CONVERTBYTEVAL) then // Byte conversion
  begin
    if (aValueToConvert < aBaseValue) then // This fixes a syntax error(Zero division)
      fResult := FloatToStr(RoundTo(aValueToConvert, -2)) + ' ' + UNITSUFFIXBYTE[0]
    else
    begin
      ConvertedValue := round(min(logn(aBaseValue, aValueToConvert), High(UNITSUFFIXBYTE)));
      fResult := FloatToStr(RoundTo((aValueToConvert / power(aBaseValue, ConvertedValue)), -2)) + ' ' + UNITSUFFIXBYTE[ConvertedValue];
    end;
  end else // All others (Metric)
  begin
    if (aValueToConvert < aBaseValue) then // This fixes a syntax error(Zero division)
      fResult := FloatToStr(RoundTo(aValueToConvert, -2)) + ' ' + UNITSUFFIX[0]
    else
    begin
      ConvertedValue := round(min(logn(aBaseValue, aValueToConvert), High(UNITSUFFIX)));
      fResult := FloatToStr(RoundTo((aValueToConvert / power(aBaseValue, ConvertedValue)), -2)) + ' ' + UNITSUFFIX[ConvertedValue];
    end;
  end;
  result := fResult;
end;

end.