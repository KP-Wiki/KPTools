Unit KP_ToolUtils;

Interface

Uses
  Windows, SysUtils, Variants, Classes, Math;

Const
  UNITSUFFIXBYTE: Array[0..7] of String = ( 'B', 'KB', 'MB', 'GB', 'TB', 'EB', 'ZB', 'YB' );
  UNITSUFFIX: Array[0..7] of String = ( '', 'K', 'M', 'G', 'T', 'E', 'Z', 'Y' );
  CONVERTBYTEVAL: Word = 1024;

Function GetFiles(Const StartDir: String; Const FileList, DirList: TStrings): Boolean;
Function convertUnits(aValueToConvert: Int64; aBaseValue: Integer): String;
Function GetEnvVarValue(Const VarName: String): String;
Function Randomstring(strLen: Integer): string;

Implementation

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
      else
      begin
        if (SRec.Attr and faDirectory = faDirectory) and not (SRec.Name = '..') then // Who needs parent dirs anyways? We create a new file structure in Tar.
        Begin
          DirList.Add(SRec.Name);

          if not (SRec.Name = '.') then // Search for more files in sub-Directories.
          Begin
            SDRes := FindFirst(StartDir + SRec.Name + PathDelim + '*.*', faAnyfile, SubDirSRec);

            if SDRes = 0 then
            Try
              While SDRes = 0 do
              Begin
                if (SubDirSRec.Attr and faDirectory <> faDirectory) then
                  FileList.Add(SRec.Name + '/' + SubDirSRec.Name); // We only need the filename
                SDRes := FindNext(SubDirSRec);
              end;
            Finally
              FindClose(SubDirSRec)
            end;
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
  end
  else // All others (Metric)
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


Function GetEnvVarValue(Const VarName: String): String;
Var
  BufSize: Integer;  // buffer size required for value
Begin
  // Get required buffer size (inc. terminal #0)
  BufSize := GetEnvironmentVariable(
    PChar(VarName), nil, 0);

  if BufSize > 0 then
  Begin
    // Read env var value into result string
    SetLength(Result, BufSize - 1);
    GetEnvironmentVariable(PChar(VarName),
      PChar(Result), BufSize);
  end
  else
    // No such environment variable
    Result := '';
end;


function Randomstring(strLen: Integer): string;
var
  str: string;
begin
  Randomize;
  str := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Result := '';
  repeat
    Result := Result + str[Random(Length(str)) + 1];
  until
    (Length(Result) = strLen);
end;

end.