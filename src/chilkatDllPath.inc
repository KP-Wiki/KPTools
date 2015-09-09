
{$IFDEF WIN32}
const DLLName = 'ChilkatDelphiXE.dll';
{$ELSE}
  {$IFDEF WIN64}
  const DLLName = 'ChilkatDelphiXE64.dll';
  {$ELSE}
  const DLLName = 'ChilkatDelphiXE.dll';  //Older versions of Delphi
  {$ENDIF}
{$ENDIF}

