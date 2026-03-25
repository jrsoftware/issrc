
unit uPSR_classes;

{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;


procedure RIRegisterTStrings(cl: TPSRuntimeClassImporter; Streams: Boolean);
procedure RIRegisterTStringList(cl: TPSRuntimeClassImporter);
{$IFNDEF PS_MINIVCL}
procedure RIRegisterTBITS(Cl: TPSRuntimeClassImporter);
{$ENDIF}
procedure RIRegisterTSTREAM(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTHANDLESTREAM(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTFILESTREAM(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTSTRINGSTREAM(Cl: TPSRuntimeClassImporter);
{$IFNDEF PS_MINIVCL}
procedure RIRegisterTCUSTOMMEMORYSTREAM(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTMEMORYSTREAM(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTRESOURCESTREAM(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPARSER(Cl: TPSRuntimeClassImporter);
{$IFDEF DELPHI3UP}
procedure RIRegisterTOWNEDCOLLECTION(Cl: TPSRuntimeClassImporter);
{$ENDIF}
procedure RIRegisterTCOLLECTION(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCOLLECTIONITEM(Cl: TPSRuntimeClassImporter);
{$ENDIF}

procedure RIRegister_Classes(Cl: TPSRuntimeClassImporter; Streams: Boolean{$IFDEF D4PLUS}=True{$ENDIF});

implementation
uses
  Classes;

procedure TStringsCapacityR(Self: TStrings; var T: Longint); begin T := Self.Capacity; end;
procedure TStringsCapacityW(Self: TStrings; T: Longint); begin Self.Capacity := T; end;
procedure TStringsDelimiterR(Self: TStrings; var T: char); begin T := Self.Delimiter; end;
procedure TStringsDelimiterW(Self: TStrings; T: char); begin Self.Delimiter:= T; end;
{$IFDEF DELPHI2006UP}
procedure TStringsStrictDelimiterR(Self: TStrings; var T: boolean); begin T := Self.StrictDelimiter; end;
procedure TStringsStrictDelimiterW(Self: TStrings; T: boolean); begin Self.StrictDelimiter:= T; end;
{$ENDIF}
procedure TStringsDelimitedTextR(Self: TStrings; var T: string); begin T := Self.DelimitedText; end;
procedure TStringsDelimitedTextW(Self: TStrings; T: string); begin Self.DelimitedText:= T; end;
procedure TStringsNameValueSeparatorR(Self: TStrings; var T: char); begin T := Self.NameValueSeparator; end;
procedure TStringsNameValueSeparatorW(Self: TStrings; T: char); begin Self.NameValueSeparator:= T; end;
procedure TStringsQuoteCharR(Self: TStrings; var T: char); begin T := Self.QuoteChar; end;
procedure TStringsQuoteCharW(Self: TStrings; T: char); begin Self.QuoteChar:= T; end;

procedure TStringsCountR(Self: TStrings; var T: Longint); begin T := Self.Count; end;

procedure TStringsTextR(Self: TStrings; var T: string); begin T := Self.Text; end;
procedure TStringsTextW(Self: TStrings; T: string); begin Self.Text:= T; end;

procedure TStringsCommaTextR(Self: TStrings; var T: string); begin T := Self.CommaText; end;
procedure TStringsCommaTextW(Self: TStrings; T: string); begin Self.CommaText:= T; end;

procedure TStringsObjectsR(Self: TStrings; var T: TObject; I: Longint);
begin
T := Self.Objects[I];
end;
procedure TStringsObjectsW(Self: TStrings; const T: TObject; I: Longint);
begin
  Self.Objects[I]:= T;
end;

procedure TStringsStringsR(Self: TStrings; var T: string; I: Longint);
begin
T := Self.Strings[I];
end;
procedure TStringsStringsW(Self: TStrings; const T: string; I: Longint);
begin
  Self.Strings[I]:= T;
end;

procedure TStringsNamesR(Self: TStrings; var T: string; I: Longint);
begin
T := Self.Names[I];
end;
procedure TStringsValuesR(Self: TStrings; var T: string; const I: string);
begin
T := Self.Values[I];
end;
procedure TStringsValuesW(Self: TStrings; Const T, I: String);
begin
  Self.Values[I]:= T;
end;

procedure TStringsValueFromIndexR(Self: TStrings; var T: string; const I: Longint);
begin
T := Self.ValueFromIndex[I];
end;
procedure TStringsValueFromIndexW(Self: TStrings; Const T: String; I: Longint);
begin
  Self.ValueFromIndex[I]:= T;
end;

procedure RIRegisterTStrings(cl: TPSRuntimeClassImporter; Streams: Boolean); // requires TPersistent
begin
  with Cl.Add(TStrings) do
  begin
{$IFDEF DELPHI2005UP}
    RegisterConstructor(@TStrings.CREATE, 'Create');
{$ENDIF}

    RegisterVirtualMethod(@TStrings.Add, 'Add');
    RegisterMethod(@TStrings.Append, 'Append');
    RegisterVirtualMethod(@TStrings.AddStrings, 'AddStrings');
    RegisterVirtualAbstractMethod(TStringList, @TStringList.Clear, 'Clear');
    RegisterVirtualAbstractMethod(TStringList, @TStringList.Delete, 'Delete');
    RegisterVirtualMethod(@TStrings.IndexOf, 'IndexOf');
    RegisterVirtualAbstractMethod(TStringList, @TStringList.Insert, 'Insert');
    RegisterPropertyHelper(@TStringsCapacityR, @TStringsCapacityW, 'Capacity');
    RegisterPropertyHelper(@TStringsDelimiterR, @TStringsDelimiterW, 'DELIMITER');
{$IFDEF DELPHI2006UP}
    RegisterPropertyHelper(@TStringsStrictDelimiterR, @TStringsStrictDelimiterW, 'StrictDelimiter');
{$ENDIF}
    RegisterPropertyHelper(@TStringsDelimitedTextR, @TStringsDelimitedTextW, 'DelimitedText');
    RegisterPropertyHelper(@TStringsNameValueSeparatorR, @TStringsNameValueSeparatorW, 'NameValueSeparator');
    RegisterPropertyHelper(@TStringsQuoteCharR, @TStringsQuoteCharW, 'QuoteChar');
    RegisterPropertyHelper(@TStringsCountR, nil, 'Count');
    RegisterPropertyHelper(@TStringsTextR, @TStringsTextW, 'Text');
    RegisterPropertyHelper(@TStringsCommaTextR, @TStringsCommatextW, 'CommaText');
    if Streams then
    begin
      RegisterVirtualMethod(@TStrings.LoadFromFile, 'LoadFromFile');
      RegisterVirtualMethod(@TStrings.SaveToFile, 'SaveToFile');
    end;
    RegisterPropertyHelper(@TStringsStringsR, @TStringsStringsW, 'Strings');
    RegisterPropertyHelper(@TStringsObjectsR, @TStringsObjectsW, 'Objects');

    {$IFNDEF PS_MINIVCL}
    RegisterMethod(@TStrings.BeginUpdate, 'BeginUpdate');
    RegisterMethod(@TStrings.EndUpdate, 'EndUpdate');
    RegisterMethod(@TStrings.Equals,  'Equals');
    RegisterVirtualMethod(@TStrings.Exchange, 'Exchange');
    RegisterMethod(@TStrings.IndexOfName, 'IndexOfName');
    if Streams then
      RegisterVirtualMethod(@TStrings.LoadFromStream, 'LoadFromStream');
    RegisterVirtualMethod(@TStrings.Move, 'Move');
    if Streams then
      RegisterVirtualMethod(@TStrings.SaveToStream, 'SaveToStream');
    RegisterVirtualMethod(@TStrings.SetText, 'SetText');
    RegisterPropertyHelper(@TStringsNamesR, nil, 'Names');
    RegisterPropertyHelper(@TStringsValuesR, @TStringsValuesW, 'Values');
    RegisterPropertyHelper(@TStringsValueFromIndexR, @TStringsValueFromIndexW, 'ValueFromIndex');
    RegisterVirtualMethod(@TSTRINGS.ADDOBJECT, 'AddObject');
    RegisterVirtualMethod(@TSTRINGS.GETTEXT, 'GetText');
    RegisterMethod(@TSTRINGS.INDEXOFOBJECT, 'IndexOfObject');
    RegisterMethod(@TSTRINGS.INSERTOBJECT, 'InsertObject');
    {$ENDIF}
  end;
end;

procedure TSTRINGLISTCASESENSITIVE_R(Self: TSTRINGLIST; var T: BOOLEAN); begin T := Self.CASESENSITIVE; end;
procedure TSTRINGLISTCASESENSITIVE_W(Self: TSTRINGLIST; const T: BOOLEAN); begin Self.CASESENSITIVE := T; end;
procedure TSTRINGLISTDUPLICATES_R(Self: TSTRINGLIST; var T: TDUPLICATES); begin T := Self.DUPLICATES; end;
procedure TSTRINGLISTDUPLICATES_W(Self: TSTRINGLIST; const T: TDUPLICATES); begin Self.DUPLICATES := T; end;
procedure TSTRINGLISTSORTED_R(Self: TSTRINGLIST; var T: BOOLEAN); begin T := Self.SORTED; end;
procedure TSTRINGLISTSORTED_W(Self: TSTRINGLIST; const T: BOOLEAN); begin Self.SORTED := T; end;
procedure TSTRINGLISTONCHANGE_R(Self: TSTRINGLIST; var T: TNOTIFYEVENT);
begin
T := Self.ONCHANGE; end;
procedure TSTRINGLISTONCHANGE_W(Self: TSTRINGLIST; const T: TNOTIFYEVENT);
begin
Self.ONCHANGE := T; end;
procedure TSTRINGLISTONCHANGING_R(Self: TSTRINGLIST; var T: TNOTIFYEVENT); begin T := Self.ONCHANGING; end;
procedure TSTRINGLISTONCHANGING_W(Self: TSTRINGLIST; const T: TNOTIFYEVENT); begin Self.ONCHANGING := T; end;
procedure RIRegisterTSTRINGLIST(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSTRINGLIST) do
  begin
{$IFDEF DELPHI2005UP}
    RegisterConstructor(@TStringList.CREATE, 'Create');
{$ENDIF}
    RegisterVirtualMethod(@TSTRINGLIST.FIND, 'Find');
    RegisterVirtualMethod(@TSTRINGLIST.SORT, 'Sort');
    RegisterPropertyHelper(@TSTRINGLISTCASESENSITIVE_R, @TSTRINGLISTCASESENSITIVE_W, 'CaseSensitive');
    RegisterPropertyHelper(@TSTRINGLISTDUPLICATES_R, @TSTRINGLISTDUPLICATES_W, 'Duplicates');
    RegisterPropertyHelper(@TSTRINGLISTSORTED_R, @TSTRINGLISTSORTED_W, 'Sorted');
    RegisterEventPropertyHelper(@TSTRINGLISTONCHANGE_R, @TSTRINGLISTONCHANGE_W, 'OnChange');
    RegisterEventPropertyHelper(@TSTRINGLISTONCHANGING_R, @TSTRINGLISTONCHANGING_W, 'OnChanging');
  end;
end;

{$IFNDEF PS_MINIVCL}
procedure TBITSBITS_W(Self: TBITS; T: BOOLEAN; t1: INTEGER); begin Self.BITS[t1] := T; end;
procedure TBITSBITS_R(Self: TBITS; var T: BOOLEAN; t1: INTEGER); begin T := Self.Bits[t1]; end;
procedure TBITSSIZE_R(Self: TBITS; T: INTEGER); begin Self.SIZE := T; end;
procedure TBITSSIZE_W(Self: TBITS; var T: INTEGER); begin T := Self.SIZE; end;

procedure RIRegisterTBITS(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TBITS) do
  begin
    RegisterMethod(@TBITS.OPENBIT, 'OpenBit');
    RegisterPropertyHelper(@TBITSBITS_R, @TBITSBITS_W, 'Bits');
    RegisterPropertyHelper(@TBITSSIZE_R, @TBITSSIZE_W, 'Size');
  end;
end;
{$ENDIF}

procedure TSTREAMPOSITION_R(Self: TSTREAM; var T: LONGINT); begin t := Self.POSITION; end;
procedure TSTREAMPOSITION_W(Self: TSTREAM; T: LONGINT); begin Self.POSITION := t; end;
procedure TSTREAMSIZE_R(Self: TSTREAM; var T: LONGINT); begin t := Self.SIZE; end;
{$IFDEF DELPHI3UP}
procedure TSTREAMSIZE_W(Self: TSTREAM; T: LONGINT); begin Self.SIZE := t; end;
{$ENDIF}

procedure RIRegisterTSTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSTREAM) do
  begin
    { uPSC_Classes doesn't turn on IsAbstract on Sydney and newer but here we
      still use RegisterVirtualAbstractMethod because with RegisterVirtualMethod
      it picks the wrong overload, at least for Seek }
    RegisterVirtualAbstractMethod(TMemoryStream, @TMemoryStream.READ, 'Read');
    RegisterVirtualAbstractMethod(TMemoryStream, @TMemoryStream.WRITE, 'Write');
    RegisterVirtualAbstractMethod(TMemoryStream, @TMemoryStream.SEEK, 'Seek');
    RegisterMethod(@TSTREAM.READBUFFER, 'ReadBuffer');
    RegisterMethod(@TSTREAM.WRITEBUFFER, 'WriteBuffer');
    RegisterMethod(@TSTREAM.COPYFROM, 'CopyFrom');
    RegisterPropertyHelper(@TSTREAMPOSITION_R, @TSTREAMPOSITION_W, 'Position');
    RegisterPropertyHelper(@TSTREAMSIZE_R, {$IFDEF DELPHI3UP}@TSTREAMSIZE_W, {$ELSE}nil, {$ENDIF}'Size');
  end;
end;

procedure THANDLESTREAMHANDLE_R(Self: THANDLESTREAM; var T: INTEGER); begin T := Self.HANDLE; end;

procedure RIRegisterTHANDLESTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(THANDLESTREAM) do
  begin
    RegisterConstructor(@THANDLESTREAM.CREATE, 'Create');
    RegisterPropertyHelper(@THANDLESTREAMHANDLE_R, nil, 'Handle');
  end;
end;

{$IFDEF FPC}
// mh: because FPC doesn't handle pointers to overloaded functions
function TFileStreamCreate(filename: string; mode: word): TFileStream;
begin
  result := TFilestream.Create(filename, mode);
end;
{$ENDIF}

procedure RIRegisterTFILESTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFILESTREAM) do
  begin
    {$IFDEF FPC}
    RegisterConstructor(@TFileStreamCreate, 'Create');
    {$ELSE}
    RegisterConstructor(@TFILESTREAM.CREATE, 'Create');
    {$ENDIF}
  end;
end;

{$IFDEF UNICODE}
  {$IFNDEF FPC}
    {$DEFINE STRINGSTREAMFIX}
  {$ENDIF}
{$ENDIF}

{$IFDEF STRINGSTREAMFIX}
function TStringStreamCreateString(AHidden1: Pointer; AHidden2: Byte; const AString: string): TStringStream;
begin
  Result := TStringStream.Create(AString);
end;
{$ENDIF}

procedure RIRegisterTSTRINGSTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSTRINGSTREAM) do
  begin
    RegisterConstructor({$IFDEF STRINGSTREAMFIX}@TStringStreamCreateString{$ELSE}@TSTRINGSTREAM.CREATE{$ENDIF}, 'Create');
  end;
end;

{$IFNDEF PS_MINIVCL}
procedure RIRegisterTCUSTOMMEMORYSTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCUSTOMMEMORYSTREAM) do
  begin
    RegisterMethod(@TCUSTOMMEMORYSTREAM.SAVETOSTREAM, 'SaveToStream');
    RegisterMethod(@TCUSTOMMEMORYSTREAM.SAVETOFILE, 'SaveToFile');
  end;
end;

procedure RIRegisterTMEMORYSTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TMEMORYSTREAM) do
  begin
    RegisterMethod(@TMEMORYSTREAM.CLEAR, 'Clear');
    RegisterMethod(@TMEMORYSTREAM.LOADFROMSTREAM, 'LoadFromStream');
    RegisterMethod(@TMEMORYSTREAM.LOADFROMFILE, 'LoadFromFile');
    RegisterMethod(@TMEMORYSTREAM.SETSIZE, 'SetSize');
  end;
end;

procedure RIRegisterTRESOURCESTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TRESOURCESTREAM) do
  begin
    RegisterConstructor(@TRESOURCESTREAM.CREATE, 'Create');
    RegisterConstructor(@TRESOURCESTREAM.CREATEFROMID, 'CreateFromID');
  end;
end;

procedure TPARSERSOURCELINE_R(Self: TPARSER; var T: INTEGER); begin T := Self.SOURCELINE; end;
procedure TPARSERTOKEN_R(Self: TPARSER; var T: CHAR); begin T := Self.TOKEN; end;

procedure RIRegisterTPARSER(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TPARSER) do
  begin
    RegisterConstructor(@TPARSER.CREATE, 'Create');
    RegisterMethod(@TPARSER.CHECKTOKEN, 'CheckToken');
    RegisterMethod(@TPARSER.CHECKTOKENSYMBOL, 'CheckTokenSymbol');
    RegisterMethod(@TPARSER.ERROR, 'Error');
    RegisterMethod(@TPARSER.ERRORSTR, 'ErrorStr');
    RegisterMethod(@TPARSER.HEXTOBINARY, 'HexToBinary');
    RegisterMethod(@TPARSER.NEXTTOKEN, 'NextToken');
    RegisterMethod(@TPARSER.SOURCEPOS, 'SourcePos');
    RegisterMethod(@TPARSER.TOKENCOMPONENTIDENT, 'TokenComponentIdent');
    RegisterMethod(@TPARSER.TOKENFLOAT, 'TokenFloat');
    RegisterMethod(@TPARSER.TOKENINT, 'TokenInt');
    RegisterMethod(@TPARSER.TOKENSTRING, 'TokenString');
    RegisterMethod(@TPARSER.TOKENSYMBOLIS, 'TokenSymbolIs');
    RegisterPropertyHelper(@TPARSERSOURCELINE_R, nil, 'SourceLine');
    RegisterPropertyHelper(@TPARSERTOKEN_R, nil, 'Token');
  end;
end;

procedure TCOLLECTIONITEMS_W(Self: TCOLLECTION; const T: TCOLLECTIONITEM; const t1: INTEGER);
begin Self.ITEMS[t1] := T; end;

procedure TCOLLECTIONITEMS_R(Self: TCOLLECTION; var T: TCOLLECTIONITEM; const t1: INTEGER);
begin T := Self.ITEMS[t1]; end;

{$IFDEF DELPHI3UP}
procedure TCOLLECTIONITEMCLASS_R(Self: TCOLLECTION; var T: TCOLLECTIONITEMCLASS);
begin T := Self.ITEMCLASS; end;
{$ENDIF}

procedure TCOLLECTIONCOUNT_R(Self: TCOLLECTION; var T: INTEGER);
begin T := Self.COUNT; end;

{$IFDEF DELPHI3UP}
procedure TCOLLECTIONITEMDISPLAYNAME_W(Self: TCOLLECTIONITEM; const T: STRING);
begin Self.DISPLAYNAME := T; end;
{$ENDIF}

{$IFDEF DELPHI3UP}
procedure TCOLLECTIONITEMDISPLAYNAME_R(Self: TCOLLECTIONITEM; var T: STRING);
begin T := Self.DISPLAYNAME; end;
{$ENDIF}

procedure TCOLLECTIONITEMINDEX_W(Self: TCOLLECTIONITEM; const T: INTEGER);
begin Self.INDEX := T; end;

procedure TCOLLECTIONITEMINDEX_R(Self: TCOLLECTIONITEM; var T: INTEGER);
begin T := Self.INDEX; end;

{$IFDEF DELPHI3UP}
procedure TCOLLECTIONITEMID_R(Self: TCOLLECTIONITEM; var T: INTEGER);
begin T := Self.ID; end;
{$ENDIF}

procedure TCOLLECTIONITEMCOLLECTION_W(Self: TCOLLECTIONITEM; const T: TCOLLECTION);
begin Self.COLLECTION := T; end;

procedure TCOLLECTIONITEMCOLLECTION_R(Self: TCOLLECTIONITEM; var T: TCOLLECTION);
begin T := Self.COLLECTION; end;

{$IFDEF DELPHI3UP}
procedure RIRegisterTOWNEDCOLLECTION(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TOWNEDCOLLECTION) do
  begin
  RegisterConstructor(@TOWNEDCOLLECTION.CREATE, 'Create');
  end;
end;
{$ENDIF}

procedure RIRegisterTCOLLECTION(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TCOLLECTION) do
  begin
  RegisterConstructor(@TCOLLECTION.CREATE, 'Create');
{$IFDEF DELPHI6UP}  {$IFNDEF FPC} RegisterMethod(@TCOLLECTION.OWNER, 'Owner'); {$ENDIF} {$ENDIF} // no owner in FPC
  RegisterMethod(@TCOLLECTION.ADD, 'Add');
  RegisterVirtualMethod(@TCOLLECTION.BEGINUPDATE, 'BeginUpdate');
  RegisterMethod(@TCOLLECTION.CLEAR, 'Clear');
{$IFDEF DELPHI5UP}  RegisterMethod(@TCOLLECTION.DELETE, 'Delete'); {$ENDIF}
  RegisterVirtualMethod(@TCOLLECTION.ENDUPDATE, 'EndUpdate');
{$IFDEF DELPHI3UP}  RegisterMethod(@TCOLLECTION.FINDITEMID, 'FindItemID'); {$ENDIF}
{$IFDEF DELPHI3UP}  RegisterMethod(@TCOLLECTION.INSERT, 'Insert'); {$ENDIF}
  RegisterPropertyHelper(@TCOLLECTIONCOUNT_R,nil,'Count');
{$IFDEF DELPHI3UP}  RegisterPropertyHelper(@TCOLLECTIONITEMCLASS_R,nil,'ItemClass'); {$ENDIF}
  RegisterPropertyHelper(@TCOLLECTIONITEMS_R,@TCOLLECTIONITEMS_W,'Items');
  end;
end;

procedure RIRegisterTCOLLECTIONITEM(Cl: TPSRuntimeClassImporter);
Begin
with Cl.Add(TCOLLECTIONITEM) do
  begin
  RegisterVirtualConstructor(@TCOLLECTIONITEM.CREATE, 'Create');
  RegisterPropertyHelper(@TCOLLECTIONITEMCOLLECTION_R,@TCOLLECTIONITEMCOLLECTION_W,'Collection');
{$IFDEF DELPHI3UP}  RegisterPropertyHelper(@TCOLLECTIONITEMID_R,nil,'ID'); {$ENDIF}
  RegisterPropertyHelper(@TCOLLECTIONITEMINDEX_R,@TCOLLECTIONITEMINDEX_W,'Index');
{$IFDEF DELPHI3UP}  RegisterPropertyHelper(@TCOLLECTIONITEMDISPLAYNAME_R,@TCOLLECTIONITEMDISPLAYNAME_W,'DisplayName'); {$ENDIF}
  end;
end;
{$ENDIF}

procedure RIRegister_Classes(Cl: TPSRuntimeClassImporter; Streams: Boolean);
begin
  if Streams then
    RIRegisterTSTREAM(Cl);
  RIRegisterTStrings(cl, Streams);
  RIRegisterTStringList(cl);
  {$IFNDEF PS_MINIVCL}
  RIRegisterTBITS(cl);
  {$ENDIF}
  if Streams then
  begin
    RIRegisterTHANDLESTREAM(Cl);
    RIRegisterTFILESTREAM(Cl);
    RIRegisterTSTRINGSTREAM(Cl);
    {$IFNDEF PS_MINIVCL}
    RIRegisterTCUSTOMMEMORYSTREAM(Cl);
    RIRegisterTMEMORYSTREAM(Cl);
    RIRegisterTRESOURCESTREAM(Cl);
    {$ENDIF}
  end;
  {$IFNDEF PS_MINIVCL}
  RIRegisterTPARSER(Cl);
  RIRegisterTCOLLECTIONITEM(Cl);
  RIRegisterTCOLLECTION(Cl);
  {$IFDEF DELPHI3UP}
  RIRegisterTOWNEDCOLLECTION(Cl);
  {$ENDIF}
  {$ENDIF}
end;

// PS_MINIVCL changes by Martijn Laan

end.
