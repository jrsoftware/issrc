program ISHelpGen;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  Classes,
  ActiveX,
  ComObj,
  TypInfo,
  XMLParse in 'XMLParse.pas',
  UIsxclassesParser in 'UIsxclassesParser.pas',
  PathFunc in '..\..\Components\PathFunc.pas';

const
  Version = '1.17';

  XMLFileVersion = '1';

  SNewLine = #13#10;

type
  TElement = (
    el_Text,
    elA,
    elAnchorLink,
    elB,
    elBody,
    elBR,
    elContents,
    elContentsHeading,
    elContentsTopic,
    elDD,
    elDL,
    elDT,
    elExample,
    elExamples,
    elExtLink,
    elFlag,
    elFlagList,
    elHeading,
    elI,
    elIndent,
    elKeyword,
    elLI,
    elLink,
    elOL,
    elP,
    elParam,
    elParamList,
    elPre,
    elPreCode,
    elSetupDefault,
    elSetupFormat,
    elSetupValid,
    elSetupTopic,
    elSmall,
    elTable,
    elTD,
    elTopic,
    elTR,
    elTT,
    elU,
    elUL);
  TElementSet = set of TElement;

  TKeywordInfo = class
  public
    Topic, Anchor: String;
  end;

var
  SourceDir, OutputDir: String;
  NoContentsHtm: Boolean;
  Keywords, DefinedTopics, TargetTopics, SetupDirectives: TStringList;
  TopicsGenerated: Integer = 0;
  CurrentTopicName: String;
  CurrentListIsCompact: Boolean;
  CurrentTableColumnIndex: Integer;

procedure UnexpectedElementError(const Node: IXMLNode);
begin
  raise Exception.CreateFmt('Element "%s" is unexpected here', [Node.NodeName]);
end;

function ElementFromNode(const Node: IXMLNode): TElement;
var
  I: Integer;
begin
  case Node.NodeType of
    NODE_ELEMENT:
      begin
        I := GetEnumValue(TypeInfo(TElement), 'el' + Node.NodeName);
        if I < 0 then
          raise Exception.CreateFmt('Unknown element "%s"', [Node.NodeName]);
        Result := TElement(I);
      end;
    NODE_TEXT, NODE_ENTITY_REFERENCE: Result := el_Text;
  else
    raise Exception.CreateFmt('ElementFromNode: Unknown node type %d', [Node.NodeType]);
  end;
end;

function IsWhitespace(const Node: IXMLNode): Boolean;
{ Returns True if the node is text that consists only of whitespace }
var
  S: String;
  I: Integer;
begin
  Result := False;
  if Node.NodeType = NODE_TEXT then begin
    S := Node.Text;
    for I := 1 to Length(S) do
      if not CharInSet(S[I], [#9, #10, ' ']) then
        Exit;
    Result := True;
  end;
end;

function IsFirstNonWhitespaceNode(Node: IXMLNode): Boolean;
{ Returns True if there are no preceding non-whitespace sibling elements }
begin
  repeat
    Node := Node.PreviousSibling;
  until (Node = nil) or not IsWhitespace(Node);
  Result := (Node = nil);
end;

function IsLastNonWhitespaceNode(Node: IXMLNode): Boolean;
{ Returns True if no non-whitespace sibling elements follow }
begin
  repeat
    Node := Node.NextSibling;
  until (Node = nil) or not IsWhitespace(Node);
  Result := (Node = nil);
end;

function NodeHasChildren(Node: IXMLNode): Boolean;
{ Returns True if the node has non-whitespace children }
begin
  Node := Node.GetFirstChild;
  while Assigned(Node) do begin
    if not IsWhitespace(Node) then begin
      Result := True;
      Exit;
    end;
    Node := Node.NextSibling;
  end;
  Result := False;
end;

function ListItemExists(const SL: TStrings; const S: String): Boolean;
var
  I: Integer;
begin
  for I := 0 to SL.Count-1 do
    if SL[I] = S then begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function StringChange(var S: String; const FromStr, ToStr: String): Integer;
var
  FromStrLen, I, EndPos, J: Integer;
  IsMatch: Boolean;
label 1;
begin
  Result := 0;
  if FromStr = '' then Exit;
  FromStrLen := Length(FromStr);
  I := 1;
1:EndPos := Length(S) - FromStrLen + 1;
  while I <= EndPos do begin
    IsMatch := True;
    J := 0;
    while J < FromStrLen do begin
      if S[J+I] <> FromStr[J+1] then begin
        IsMatch := False;
        Break;
      end;
      Inc(J);
    end;
    if IsMatch then begin
      Inc(Result);
      Delete(S, I, FromStrLen);
      Insert(ToStr, S, I);
      Inc(I, Length(ToStr));
      goto 1;
    end;
    Inc(I);
  end;
end;

procedure SaveStringToFile(const S, Filename: String);
var
  F: TFileStream;
  U: UTF8String;
begin
  F := TFileStream.Create(Filename, fmCreate);
  try
    U := UTF8String(S);
    F.WriteBuffer(U[1], Length(U));
  finally
    F.Free;
  end;
end;

function EscapeHTML(const S: String; const EscapeDoubleQuotes: Boolean = True): String;
begin
  Result := S;
  StringChange(Result, '&', '&amp;');
  StringChange(Result, '<', '&lt;');
  StringChange(Result, '>', '&gt;');
  if EscapeDoubleQuotes then
    StringChange(Result, '"', '&quot;');
  { Also convert the Unicode representation of a non-breaking space into &nbsp;
    so it's easily to tell them apart from normal spaces when viewing the
    generated HTML source }
  StringChange(Result, #$00A0, '&nbsp;');
end;

procedure CheckTopicNameValidity(const TopicName: String);
var
  I: Integer;
begin
  if TopicName = '' then
    raise Exception.Create('Topic name cannot be empty');
  { Security: Make sure topic names don't include slashes etc. }
  for I := 1 to Length(TopicName) do
    if not CharInSet(TopicName[I], ['A'..'Z', 'a'..'z', '0'..'9', '_', '-']) then
      raise Exception.CreateFmt('Topic name "%s" includes invalid characters', [TopicName]);
end;

procedure CheckAnchorNameValidity(const AnchorName: String);
var
  I: Integer;
begin
  if AnchorName = '' then
    raise Exception.Create('Anchor name cannot be empty');
  for I := 1 to Length(AnchorName) do
    if not CharInSet(AnchorName[I], ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', '.']) then
      raise Exception.CreateFmt('Anchor name "%s" includes invalid characters', [AnchorName]);
end;

function GenerateTopicFilename(const TopicName: String): String;
begin
  CheckTopicNameValidity(TopicName);
  Result := 'topic_' + Lowercase(TopicName) + '.htm';
end;

function GenerateTopicLink(const TopicName, AnchorName: String): String;
begin
  if TopicName <> '' then
    Result := GenerateTopicFileName(TopicName)
  else begin
    Result := '';
    if AnchorName = '' then
      raise Exception.Create('Cannot create link with neither a target topic nor anchor');
  end;
  if AnchorName <> '' then begin
    CheckAnchorNameValidity(AnchorName);
    Result := Result + '#' + AnchorName;
  end;
end;

function GenerateAnchorHTML(const AnchorName, InnerContents: String): String;
{ Generates HTML for an anchor on the current topic, also updating
  DefinedTopics and checking for duplicates }
var
  S: String;
begin
  if CurrentTopicName = '' then
    raise Exception.Create('Cannot create anchor outside of topic');
  CheckAnchorNameValidity(AnchorName);

  S := CurrentTopicName + '#' + AnchorName;
  if ListItemExists(DefinedTopics, S) then
    raise Exception.CreateFmt('Anchor name "%s" in topic "%s" defined more than once',
      [AnchorName, CurrentTopicName]);
  DefinedTopics.Add(S);

  Result := Format('<a name="%s">%s</a>', [EscapeHTML(AnchorName), InnerContents]);
end;

function GenerateTopicLinkHTML(const TopicName, AnchorName, InnerContents: String): String;
{ Generates HTML for a link to a topic and/or anchor, also updating
  TargetTopics }
var
  S: String;
begin
  if TopicName <> '' then
    S := TopicName
  else begin
    S := CurrentTopicName;
    if S = '' then
      raise Exception.Create('Cannot create link outside of topic with empty target topic');
    if AnchorName = '' then
      raise Exception.Create('Cannot create link with neither a target topic nor anchor');
  end;
  CheckTopicNameValidity(S);
  if AnchorName <> '' then begin
    CheckAnchorNameValidity(AnchorName);
    S := S + '#' + AnchorName;
  end;
  if not ListItemExists(TargetTopics, S) then
    TargetTopics.Add(S);

  Result := Format('<a href="%s">%s</a>',
    [EscapeHTML(GenerateTopicLink(TopicName, AnchorName)), InnerContents]);
end;

procedure CreateKeyword(const AKeyword, ATopicName, AAnchorName: String);
var
  KeywordInfo: TKeywordInfo;
begin
  KeywordInfo := TKeywordInfo.Create;
  KeywordInfo.Topic := ATopicName;
  KeywordInfo.Anchor := AAnchorName;
  Keywords.AddObject(AKeyword, KeywordInfo);
end;

function ParseFormattedText(Node: IXMLNode): String;
var
  S: String;
  I: Integer;
  B: Boolean;
begin
  Result := '';
  Node := Node.FirstChild;
  while Assigned(Node) do begin
    case ElementFromNode(Node) of
      el_Text:
        Result := Result + EscapeHTML(Node.Text, False);
      elA:
        begin
          S := Node.Attributes['name'];
          Result := Result + GenerateAnchorHTML(S, ParseFormattedText(Node));
        end;
      elAnchorLink:
        begin
          S := Node.Attributes['name'];
          Result := Result + GenerateTopicLinkHTML('', S, ParseFormattedText(Node));
        end;
      elB:
        Result := Result + '<b>' + ParseFormattedText(Node) + '</b>';
      elBR:
        Result := Result + '<br/>';
      elDD:
        Result := Result + '<dd>' + ParseFormattedText(Node) + '</dd>';
      elDL:
        Result := Result + '<dl>' + ParseFormattedText(Node) + '</dl>';
      elDT:
        Result := Result + '<dt>' + ParseFormattedText(Node) + '</dt>';
      elExample:
        Result := Result + '<div class="examplebox">' + SNewLine +
          '<div class="exampleheader">Example:</div>' + ParseFormattedText(Node) + '</div>';
      elExamples:
        Result := Result + '<div class="examplebox">' + SNewLine +
          '<div class="exampleheader">Examples:</div>' + ParseFormattedText(Node) + '</div>';
      elFlag:
        begin
          S := Node.Attributes['name'];
          if CurrentTopicName = '' then
            raise Exception.Create('<flag> used outside of topic');
          CreateKeyword(S, CurrentTopicName, S);
          Result := Result + '<dt class="flaglist">' + GenerateAnchorHTML(S, EscapeHTML(S)) +
            '</dt>' + SNewLine + '<dd>' + ParseFormattedText(Node) +
            '</dd>';
        end;
      elFlagList:
        Result := Result + '<dl>' + ParseFormattedText(Node) + '</dl>';
      elI:
        Result := Result + '<i>' + ParseFormattedText(Node) + '</i>';
      elIndent:
        Result := Result + '<div class="indent">' + ParseFormattedText(Node) + '</div>';
      elLI:
        begin
          Result := Result + '<li';
          if CurrentListIsCompact then
            Result := Result + ' class="compact"';
          Result := Result + '>' + ParseFormattedText(Node) + '</li>';
        end;
      elLink:
        begin
          S := Node.Attributes['topic'];
          Result := Result + GenerateTopicLinkHTML(S, Node.OptionalAttributes['anchor'],
            ParseFormattedText(Node));
        end;
      elExtLink:
        begin
          S := EscapeHTML(Node.Attributes['href']);
          if Pos('ms-its:', S) = 1 then
            Result := Result + Format('<a href="%s">%s</a>', [S, ParseFormattedText(Node)])
          else
            Result := Result + Format('<a href="%s" target="_blank" title="%s">%s</a><img src="images/extlink.svg" alt=" [external link]" />',
              [S, S, ParseFormattedText(Node)]);
        end;
      elHeading:
        begin
          if IsFirstNonWhitespaceNode(Node) then
            Result := Result + '<h2 class="heading notopmargin">'
          else
            Result := Result + '<h2 class="heading">';
          Result := Result + ParseFormattedText(Node) + '</h2>';
        end;
      elOL:
        Result := Result + '<ol>' + ParseFormattedText(Node) + '</ol>';
      elP:
        begin
          if Node.HasAttribute('margin') and (Node.Attributes['margin'] = 'no') then
            Result := Result + '<div>' + ParseFormattedText(Node) + '</div>'
          else
            Result := Result + '<p>' + ParseFormattedText(Node) + '</p>';
        end;
      elParam:
        begin
          { IE doesn't support immediate-child-only selectors in CSS (e.g.
            "DL.paramlist > DT") so we have to apply the class to each DT
            instead of just on the DL. }
          S := Node.Attributes['name'];
          if CurrentTopicName = '' then
            raise Exception.Create('<param> used outside of topic');
          CreateKeyword(S, CurrentTopicName, S);
          Result := Result + '<dt class="paramlist"><b>' + GenerateAnchorHTML(S, EscapeHTML(S)) + '</b>';
          if Node.Attributes['required'] = 'yes' then
            Result := Result + ' &nbsp;<i>(Required)</i>';
          Result := Result + '</dt><dd class="paramlist">' + ParseFormattedText(Node) + '</dd>';
        end;
      elParamList:
        Result := Result + '<dl>' + ParseFormattedText(Node) + '</dl>';
      elPre:
        begin
          Result := Result + '<pre';
          { Special handling for <pre> inside example boxes: Don't include a
            bottom margin if <pre> is the last element } 
          if (ElementFromNode(Node.ParentNode) in [elExample, elExamples]) and
             IsLastNonWhitespaceNode(Node) then
            Result := Result + ' class="nomargin"';
          Result := Result + '>' + ParseFormattedText(Node) + '</pre>';
        end;
      elPreCode:
        Result := Result + '<pre class="indent examplebox">' + ParseFormattedText(Node) + '</pre>';
      elSmall:
        Result := Result + '<span class="small">' + ParseFormattedText(Node) + '</span>';
      elTable:
        Result := Result + '<table>' + ParseFormattedText(Node) + '</table>';
      elTD:
        begin
          Result := Result + '<td';
          if CurrentTableColumnIndex = 0 then
            Result := Result + ' class="cellleft"'
          else
            Result := Result + ' class="cellright"';
          Result := Result + '>' + ParseFormattedText(Node) + '</td>';
          Inc(CurrentTableColumnIndex);
        end;
      elTR:
        begin
          I := CurrentTableColumnIndex;
          CurrentTableColumnIndex := 0;
          Result := Result + '<tr>' + ParseFormattedText(Node) + '</tr>';
          CurrentTableColumnIndex := I;
        end;
      elTT:
        Result := Result + '<tt>' + ParseFormattedText(Node) + '</tt>';
      elU:
        Result := Result + '<u>' + ParseFormattedText(Node) + '</u>';
      elUL:
        begin
          B := CurrentListIsCompact;
          CurrentListIsCompact := (Node.HasAttribute('appearance') and (Node.Attributes['appearance'] = 'compact'));
          Result := Result + '<ul>' + ParseFormattedText(Node) + '</ul>';
          CurrentListIsCompact := B;
        end;
    else
      UnexpectedElementError(Node);
    end;
    Node := Node.NextSibling;
  end;
end;

function GenerateSetupDirectiveTopicName(const Directive: String): String;
begin
  Result := 'setup_' + Lowercase(Directive);
end;

procedure ParseTopic(const TopicNode: IXMLNode; const SetupTopic: Boolean);
var
  TopicDirective, TopicName, TopicTitle: String;
  BodyText, SetupFormatText, SetupValidText, SetupDefaultText, S: String;
  Node: IXMLNode;
begin
  if not SetupTopic then begin
    TopicName := TopicNode.Attributes['name'];
    TopicTitle := TopicNode.Attributes['title'];
  end
  else begin
    TopicDirective := TopicNode.Attributes['directive'];
    TopicName := GenerateSetupDirectiveTopicName(TopicDirective);
    CreateKeyword(TopicDirective, TopicName, '');
    if TopicNode.HasAttribute('title') then
      TopicTitle := '[Setup]: ' + TopicNode.Attributes['title']
    else
      TopicTitle := '[Setup]: ' + TopicDirective;
  end;

  CheckTopicNameValidity(TopicName);
  if ListItemExists(DefinedTopics, TopicName) then
    raise Exception.CreateFmt('Topic "%s" defined more than once', [TopicName]);
  DefinedTopics.Add(TopicName);

  CurrentTopicName := TopicName;

  Node := TopicNode.FirstChild;
  while Assigned(Node) do begin
    if not IsWhitespace(Node) then begin
      case ElementFromNode(Node) of
        elBody:
          BodyText := ParseFormattedText(Node);
        elKeyword:
          CreateKeyword(Node.Attributes['value'], TopicName, Node.OptionalAttributes['anchor']);
        elSetupDefault:
          begin
            if not SetupTopic then
              raise Exception.Create('<setupdefault> is only valid inside <setuptopic>');
            { <div class="margined"> is used instead of <p> since the data could
              contain <p>'s of its own, which can't be nested.
              NOTE: The space before </div> is intentional -- as noted in
              styles.css, "vertical-align: baseline" doesn't work right on IE6,
              but putting a space before </div> works around the problem, at
              least when it comes to lining up normal text with a single line
              of monospaced text. }
            SetupDefaultText := '<tr><td class="setuphdrl"><p>Default value:</p></td>' +
              '<td class="setuphdrr"><div class="margined">' + ParseFormattedText(Node) +
               ' </div></td></tr>' + SNewLine;
          end;
        elSetupFormat:
          begin
            if not SetupTopic then
              raise Exception.Create('<setupformat> is only valid inside <setuptopic>');
            { See comments above! }
            SetupFormatText := '<tr><td class="setuphdrl"><p>Format:</p></td>' +
              '<td class="setuphdrr"><div class="margined">' + ParseFormattedText(Node) +
              ' </div></td></tr>' + SNewLine;
          end;
        elSetupValid:
          begin
            if not SetupTopic then
              raise Exception.Create('<setupvalid> is only valid inside <setuptopic>');
            { See comments above! }
            SetupValidText := '<tr><td class="setuphdrl"><p>Valid values:</p></td>' +
              '<td class="setuphdrr"><div class="margined">' + ParseFormattedText(Node) +
              ' </div></td></tr>' + SNewLine;
          end;
      else
        UnexpectedElementError(Node);
      end;
    end;
    Node := Node.NextSibling;
  end;

  CurrentTopicName := '';

  S :=
    '<!DOCTYPE html>' + SNewLine +
    '<html lang="en">' + SNewLine +
    '<head>' + SNewLine +
    '<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />' + SNewLine +
    '<meta http-equiv="X-UA-Compatible" content="IE=11" />' + SNewLine +
    '<title>' + EscapeHTML(TopicTitle, False) + '</title>' + SNewLine +
    '<link rel="stylesheet" type="text/css" href="styles.css" />' + SNewLine +
    '<script type="text/javascript" src="topic.js"></script>' + SNewLine +
    '</head>' + SNewLine +
    '<body>' + SNewLine +
    '<h1 class="topicheading">' + EscapeHTML(TopicTitle, False) + '</h1>' + SNewLine +
    '<div class="topicbody">';

  if TopicName = 'whatisinnosetup' then begin
    S := S + SNewLine + SNewLine +
      '<!--[if lt IE 6]>' + SNewLine +
      '<p style="background: #ffa0a0; color: black; padding: 6px; border: 1px solid black">' + SNewLine +
      'You are running an old version of Internet Explorer. Consequently, ' +
      'you may encounter problems viewing the documentation. It is ' +
      'recommended that you upgrade to Internet Explorer 6.0 or later.' + SNewLine +
      '</p>' + SNewLine +
      '<![endif]-->';
  end;

  if SetupTopic then begin
    if (SetupFormatText <> '') or
       (SetupValidText <> '') or
       (SetupDefaultText <> '') then
      S := S + SNewLine + '<table class="setuphdr">' + SNewLine +
        SetupFormatText + SetupValidText + SetupDefaultText + '</table>';
    S := S + SNewLine + '<div><b>Description:</b></div>';
  end;

  S := S +
    BodyText +
    '</div>' + SNewLine +
    '</body>' + SNewLine +
    '</html>' + SNewLine;

  { Normalize the line breaks (MSXML converts CRLF -> LF) }
  StringChange(S, #13#10, #10);
  StringChange(S, #10, #13#10);

  SaveStringToFile(S, OutputDir + GenerateTopicFilename(TopicName));
  Inc(TopicsGenerated);
end;

procedure GenerateHTMLHelpContents(const ContentsNode: IXMLNode);
var
  SL: TStringList;

  procedure AddLeaf(const Title, TopicName: String);
  begin
    SL.Add(Format('<li><object type="text/sitemap">' +
      '<param name="Name" value="%s">' +
      '<param name="Local" value="%s"></object>',
      [EscapeHTML(Title), EscapeHTML(GenerateTopicLink(TopicName, ''))]));
  end;

  procedure HandleSetupDirectivesNode;
  var
    I: Integer;
  begin
    SL.Add('<ul>');
    for I := 0 to SetupDirectives.Count-1 do
      AddLeaf(SetupDirectives[I], GenerateSetupDirectiveTopicName(SetupDirectives[I]));
    SL.Add('</ul>');
  end;

  procedure HandleNode(const ParentNode: IXMLNode);
  var
    Node: IXMLNode;
  begin
    SL.Add('<ul>');
    Node := ParentNode.FirstChild;
    while Assigned(Node) do begin
      if not IsWhitespace(Node) then begin
        case ElementFromNode(Node) of
          elContentsHeading:
            begin
              SL.Add(Format('<li><object type="text/sitemap">' +
                '<param name="Name" value="%s"></object>',
                [EscapeHTML(Node.Attributes['title'])]));
              if Node.Attributes['title'] = '[Setup] section directives' then
                HandleSetupDirectivesNode
              else
                HandleNode(Node);
            end;
          elContentsTopic:
            AddLeaf(Node.Attributes['title'], Node.Attributes['topic']);
        else
          UnexpectedElementError(Node);
        end;
      end;
      Node := Node.NextSibling;
    end;
    SL.Add('</ul>');
  end;

begin
  SL := TStringList.Create;
  try
    SL.Add('<html><head></head><body>');

    HandleNode(ContentsNode);

    SL.Add('</body></html>');
    SL.WriteBOM := False;
    SL.SaveToFile(OutputDir + 'hh_generated_contents.hhc', TEncoding.UTF8);
  finally
    SL.Free;
  end;
end;

procedure GenerateStaticContents(const ContentsNode: IXMLNode);
var
  SL: TStringList;
  CurHeadingID: Integer;

  procedure AddLeaf(const Title, TopicName: String);
  begin
    SL.Add(Format('<tr><td><img src="images/contentstopic.svg" alt="" /></td>' +
      '<td><a href="%s" target="bodyframe">%s</a></td></tr>',
      [EscapeHTML(GenerateTopicLink(TopicName, '')), EscapeHTML(Title)]));
  end;

  procedure HandleSetupDirectivesNode;
  var
    I: Integer;
  begin
    SL.Add('<table>');
    for I := 0 to SetupDirectives.Count-1 do
      AddLeaf(SetupDirectives[I], GenerateSetupDirectiveTopicName(SetupDirectives[I]));
    SL.Add('</table>');
  end;

  procedure HandleNode(const ParentNode: IXMLNode);
  var
    Node: IXMLNode;
  begin
    SL.Add('<table>');
    Node := ParentNode.FirstChild;
    while Assigned(Node) do begin
      if not IsWhitespace(Node) then begin
        case ElementFromNode(Node) of
          elContentsHeading:
            begin
              Inc(CurHeadingID);
              SL.Add(Format('<tr id="nodecaption_%d"><td><img id="nodeimg_%d" src="images/contentsheadopen.png" alt="&gt;&nbsp;" onclick="toggle_node(%d);" /></td>' +
                '<td><a href="javascript:toggle_node(%d);">%s</a></td></tr>',
                [CurHeadingID, CurHeadingID, CurHeadingID, CurHeadingID, EscapeHTML(Node.Attributes['title'])]));
              SL.Add(Format('<tr id="nodecontent_%d"><td></td><td>', [CurHeadingID]));
              if Node.Attributes['title'] = '[Setup] section directives' then
                HandleSetupDirectivesNode
              else
                HandleNode(Node);
              SL.Add('</td></tr>');
            end;
          elContentsTopic:
            AddLeaf(Node.Attributes['title'], Node.Attributes['topic']);
        else
          UnexpectedElementError(Node);
        end;
      end;
      Node := Node.NextSibling;
    end;
    SL.Add('</table>');
  end;

var
  TemplateSL: TStringList;
  S: String;
begin
  SL := TStringList.Create;
  try
    CurHeadingID := 0;
    HandleNode(ContentsNode);

    TemplateSL := TStringList.Create;
    try
      TemplateSL.LoadFromFile(OutputDir + 'contents-template.htm');
      S := TemplateSL.Text;
      if StringChange(S, '%CONTENTSTABLES%' + SNewLine, SL.Text) <> 1 then
        raise Exception.Create('GenerateStaticContents: Unexpected result from StringChange');
      TemplateSL.Text := S;
      TemplateSL.WriteBOM := False;
      TemplateSL.SaveToFile(OutputDir + 'contents.htm', TEncoding.UTF8);
    finally
      TemplateSL.Free;
    end;
  finally
    SL.Free;
  end;
end;

procedure GenerateHTMLHelpIndex;

  function MultiKeyword(const Keyword: String): Boolean;
  var
    I, N: Integer;
  begin
    N := 0;
    for I := 0 to Keywords.Count-1 do begin
      if Keywords[I] = Keyword then begin
        Inc(N);
        if N > 1 then
          Break;
      end;
    end;
    Result := N > 1;
  end;

var
  SL: TStringList;
  I: Integer;
  Anchor: String;
begin
  SL := TStringList.Create;
  try
    SL.Add('<html><head></head><body><ul>');
    for I := 0 to Keywords.Count-1 do begin
      { If a keyword is used more then once, don't use anchors: the 'Topics Found'
        dialog displayed when clicking on such a keyword doesn't display the correct
        topic titles anymore for each item with an anchor. Some HTML Help bug, see
        http://social.msdn.microsoft.com/Forums/en-US/devdocs/thread/a2ee989e-4488-4edd-b034-745ed91c19e2 }
      if not MultiKeyword(Keywords[I]) then
        Anchor := TKeywordInfo(Keywords.Objects[I]).Anchor
      else
        Anchor := '';
      SL.Add(Format('<li><object type="text/sitemap">' +
        '<param name="Name" value="%s">' +
        '<param name="Local" value="%s">' +
        '</object>',
        [EscapeHTML(Keywords[I]),
         EscapeHTML(GenerateTopicLink(TKeywordInfo(Keywords.Objects[I]).Topic,
           Anchor))]));
    end;
    SL.Add('</ul></body></html>');
    SL.WriteBOM := False;
    SL.SaveToFile(OutputDir + 'hh_generated_index.hhk', TEncoding.UTF8);
  finally
    SL.Free;
  end;
end;

procedure GenerateStaticIndex;

  function EscapeForJSStringLiteral(const S: String): String;
  begin
    Result := S;
    StringChange(Result, '\', '\\');
    StringChange(Result, '"', '\"');
    { Note: Escaping " isn't really necessary here since EscapeHTML will
      replace all " with &quot; }
  end;

var
  S, T: String;
  I: Integer;
begin
  S := 'var contentsIndexData=[';

  for I := 0 to Keywords.Count-1 do begin
    T := Lowercase(TKeywordInfo(Keywords.Objects[I]).Topic);
    if TKeywordInfo(Keywords.Objects[I]).Anchor <> '' then
      T := T + '#' + TKeywordInfo(Keywords.Objects[I]).Anchor;
    if Pos(':', T) <> 0 then
      raise Exception.CreateFmt('GenerateStaticIndex: Invalid character in topic name/anchor "%s"', [T]);
    if I <> 0 then
      S := S + ',';
    S := S + Format('"%s:%s"', [EscapeForJSStringLiteral(EscapeHTML(T)),
      EscapeForJSStringLiteral(EscapeHTML(Keywords[I]))]);
  end;

  S := S + ('];' + SNewLine + 'init_index_tab_elements();');
  SaveStringToFile(S, OutputDir + 'contentsindex.js');
end;

procedure CheckForNonexistentTargetTopics;
var
  I: Integer;
begin
  for I := 0 to TargetTopics.Count-1 do
    if not ListItemExists(DefinedTopics, TargetTopics[I]) then
      raise Exception.CreateFmt('Link target topic "%s" does not exist',
        [TargetTopics[I]]);
      //Writeln(Format('Warning: Link target topic "%s" does not exist',
      //  [TargetTopics[I]]));
end;

procedure Go;

  procedure TransformFile(const FromXml, FromXsl, ToXml: String);
  var
    Doc, StyleDoc: TXMLDocument;
  begin
    Writeln('- Generating ' + ToXml);
    Doc := TXMLDocument.Create;
    try
      StyleDoc := TXMLDocument.Create;
      try
        Writeln('  - Loading ' + FromXml);
        Doc.LoadFromFile(SourceDir + FromXml);
        Writeln('  - Loading ' + FromXsl);
        StyleDoc.LoadFromFile(SourceDir + FromXsl);
        Writeln('  - Transforming');
        SaveStringToFile(Doc.Root.TransformNode(StyleDoc.Root),
          SourceDir + ToXml);
      finally
        StyleDoc.Free;
      end;
    finally
      Doc.Free;
    end;
  end;

  procedure GenerateIsxClassesFile;
  var
    IsxclassesParser: TIsxclassesParser;
  begin
    Writeln('- Generating isxclasses_generated.xml');
    IsxclassesParser := TIsxclassesParser.Create;
    try
      IsxclassesParser.Parse(SourceDir + 'isxclasses.pas');
      IsxclassesParser.SaveXML(SourceDir + 'isxclasses.header',
        SourceDir + 'isxclasses.header2',
        SourceDir + 'isxclasses.footer',
        SourceDir + 'isxclasses_generated.xml');
      IsxclassesParser.SaveWordLists(SourceDir + 'isxclasses_wordlists_generated.pas');
    finally
      IsxclassesParser.Free;
    end;
  end;

  procedure ReadSetupDirectiveNames(Node: IXMLNode);
  begin
    while Assigned(Node) do begin
      if ElementFromNode(Node) = elSetupTopic then
        SetupDirectives.Add(Node.Attributes['directive']);
      Node := Node.NextSibling;
    end;
  end;

  procedure DoDoc(Filename: String);
  var
    Doc: TXMLDocument;
    Node: IXMLNode;
  begin
    Writeln('- Parsing ', Filename);
    Doc := TXMLDocument.Create;
    try
      Doc.LoadFromFile(SourceDir + Filename);
      Doc.StripComments;

      Node := Doc.Root;
      if Node.HasAttribute('version') and (Node.Attributes['version'] <> XMLFileVersion) then
        raise Exception.CreateFmt('Unrecognized file version "%s" (expected "%s")',
          [Node.Attributes['version'], XMLFileVersion]);
      Node := Node.FirstChild;
      ReadSetupDirectiveNames(Node);
      while Assigned(Node) do begin
        if not IsWhitespace(Node) then begin
          case ElementFromNode(Node) of
            elContents:
              begin
                Writeln('  - Generating hh_generated_contents.hhc');
                GenerateHTMLHelpContents(Node);
                if not NoContentsHtm then begin
                  Writeln('  - Generating contents.htm');
                  GenerateStaticContents(Node);
                end;
              end;
            elSetupTopic: ParseTopic(Node, True);
            elTopic: ParseTopic(Node, False);
          else
            UnexpectedElementError(Node);
          end;
        end;
        Node := Node.NextSibling;
      end;
    finally
      Doc.Free;
    end;
  end;

var
  I: Integer;
begin
  TransformFile('isxfunc.xml', 'isxfunc.xsl', 'isxfunc_generated.xml');
  GenerateIsxClassesFile;
  TransformFile('ispp.xml', 'ispp.xsl', 'ispp_generated.xml');

  Keywords := TStringList.Create;
  Keywords.Duplicates := dupAccept;
  Keywords.Sorted := True;
  DefinedTopics := TStringList.Create;
  DefinedTopics.Sorted := True;
  TargetTopics := TStringList.Create;
  TargetTopics.Sorted := True;
  SetupDirectives := TStringList.Create;
  SetupDirectives.Duplicates := dupError;
  SetupDirectives.Sorted := True;
  try
    DoDoc('isetup.xml');
    DoDoc('isx.xml');
    DoDoc('isxfunc_generated.xml');
    DoDoc('isxclasses_generated.xml');
    DoDoc('ispp_generated.xml');

    CheckForNonexistentTargetTopics;

    Writeln('- Generating hh_generated_index.hhk');
    GenerateHTMLHelpIndex;
    if not NoContentsHtm then begin
      Writeln('- Generating contentsindex.js');
      GenerateStaticIndex;
    end;
  finally
    SetupDirectives.Free;
    TargetTopics.Free;
    DefinedTopics.Free;
    if Assigned(Keywords) then begin
      for I := Keywords.Count-1 downto 0 do
        TKeywordInfo(Keywords.Objects[I]).Free;
      Keywords.Free;
    end;
  end;
end;

var
  StartTime, EndTime: DWORD;
begin
  try
    Writeln('ISHelpGen v' + Version + ' by Jordan Russell & Martijn Laan');

    if (ParamCount = 0) or (ParamCount > 2) then begin
      Writeln('usage: ISHelpGen <source-dir> [postfix]');
      Halt(2);
    end;
    SourceDir := ParamStr(1) + '\';
    OutputDir := SourceDir + 'Staging' + ParamStr(2) + '\';

    NoContentsHtm := not FileExists(OutputDir + 'contents-template.htm');
    if NoContentsHtm then
      Writeln('Running in NoContentsHtm mode');

    OleCheck(CoInitialize(nil));  { for MSXML }

    StartTime := GetTickCount;
    Go;
    EndTime := GetTickCount;

    Writeln('Success - ', TopicsGenerated, ' topics generated (',
      EndTime - StartTime, ' ms elapsed)');
  except
    on E: Exception do begin
      Writeln('Error: ', TrimRight(E.Message));
      Halt(1);
    end;
  end;
end.
