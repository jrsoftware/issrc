{ Compiletime STDCtrls support }
unit uPSC_stdctrls;

{$I PascalScript.inc}
interface
uses
  uPSCompiler, uPSUtils;

{
   Will register files from:
     stdctrls
 
Requires:
  STD, classes, controls and graphics
}

procedure SIRegister_StdCtrls_TypesAndConsts(cl: TPSPascalCompiler);



procedure SIRegisterTCUSTOMLABEL(Cl: TPSPascalCompiler);
procedure SIRegisterTLABEL(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMEDIT(Cl: TPSPascalCompiler);
procedure SIRegisterTEDIT(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMMEMO(Cl: TPSPascalCompiler);
procedure SIRegisterTMEMO(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMCOMBOBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTCOMBOBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTBUTTONCONTROL(Cl: TPSPascalCompiler);
procedure SIRegisterTBUTTON(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMCHECKBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTCHECKBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTRADIOBUTTON(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMLISTBOX(Cl: TPSPascalCompiler);
procedure SIRegisterTLISTBOX(Cl: TPSPascalCompiler);

procedure SIRegister_StdCtrls(cl: TPSPascalCompiler);


implementation

procedure SIRegisterTCUSTOMLABEL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicControl'), 'TCustomLabel') do
  begin
  end;
end;


procedure SIRegisterTLABEL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomLabel'), 'TLabel') do
  begin
    RegisterProperty('Alignment', 'TAlignment', iptrw);
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('AutoSize', 'Boolean', iptrw);
    RegisterProperty('Caption', 'string', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('DragCursor', 'LongInt', iptrw);
    RegisterProperty('DragMode', 'TDragMode', iptrw);
    RegisterProperty('FocusControl', 'TWinControl', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('Layout', 'TTextLayout', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('ParentShowHint', 'Boolean', iptrw);
    RegisterProperty('ShowAccelChar', 'Boolean', iptrw);
    RegisterProperty('Transparent', 'Boolean', iptrw);
    RegisterProperty('WordWrap', 'Boolean', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDragDrop', 'TDragDropEvent', iptrw);
    RegisterProperty('OnDragOver', 'TDragOverEvent', iptrw);
    RegisterProperty('OnEndDrag', 'TEndDragEvent', iptrw);
    RegisterProperty('OnMouseDown', 'TMouseEvent', iptrw);
    RegisterProperty('OnMouseMove', 'TMouseMoveEvent', iptrw);
    RegisterProperty('OnMouseUp', 'TMouseEvent', iptrw);
    RegisterProperty('OnStartDrag', 'TStartDragEvent', iptrw);
  end;
end;







procedure SIRegisterTCUSTOMEDIT(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TWinControl'), 'TCustomEdit') do
  begin
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure ClearSelection');
    RegisterMethod('procedure SelectAll');
    RegisterProperty('Modified', 'Boolean', iptrw);
    RegisterProperty('SelLength', 'Integer', iptrw);
    RegisterProperty('SelStart', 'Integer', iptrw);
    RegisterProperty('SelText', 'string', iptrw);
    RegisterProperty('Text', 'string', iptrw);
  end;
end;




procedure SIRegisterTEDIT(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomEdit'), 'TEdit') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('AutoSelect', 'Boolean', iptrw);
    RegisterProperty('AutoSize', 'Boolean', iptrw);
    RegisterProperty('BorderStyle', 'TBorderStyle', iptrw);
    RegisterProperty('CharCase', 'TEditCharCase', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('HideSelection', 'Boolean', iptrw);
    RegisterProperty('MaxLength', 'Integer', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('PasswordChar', 'Char', iptrw);
    RegisterProperty('ReadOnly', 'Boolean', iptrw);
    RegisterProperty('Text', 'string', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnExit', 'TNotifyEvent', iptrw);
    RegisterProperty('OnKeyDown', 'TKeyEvent', iptrw);
    RegisterProperty('OnKeyPress', 'TKeyPressEvent', iptrw);
    RegisterProperty('OnKeyUp', 'TKeyEvent', iptrw);
  end;
end;




procedure SIRegisterTCUSTOMMEMO(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomEdit'), 'TCustomMemo') do
  begin
    RegisterProperty('Lines', 'TStrings', iptrw);
  end;
end;


procedure SIRegisterTMEMO(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomMemo'), 'TMemo') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('Alignment', 'TAlignment', iptrw);
    RegisterProperty('BorderStyle', 'TBorderStyle', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('HideSelection', 'Boolean', iptrw);
    RegisterProperty('MaxLength', 'Integer', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('ReadOnly', 'Boolean', iptrw);
    RegisterProperty('ScrollBars', 'TScrollStyle', iptrw);
    RegisterProperty('WantReturns', 'Boolean', iptrw);
    RegisterProperty('WantTabs', 'Boolean', iptrw);
    RegisterProperty('WordWrap', 'Boolean', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnExit', 'TNotifyEvent', iptrw);
    RegisterProperty('OnKeyDown', 'TKeyEvent', iptrw);
    RegisterProperty('OnKeyPress', 'TKeyPressEvent', iptrw);
    RegisterProperty('OnKeyUp', 'TKeyEvent', iptrw);
  end;
end;





procedure SIRegisterTCUSTOMCOMBOBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TWinControl'), 'TCustomComboBox') do
  begin
    RegisterProperty('DroppedDown', 'Boolean', iptrw);
    RegisterProperty('Items', 'TStrings', iptrw);
    RegisterProperty('ItemIndex', 'Integer', iptrw);
  end;
end;


procedure SIRegisterTCOMBOBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomComboBox'), 'TComboBox') do
  begin
    RegisterProperty('Style', 'TComboBoxStyle', iptrw);
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('DropDownCount', 'Integer', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('MaxLength', 'Integer', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('Sorted', 'Boolean', iptrw);
    RegisterProperty('Text', 'string', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDropDown', 'TNotifyEvent', iptrw);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnExit', 'TNotifyEvent', iptrw);
    RegisterProperty('OnKeyDown', 'TKeyEvent', iptrw);
    RegisterProperty('OnKeyPress', 'TKeyPressEvent', iptrw);
    RegisterProperty('OnKeyUp', 'TKeyEvent', iptrw);
  end;
end;



procedure SIRegisterTBUTTONCONTROL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TWinControl'), 'TButtonControl') do
  begin
  end;
end;



procedure SIRegisterTBUTTON(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TButtonControl'),  'TButton') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('Cancel', 'Boolean', iptrw);
    RegisterProperty('Caption', 'string', iptrw);
    RegisterProperty('Default', 'Boolean', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('ModalResult', 'LongInt', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnExit', 'TNotifyEvent', iptrw);

    RegisterProperty('CommandLinkHint', 'string', iptrw);
    RegisterProperty('ElevationRequired', 'Boolean', iptrw);
    RegisterProperty('Style', 'TButtonStyle', iptrw);
  end;
end;



procedure SIRegisterTCUSTOMCHECKBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TButtonControl'), 'TCustomCheckBox') do
  begin
  end;
end;



procedure SIRegisterTCHECKBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomCheckBox'), 'TCheckBox') do
  begin
    RegisterProperty('Alignment', 'TAlignment', iptrw);
    RegisterProperty('AllowGrayed', 'Boolean', iptrw);
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('Caption', 'string', iptrw);
    RegisterProperty('Checked', 'Boolean', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('State', 'TCheckBoxState', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnExit', 'TNotifyEvent', iptrw);
  end;
end;





procedure SIRegisterTRADIOBUTTON(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TButtonControl'), 'TRadioButton') do
  begin
    RegisterProperty('Alignment', 'TAlignment', iptrw);
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('Caption', 'string', iptrw);
    RegisterProperty('Checked', 'Boolean', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnExit', 'TNotifyEvent', iptrw);
  end;
end;



procedure SIRegisterTCUSTOMLISTBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TWinControl'), 'TCustomListBox') do
  begin
    RegisterProperty('Items', 'TStrings', iptrw);
    RegisterProperty('ItemIndex', 'Integer', iptrw);
    RegisterProperty('SelCount', 'Integer', iptr);
    RegisterProperty('Selected', 'Boolean Integer', iptrw);
  end;
end;



procedure SIRegisterTLISTBOX(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomListBox'), 'TListBox') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('BorderStyle', 'TBorderStyle', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('MultiSelect', 'Boolean', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
    RegisterProperty('Sorted', 'Boolean', iptrw);
    RegisterProperty('Style', 'TListBoxStyle', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnDblClick', 'TNotifyEvent', iptrw);
    RegisterProperty('OnEnter', 'TNotifyEvent', iptrw);
    RegisterProperty('OnExit', 'TNotifyEvent', iptrw);
    RegisterProperty('OnKeyDown', 'TKeyEvent', iptrw);
    RegisterProperty('OnKeyPress', 'TKeyPressEvent', iptrw);
    RegisterProperty('OnKeyUp', 'TKeyEvent', iptrw);
  end;
end;



procedure SIRegister_StdCtrls_TypesAndConsts(cl: TPSPascalCompiler);
begin
  cl.AddTypeS('TEditCharCase', '(ecNormal, ecUpperCase, ecLowerCase)');
  cl.AddTypeS('TScrollStyle', '(ssNone, ssHorizontal, ssVertical, ssBoth)');
  cl.AddTypeS('TComboBoxStyle', '(csDropDown, csSimple, csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable)');
  cl.AddTypeS('TButtonStyle', '(bsPushButton, bsCommandLink, bsSplitButton)');
  cl.AddTypeS('TCheckBoxState', '(cbUnchecked, cbChecked, cbGrayed)');
  cl.AddTypeS('TListBoxStyle', '(lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable)');
  cl.AddTypeS('TScrollCode', '(scLineUp, scLineDown, scPageUp, scPageDown, scPosition, scTrack, scTop, scBottom, scEndScroll)');
  Cl.addTypeS('TEOwnerDrawState', '(odSelected, odGrayed, odDisabled, odChecked, odFocused, odDefault, odHotLight, odInactive, odNoAccel, odNoFocusRect, odReserved1, odReserved2, odComboBoxEdit)');
  cl.AddTypeS('TTextLayout', '(tlTop, tlCenter, tlBottom)');
  cl.AddTypeS('TOwnerDrawState', 'set of TEOwnerDrawState');

  cl.AddTypeS('TDrawItemEvent', 'procedure(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState)');
  cl.AddTypeS('TMeasureItemEvent', 'procedure(Control: TWinControl; Index: Integer; var Height: Integer)');
  cl.AddTypeS('TScrollEvent', 'procedure(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer)');
end;


procedure SIRegister_stdctrls(cl: TPSPascalCompiler);
begin
  SIRegister_StdCtrls_TypesAndConsts(cl);
  SIRegisterTCUSTOMLABEL(Cl);
  SIRegisterTLABEL(Cl);
  SIRegisterTCUSTOMEDIT(Cl);
  SIRegisterTEDIT(Cl);
  SIRegisterTCUSTOMMEMO(Cl);
  SIRegisterTMEMO(Cl);
  SIRegisterTCUSTOMCOMBOBOX(Cl);
  SIRegisterTCOMBOBOX(Cl);
  SIRegisterTBUTTONCONTROL(Cl);
  SIRegisterTBUTTON(Cl);
  SIRegisterTCUSTOMCHECKBOX(Cl);
  SIRegisterTCHECKBOX(Cl);
  SIRegisterTRADIOBUTTON(Cl);
  SIRegisterTCUSTOMLISTBOX(Cl);
  SIRegisterTLISTBOX(Cl);
end;

end.





