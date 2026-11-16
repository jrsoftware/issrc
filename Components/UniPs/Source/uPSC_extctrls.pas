{ Compiletime Extctrls support }
unit uPSC_extctrls;

{$I PascalScript.inc}
interface
uses
  uPSCompiler, uPSUtils;

(*
   Will register files from:
     ExtCtrls
 
Requires:
  STD, classes, controls, graphics
*)

procedure SIRegister_ExtCtrls_TypesAndConsts(cl: TPSPascalCompiler);

procedure SIRegisterTBEVEL(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMPANEL(Cl: TPSPascalCompiler);
procedure SIRegisterTPANEL(Cl: TPSPascalCompiler);
procedure SIRegisterTCUSTOMLINKLABEL(Cl: TPSPascalCompiler);
procedure SIRegisterTLINKLABEL(Cl: TPSPascalCompiler);
procedure SIRegister_ExtCtrls(cl: TPSPascalCompiler);

implementation
procedure SIRegisterTBEVEL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TGraphicControl'), 'TBevel') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('Shape', 'TBevelShape', iptrw);
    RegisterProperty('Style', 'TBevelStyle', iptrw);
  end;
end;

procedure SIRegisterTCUSTOMPANEL(Cl: TPSPascalCompiler);
begin
  Cl.AddClassN(cl.FindClass('TCustomControl'), 'TCustomPanel');
end;

procedure SIRegisterTPANEL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomPanel'), 'TPanel') do
  begin
    RegisterProperty('Alignment', 'TAlignment', iptrw);
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('BevelInner', 'TPanelBevel', iptrw);
    RegisterProperty('BevelOuter', 'TPanelBevel', iptrw);
    RegisterProperty('BevelKind', 'TBevelKind', iptrw);
    RegisterProperty('BevelWidth', 'TBevelWidth', iptrw);
    RegisterProperty('BorderWidth', 'TBorderWidth', iptrw);
    RegisterProperty('BorderStyle', 'TBorderStyle', iptrw);
    RegisterProperty('Caption', 'string', iptrw);
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

procedure SIRegisterTCUSTOMLINKLABEL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TWinControl'), 'TCustomLinkLabel') do
  begin
    RegisterProperty('Alignment', 'TAlignment', iptrw); //actual type: taLeftJustify..taRightJustify
    RegisterProperty('AutoSize', 'Boolean', iptrw);
    RegisterProperty('UseVisualStyle', 'Boolean', iptrw);
    RegisterProperty('OnLinkClick', 'TSysLinkEvent', iptrw);  
  end;
end;

procedure SIRegisterTLINKLABEL(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TCustomLinkLabel'), 'TLinkLabel') do
  begin
    RegisterProperty('Anchors', 'TAnchors', iptrw);
    RegisterProperty('Caption', 'string', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('ParentColor', 'Boolean', iptrw);
    RegisterProperty('ParentFont', 'Boolean', iptrw);
  end;
end;

procedure SIRegister_ExtCtrls_TypesAndConsts(cl: TPSPascalCompiler);
begin
  cl.AddTypeS('TShapeType', '(stRectangle, stSquare, stRoundRect, stRoundSquare, stEllipse, stCircle)');
  cl.AddTypeS('TBevelStyle', '(bsLowered, bsRaised)');
  cl.AddTypeS('TBevelShape', '(bsBox, bsFrame, bsTopLine, bsBottomLine, bsLeftLine, bsRightLine,bsSpacer)');
  cl.AddTypeS('TPanelBevel', '(bvNone, bvLowered, bvRaised,bvSpace)');
  cl.AddTypeS('TBevelWidth', 'LongInt');
  cl.AddTypeS('TBorderWidth', 'LongInt');
  cl.AddTypeS('TSectionEvent', 'procedure(Sender: TObject; ASection, AWidth: Integer)');
  cl.AddTypeS('TSysLinkType', '(sltURL, sltID)');
  cl.AddTypeS('TSysLinkEvent', 'procedure(Sender: TObject; const Link: string; LinkType: TSysLinkType)');
end;

procedure SIRegister_ExtCtrls(cl: TPSPascalCompiler);
begin
  SIRegister_ExtCtrls_TypesAndConsts(cl);

  SIRegisterTBEVEL(Cl);
  SIRegisterTCUSTOMPANEL(Cl);
  SIRegisterTPANEL(Cl);
  SIRegisterTCUSTOMLINKLABEL(Cl);
  SIRegisterTLINKLABEL(Cl);
end;

end.





