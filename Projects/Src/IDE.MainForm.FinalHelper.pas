unit IDE.MainForm.FinalHelper;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form - final helper to be used by MainForm
}

interface

uses
  IDE.MainForm, IDE.MainForm.UpdateMenuHelper;

type
  TMainFormFinalHelper = class helper(TMainFormUpdateMenuHelper) for TMainForm
  end;

implementation

end.
