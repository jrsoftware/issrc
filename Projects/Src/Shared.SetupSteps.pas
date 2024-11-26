unit Shared.SetupSteps;

{
  Inno Setup
  Copyright (C) 1997-2018 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Setup steps
}

interface

type
  TSetupStep = (ssPreInstall, ssInstall, ssPostInstall, ssDone);

  TUninstallStep = (usAppMutexCheck, usUninstall, usPostUninstall, usDone);

implementation

end.
