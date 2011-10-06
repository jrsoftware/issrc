unit SetupTypes;

{
  Inno Setup
  Copyright (C) 1997-2007 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Types used by both ISCmplr-only and Setup-only units

  $jrsoftware: issrc/Projects/SetupTypes.pas,v 1.15 2007/09/05 02:07:35 jr Exp $
}

interface

type
  TSetupStep = (ssPreInstall, ssInstall, ssPostInstall, ssDone);

  TUninstallStep = (usAppMutexCheck, usUninstall, usPostUninstall, usDone);

const
  { Predefined page identifiers }
  wpWelcome = 1;
  wpLicense = 2;
  wpPassword = 3;
  wpInfoBefore = 4;
  wpUserInfo = 5;
  wpSelectDir = 6;
  wpSelectComponents = 7;
  wpSelectProgramGroup = 8;
  wpSelectTasks = 9;
  wpReady = 10;
  wpPreparing = 11;
  wpInstalling = 12;
  wpInfoAfter = 13;
  wpFinished = 14;

type
  TShellFolderID = (sfDesktop, sfStartMenu, sfPrograms, sfStartup, sfSendTo,
    sfFonts, sfAppData, sfDocs, sfTemplates, sfFavorites, sfLocalAppData);

  TInstallOnThisVersionResult = (irInstall, irNotOnThisPlatform,
    irVerTooLow, irVerTooHigh);

const
  irInvalid = Ord(High(TInstallOnThisVersionResult))+1;
  crHand = 1;

  CodeRootKeyFlagMask  = $7F000000;
  CodeRootKeyFlag32Bit = $01000000;
  CodeRootKeyFlag64Bit = $02000000;
  CodeRootKeyValidFlags = CodeRootKeyFlag32Bit or CodeRootKeyFlag64Bit;

implementation

end.
