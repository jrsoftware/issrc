; *** Inno Setup version 4.0.5+ Norsk, nynorsk messages ***
;
; Translated by/omsett av: Magnar Myrtveit
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/is3rdparty.php
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; $jrsoftware: issrc/Files/Default.isl,v 1.32 2003/06/18 19:24:07 jr Exp $

[LangOptions]
LanguageName=Norsk, nynorsk
LanguageID=$0409
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
;DialogFontName=MS Shell Dlg
;DialogFontSize=8
;DialogFontStandardHeight=13
;TitleFontName=Arial
;TitleFontSize=29
;WelcomeFontName=Verdana
;WelcomeFontSize=12
;CopyrightFontName=Arial
;CopyrightFontSize=8

DialogFontName=MS Shell Dlg
DialogFontSize=8
DialogFontStandardHeight=13
TitleFontName=Arial
TitleFontSize=29
WelcomeFontName=Verdana
WelcomeFontSize=12
CopyrightFontName=Arial
CopyrightFontSize=8
[Messages]

; *** Application titles
SetupAppTitle=Installasjon
SetupWindowTitle=Installering - %1
UninstallAppTitle=Avinstallasjon
UninstallAppFullTitle=Avinstallering - %1

; *** Misc. common
InformationTitle=Informasjon
ConfirmTitle=Godta
ErrorTitle=Feil

; *** SetupLdr messages
SetupLdrStartupMessage=Dette vil installera %1. Vil du halda fram?
LdrCannotCreateTemp=Kan ikkje laga midlertidig fil. Installasjonen er avbroten
LdrCannotExecTemp=Kan ikkje køyra fila i den midlertidige katalogen. Installasjonen er avbroten

; *** Startup error messages
LastErrorMessage=%1.%n%nFeil %2: %3
SetupFileMissing=Fila %1 finst ikkje i installasjonskatalogen. Ver venleg og fiks problemet eller få tak i ein ny kopi av programmet.
SetupFileCorrupt=Installasjonsfila er øydelagt. Ver venleg og få tak i ein ny kopi av programmet.
SetupFileCorruptOrWrongVer=Installasjonsfilane er øydelagde, eller ikkje kompatible med dette installasjonsprogrammet.Ver venleg og fiks problemet eller få tak i ein ny kopi av programmet.
NotOnThisPlatform=Dette programmet køyrer ikkje på %1.
OnlyOnThisPlatform=Dette programmet køyrer berre på %1.
WinVersionTooLowError=Dette programmet krev %1 versjon %2 eller nyare.
WinVersionTooHighError=Dette programmet kan ikkje bli installert på %1 versjon %2 eller nyare.
AdminPrivilegesRequired=Du må vera logga inn som administrator når du installerer dette programmet.
PowerUserPrivilegesRequired=Du må vera logga inn som administrator eller ha administrator-rettar når du installerer dette programmet.
SetupAppRunningError=Installasjonen har oppdaga at %1 køyrer.%n%nVer venleg og lukk det no, og trykk OK for å halda fram, eller Avbryt for å avslutta.
UninstallAppRunningError=Avinstallasjonen har oppdaga at %1 køyrer.%n%nVer venleg og lukk det no, og trykk OK for å halda fram, eller Avbryt for å avslutta.

; *** Misc. errors
ErrorCreatingDir=Installasjonsprogrammet kunne ikkje laga katalogen "%1"
ErrorTooManyFilesInDir=Kunne ikkje laga ei fil i mappa "%1" fordi den inneheld for mange filar

; *** Setup common messages
ExitSetupTitle=Avslutt installasjonen
ExitSetupMessage=Installasjonen er ikkje ferdig. Viss du avsluttar no, vil ikkje programmet bli installert.%n%nDu kan køyra installasjonen på nytt seinare for å fullføra installsajonen.%n%nVil du avslutta installasjonen?
AboutSetupMenuItem=&Om installasjonsprogrammet...
AboutSetupTitle=Om installasjonsprogrammet
AboutSetupMessage=%1 versjon %2%n%3%n%n%1 heimeside:%n%4
AboutSetupNote=

; *** Buttons
ButtonBack=< &Tilbake
ButtonNext=&Neste >
ButtonInstall=&Installer
ButtonOK=OK
ButtonCancel=Avbryt
ButtonYes=&Ja
ButtonYesToAll=Ja til &alle
ButtonNo=&Nei
ButtonNoToAll=N&ei til alle
ButtonFinish=&Ferdig
ButtonBrowse=&Bla gjennom...

; *** "Select Language" dialog messages
SelectLanguageTitle=Vel installasjonsspråk
SelectLanguageLabel=Vel språket som skal brukast under installasjonen:

; *** Common wizard text
ClickNext=Trykk Neste for å halda fram, eller Avbryt for å avslutta installasjonen.
BeveledLabel=

; *** "Welcome" wizard page
WelcomeLabel1=Velkomen til installasjonen av [name]
WelcomeLabel2=Dette vil installera [name/ver] på di datamaskin.%n%nDet er anbefalt at du lukkar alle andre program før du fortset.

; *** "Password" wizard page
WizardPassword=Passord
PasswordLabel1=Denne installasjonen er passordbeskytta.
PasswordLabel3=Ver venleg og skriv inn passordet, og trykk Neste for å halda fram. Store og små bokstavar blir behandla ulikt.
PasswordEditLabel=&Passord:
IncorrectPassword=Passordet du skreiv inn er feil. Ver venleg og prøv igjen.

; *** "License Agreement" wizard page
WizardLicense=Lisensvilkår
LicenseLabel=Ver venleg og les den fylgjande viktige informasjonen før du fortset.
LicenseLabel3=Ver venleg og les dei fylgjande lisensvilkåra. Du må godta innehaldet i denne avtalen før du fortset installasjonen.
LicenseAccepted=Eg &godtar avtalen
LicenseNotAccepted=Eg godtar &ikkje avtalen

; *** "Information" wizard pages
WizardInfoBefore=Informasjon
InfoBeforeLabel=Ver venleg og les den fylgjande viktige informasjonen før du fortset.
InfoBeforeClickLabel=Når du er klar til å halda fram med installasjonen, trykk Neste.
WizardInfoAfter=Informasjon
InfoAfterLabel=Ver venleg og les den fylgjande viktige informasjonen før du fortset.
InfoAfterClickLabel=Når du er klar til å fullføra installasjonen, trykk Neste.

; *** "User Information" wizard page
WizardUserInfo=Brukarinformasjon
UserInfoDesc=Ver venleg og skriv inn din informasjon.
UserInfoName=&Brukarnamn:
UserInfoOrg=&Organisasjon:
UserInfoSerial=&Serienummer:
UserInfoNameRequired=Du må skriva inn eit namn.

; *** "Select Destination Directory" wizard page
WizardSelectDir=Vel målmappe
SelectDirDesc=Kvar skal [name] bli installert?
SelectDirLabel=Vel mappa der du vil at [name] skal bli installert, og trykk Neste.
DiskSpaceMBLabel=Programmet krev minst [mb] MB diskplass.
ToUNCPathname=Kan ikkje installera til ei UNC-bane. Viss du prøver å installera i eit nettverk, må du skriva inn nettverksmålstasjonen.
InvalidPath=Du må skriva inn ei full bane med stasjonsbokstav; for eksempel:%n%nC:\APP%n%neller ei UNC-bane som:%n%n\\server\share
InvalidDrive=Den valte stasjonen eller UNC-delinga finst ikkje, eller er ikkje tilgjengeleg. Ver venleg og vel ein annan.
DiskSpaceWarningTitle=Ikkje nok diskplass
DiskSpaceWarning=Installasjonsprogrammet krev minst %1 KB ledig plass for å installera, men den valte stasjonen har berre %2 KB ledig.%n%nVil du halda fram likevel?
BadDirName32=Katalognamn kan ikkje innehalda nokon av dei fylgjande teikna:%n%n%1
DirExistsTitle=Katalogen finst
DirExists=Katalogen:%n%n%1%n%nfinst allereie. Vil du installera til denne katalogen likevel?
DirDoesntExistTitle=Katalogen finst ikkje
DirDoesntExist=Katalogen:%n%n%1%n%nfinst ikkje. Vil du at katalogen skal bli laga?

; *** "Select Components" wizard page
WizardSelectComponents=Vel komponentar
SelectComponentsDesc=Kva komponentar skal installerast?
SelectComponentsLabel2=Vel dei komponentane du vil installera; vel vekk dei komponentane du ikkje vil installera. Trykk Neste når du er klar til å halda fram.
FullInstallation=Full installasjon
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompakt installasjon
CustomInstallation=Eigendefinert installasjon
NoUninstallWarningTitle=Komponentane finst
NoUninstallWarning=Installasjonen har oppdaga at desse komponentane allereie er installerte på datamaskina:%n%n%1%n%nDesse komponentane blir ikkje avinstallerte sjølv om du vel dei vekk.%n%nVil du halda fram likevel?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Valte alternativ krev minst [mb] MB ledig diskplass.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Vel tilleggsoppgåver
SelectTasksDesc=Kva tilleggsoppgåver skal utførast?
SelectTasksLabel2=Vel dei tilleggsoppgåvene som skal utførast mens [name] blir installert, trykk deretter Neste.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Vel mappe i Start-menyen
SelectStartMenuFolderDesc=Kvar skal snarvegane plasserast?
SelectStartMenuFolderLabel=Vel mappa i Start-menyen der du vil at snarvegane skal bli plasserte. Trykk deretter Neste.
NoIconsCheck=&Ikkje lag ikon
MustEnterGroupName=Du må skriva inn eit katalognamn.
BadGroupName=Katalognamn kan ikkje innehalda nokon av dei fylgjande teikna:%n%n%1
NoProgramGroupCheck2=&Ikkje lag mappe i Start-menyen

; *** "Ready to Install" wizard page
WizardReady=Klar til å installera
ReadyLabel1=Installasjonsprogrammet er no klart til å starta installasjonen av [name] på di datamaskin.
ReadyLabel2a=Trykk Installer for å halda fram med installasjonen, eller trykk Tilbake viss du vil sjå på eller endra instillingane.
ReadyLabel2b=Trykk Installer for å halda fram med installasjonen.
ReadyMemoUserInfo=Brukarinformasjon:
ReadyMemoDir=Målmappe:
ReadyMemoType=Installasjonstype:
ReadyMemoComponents=Valte komponentar:
ReadyMemoGroup=Mappe i Start-menyen:
ReadyMemoTasks=Tilleggsoppgåver:

; *** "Preparing to Install" wizard page
WizardPreparing=Førebur installasjonen
PreparingDesc=Installasjonsprogrammet førebur installasjonen av [name] på di datamaskin.
PreviousInstallNotCompleted=Installasjonen/fjerninga av eit tidlegare program blei ikkje fullført. Du må starta maskina på nytt for å fullføra den installasjonen.%n%nEtter omstarten må du køyra installasjonsprogrammet på nytt for å fullføra installasjonen av [name].
CannotContinue=Installasjonsprogrammet kan ikkje halda fram. Ver venleg og trykk Avbryt for å avslutta.

; *** "Installing" wizard page
WizardInstalling=Installerer
InstallingLabel=Ver venleg og vent mens [name] blir installert på di datamaskin.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Fullfører installasjonsvegvisaren for [name]
FinishedLabelNoIcons=Installasjonen av [name] er fullført.
FinishedLabel=Installasjonen av [name] er fullført. Du kan starta programmet ved å trykka på eit av dei installerte ikona.
ClickFinish=Trykk Ferdig for å avslutta installasjonen.
FinishedRestartLabel=For å fullføra installasjonen av [name], må maskina bli starta på nytt. Vil du starta på nytt no?
FinishedRestartMessage=For å fullføra installasjonen av [name], må maskina bli starta på nytt.%n%nVil du starta på nytt no?
ShowReadmeCheck=Ja, eg vil sjå LESMEG-fila
YesRadio=&Ja, start maskina på nytt no
NoRadio=&Nei, eg vil starta på nytt seinare
; used for example as 'Run MyProg.exe'
RunEntryExec=Køyr %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Vis %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Installasjonen treng den neste disketten
SelectDirectory=Vel katalog
SelectDiskLabel2=Ver venleg og set inn diskett %1 og trykk OK.%n%nViss filane finst i ein annan katalog enn den vist nedom, skriv inn riktig bane eller trykk Bla gjennom...
PathLabel=&Bane:
FileNotInDir2=Finn ikkje fila "%1" i "%2". Ver venleg og set inn riktig diskett eller vel ein annan katalog.
SelectDirectoryLabel=Ver venleg og skriv inn plasseringa til den neste disketten.

; *** Installation phase messages
SetupAborted=Installasjonen blei ikkje fullført.%n%nVer venleg og fiks problemet og køyr installasjonen på nytt.
EntryAbortRetryIgnore=Tykkk Prøv på nytt for å prøva på nytt, Ignorér for å halda fram, eller Avbryt for å avslutta installasjonen.

; *** Installation status messages
StatusCreateDirs=Oppretter katalogar...
StatusExtractFiles=Pakkar ut filar...
StatusCreateIcons=Oppretter program-ikon...
StatusCreateIniEntries=Oppretter INI-instillingar...
StatusCreateRegistryEntries=Opprettter register-instillingar...
StatusRegisterFiles=Registrerer filar...
StatusSavingUninstall=Lagrar avinstallasjonsinformasjon...
StatusRunProgram=Fullfører installasjonen...
StatusRollback=Tilbakestiller endringar...

; *** Misc. errors
ErrorInternal2=Intern feil: %1
ErrorFunctionFailedNoCode=%1 gjekk gale
ErrorFunctionFailed=%1 gjekk gale; kode %2
ErrorFunctionFailedWithMessage=%1 gjekk gale; kode %2.%n%3
ErrorExecutingProgram=Kunne ikkje køyre fila:%n%1

; *** Registry errors
ErrorRegOpenKey=Feil under opning av registernøkkel:%n%1\%2
ErrorRegCreateKey=Feil under oppretting av registernøkkel:%n%1\%2
ErrorRegWriteKey=Feil under skriving til registernøkkel:%n%1\%2

; *** INI errors
ErrorIniEntry=Feil under oppretting av innstillingar i fila "%1".

; *** File copying errors
FileAbortRetryIgnore=Trykk Prøv på nytt for å prøva på nytt, Ignorér for å hoppa over denne fila (ikkje anbefalt), eller Avbryt for å avslutta installasjonen.
FileAbortRetryIgnore2=Trykk Prøv på nytt for å prøva på nytt, Ignorér for å halda fram (ikkje anbefalt), eller Avbryt for å avslutta installasjonen.
SourceIsCorrupted=Kjeldefila er øydelagt
SourceDoesntExist=Kjeldefila "%1" finst ikkje
ExistingFileReadOnly=Den eksisterande fila er skrivebeskytta.%n%nTrykk Prøv på nytt for å fjerna skrivebeskyttinga og prøva på nytt, Ignorér for å hoppa over denne fila, eller Avbryt for å avslutta installasjonen.
ErrorReadingExistingDest=Ein feil oppstod under lesing av den eksisterande fila:
FileExists=Fila finst allereie.%n%nVil du at installasjonsprogrammet skal skriva over den?
ExistingFileNewer=Den eksisterande fila er nyare enn den installasjonen prøver å installera. Det er anbefalt at du beheld den eksisterande fila.%n%nVil du behalda den eksisterande fila?
ErrorChangingAttr=Ein feil oppstod under forsøk på å endra attributtar i den eksisterande fila:
ErrorCreatingTemp=Ein feil oppstod under forsøk på å oppretta ei fil i målmappa:
ErrorReadingSource=Ein feil oppstod under forsøk på å lesa kjeldefila:
ErrorCopying=Ein feil oppstod under forsøk på å kopiera fila:
ErrorReplacingExistingFile=Ein feil oppstod under forsøk på å erstatta den eksisterande fila:
ErrorRestartReplace=RestartReplace gjekk gale:
ErrorRenamingTemp=Ein feil oppstod under forsøk på å gje nytt namnt til ei fil i målmappa:
ErrorRegisterServer=Kunne ikkje registrera DLL/OCX: %1
ErrorRegisterServerMissingExport=DllRegisterServer-eksportering blei ikkje funne
ErrorRegisterTypeLib=Kunne ikkje registrera typebiblioteket: %1

; *** Post-installation errors
ErrorOpeningReadme=Ein feil oppstod under forsøk på å opna LESMEG-fila.
ErrorRestartingComputer=Installasjonsprogrammet kunne ikkje starta maskina på nytt. Ver venleg og gjer dette manuelt.

; *** Uninstaller messages
UninstallNotFound=Kan ikkje avinstallera. Fila "%1" finst ikkje.
UninstallOpenError=Kan ikkje avinstallera. Fila "%1" kunne ikkje opnast.
UninstallUnsupportedVer=Kan ikkje avinstallera. Avinstallasjonsloggfila "%1" er i eit format som denne versjonen av avinstallasjonsprogrammet ikkje forstår
UninstallUnknownEntry=Ein ukjend parameter (%1) blei funne i avinstallasjonsloggen
ConfirmUninstall=Er du sikker på at du vil fjerna %1 og alle tilhøyrande komponentar?
OnlyAdminCanUninstall=Denne installasjonen kan berre avinstallerast av ein brukar med administrator-rettar.
UninstallStatusLabel=Ver venleg og vent mens %1 blir fjerna frå di datamaskin.
UninstalledAll=Fjerninga av %1 var vellukka.
UninstalledMost=Avinstalleringa av %1 er fullført.%n%nNokre element kunne ikkje bli sletta. Desse kan slettast manuelt.
UninstalledAndNeedsRestart=For å fullføra avinstallasjonen av %1, må datamaskina startast på nytt.%n%nVil du starta på nytt no?
UninstallDataCorrupted=Kan ikkje avinstallera. "%1"-fila er øydelagd.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Fjerna delt fil?
ConfirmDeleteSharedFile2=Systemet indikerer at den fylgjande delte fila ikkje blir brukt av nokon program. Vil du at avinstallasjonsprogrammet skal fjerna denne delte fila?%n%nViss nokre program framleis brukar denne fila, og den blir fjerna, kan du risikere at dei ikkje verkar som dei skal. Viss du er usikker, vel Nei. Å la denne fila vera på systemet vil ikkje gjera nokon skade.
SharedFileNameLabel=Filnamn:
SharedFileLocationLabel=Plassering:
WizardUninstalling=Avinstallasjons-status
StatusUninstalling=Avinstallerer %1...

