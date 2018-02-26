; *** Inno Setup version 5.5.3+ Albanian messages ***
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; Përktheu Besmir Godole
; Posta elektronike: bgodole@gmail.com
; Më kontaktoni për ndonjë gabim ose sugjerim rreth përkthimit.

[LangOptions]
; The following three entries are very important. Be sure to read and
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Albanian
LanguageID=$041C
LanguageCodePage=1252
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
;DialogFontName=
;DialogFontSize=8
;WelcomeFontName=Verdana
;WelcomeFontSize=12
;TitleFontName=Arial
;TitleFontSize=29
;CopyrightFontName=Arial
;CopyrightFontSize=8

[Messages]

; *** Application titles
SetupAppTitle=Sistemim
SetupWindowTitle=Sistemon - %1
UninstallAppTitle=Çinstalim
UninstallAppFullTitle=Çinstalon %1

; *** Misc. common
InformationTitle=Informacion
ConfirmTitle=Miratim
ErrorTitle=Gabim

; *** SetupLdr messages
SetupLdrStartupMessage=Do të instalohet %1. Të vijohet?
LdrCannotCreateTemp=Nuk mund të krijohet një skedar i përkohshëm. U ndërpre sistemimi
LdrCannotExecTemp=Nuk mund të ekzekutohet skedari në direktorinë e përkohshme. U ndërpre sistemimi

; *** Startup error messages
LastErrorMessage=%1.%n%nGabim %2: %3
SetupFileMissing=Mungon skedari %1 në direktorinë e instalimit. Lutemi të korrigjohet problemi ose të përdoret një kopje e re e programit.
SetupFileCorrupt=Janë dëmtuar skedarët e Sistemuesit. Lutemi të përdoret një kopje e re e programit.
SetupFileCorruptOrWrongVer=Janë dëmtuar skedarët e sistemuesit ose nuk përshtaten me këtë version të Sistemimit. Lutemi të korrigjohet problemi ose të përdoret një kopje e re e programit.
InvalidParameter=Në vijën e komandës u vendos një parametër i pasaktë:%n%n%1
SetupAlreadyRunning=Është duke vepruar Sistemuesi.
WindowsVersionNotSupported=Nuk përshtatet programi me këtë version të Windows-it.
WindowsServicePackRequired=Programi ka nevojë për %1 me Paketë Sigurie %2 ose më të re.
NotOnThisPlatform=Programi nuk do të veprojë në %1.
OnlyOnThisPlatform=Programi duhet të veprojë në %1.
OnlyOnTheseArchitectures=Programi mund të instalohet vetëm në versionet e Windows-it me këto modele arkitekturore të procesorit:%n%n%1
MissingWOW64APIs=Ky version i Windows-it nuk përmban funksionet që kërkohen për të kryer një instalim 64-bit. Lutemi të korrigjohet problemi duke instaluar Paketën e Shërbimit %1.
WinVersionTooLowError=Programi ka nevojë për %1 në versionin %2 a më të ri.
WinVersionTooHighError=Programi nuk mund të instalohet në %1 në versionin %2 a më të ri.
AdminPrivilegesRequired=Instalimi i programit duhet të kryhet nga administratori.
PowerUserPrivilegesRequired=Instalimi i këtij programi duhet të kryhet nga administratori ose nga një Përdorues me Privilegje.
SetupAppRunningError=Sistemuesi vëren se aktualisht po vepron %1.%n%nLutemi të mbyllet dhe të vijoni duke klikuar OK, ose Anuloj për t'u larguar.
UninstallAppRunningError=Çinstaluesi vëren se aktualisht po vepron %1.%n%nLutemi të mbyllet dhe të vijoni duke klikuar OK, ose Anuloj për t'u larguar.

; *** Misc. errors
ErrorCreatingDir=Sistemuesi nuk arrin të krijojë direktorinë "%1"
ErrorTooManyFilesInDir=Nuk mund të krijohen skedarë në direktorinë "%1" sepse ka shumë të tjera

; *** Setup common messages
ExitSetupTitle=Mbyllet sistemuesi
ExitSetupMessage=Nuk ka përfunduar sistemimi. Nuk do të instalohet programi nëse e mbyllni.%n%nQë instalimi të përfundojë, mund ta hapni Sistemuesin një herë tjetër.%n%nTë mbyllet Sistemuesi?
AboutSetupMenuItem=&Për Sistemimin...
AboutSetupTitle=Për Sistemimin
AboutSetupMessage=%1 versioni %2%n%3%n%n%1 faqe zyrtare:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Kthehem
ButtonNext=&Tjetër >
ButtonInstall=&Instaloj
ButtonOK=OK
ButtonCancel=Anuloj
ButtonYes=&Po
ButtonYesToAll=Po, &të gjitha
ButtonNo=&Jo
ButtonNoToAll=J&o, asnjë
ButtonFinish=&Përfundoj
ButtonBrowse=&Shfletoj...
ButtonWizardBrowse=S&hfletoj...
ButtonNewFolder=&Krijoj dosje të re

; *** "Select Language" dialog messages
SelectLanguageTitle=Zgjidhet gjuha e sistemuesit
SelectLanguageLabel=Zgjidhni gjuhën e përdorur gjatë instalimit:

; *** Common wizard text
ClickNext=Vijoni duke klikuar Tjetër, ose Anuloj për ta mbyllur Sistemuesin.
BeveledLabel=
BrowseDialogTitle=Shfletohet dosja
BrowseDialogLabel=Zgjidhni dosjen nga kjo listë, pastaj klikoni OK.
NewFolderName=Dosje të re

; *** "Welcome" wizard page
WelcomeLabel1=Mirësevini te Udhërrëfyesi për Sistemimin e [name]
WelcomeLabel2=[name/ver] do të instalohet në kompjuter.%n%nRekomandohet të mbyllen aplikacionet e tjera para se të vijoni.

; *** "Password" wizard page
WizardPassword=Fjalëkalimi
PasswordLabel1=Instalimi është i mbrojtur me fjalëkalim.
PasswordLabel3=Lutemi të shkruani fjalëkalimin, pastaj vijoni duke klikuar Tjetër. Fjalëkalimi duhet vendosur me kujdes.
PasswordEditLabel=&Fjalëkalimi:
IncorrectPassword=Nuk është i saktë fjalëkalimi. Lutemi të provoni sërish.

; *** "License Agreement" wizard page
WizardLicense=Marrëveshja e licencimit
LicenseLabel=Lutemi të lexohet ky informacion i rëndësishëm para se të vijoni.
LicenseLabel3=Lutemi të lexohet Marrëveshja e licencimit. Duhet të pranoni detyrimet e kësaj marrëveshjeje para se të vijoni me instalimin.
LicenseAccepted=&Pranoj marrëveshjen
LicenseNotAccepted=&Nuk e pranoj marrëveshjen

; *** "Information" wizard pages
WizardInfoBefore=Informacion
InfoBeforeLabel=Lutemi të lexohet ky informacion i rëndësishëm para se të vijoni.
InfoBeforeClickLabel=Kur të jeni gati për të vijuar me Sistemimin, klikoni Tjetër.
WizardInfoAfter=Informacion
InfoAfterLabel=Lutemi të lexohet ky informacion i rëndësishëm para se të vijoni.
InfoAfterClickLabel=Kur të jeni gati për të vijuar me Sistemimin, klikoni Tjetër.

; *** "User Information" wizard page
WizardUserInfo=Informacion i përdoruesit
UserInfoDesc=Vendosni informacionin tuaj.
UserInfoName=&Përdoruesi:
UserInfoOrg=&Organizata:
UserInfoSerial=&Numri i Serisë:
UserInfoNameRequired=Të shkruhet emri.

; *** "Select Destination Location" wizard page
WizardSelectDir=Zgjidhet destinacioni
SelectDirDesc=Ku do të instalohet [name]?
SelectDirLabel3=Sistemuesi do e instalojë [name] në këtë dosje.
SelectDirBrowseLabel=Vijoni duke klikuar Tjetër. Klikoni Shfletoj për të zgjedhur një dosje të ndryshme.
DiskSpaceMBLabel=Kërkohet jo më pak se [mb] MB hapësirë e lirë në disk.
CannotInstallToNetworkDrive=Instalimi nuk mund të kryhet në një disk në rrjet.
CannotInstallToUNCPath=Instalimi nuk mund të kryhet në një shteg UNC.
InvalidPath=Të shkruhet i plotë shtegu i diskut, për shembull:%n%nC:\APP%n%nose shtegu UNC sipas formatit:%n%n\\server\share
InvalidDrive=Nuk ekziston ose nuk hapet disku ose shpërndarësi UNC i zgjedhur. Lutemi të zgjidhet një tjetër.
DiskSpaceWarningTitle=Nuk mjafton hapësira
DiskSpaceWarning=Sistemuesi kërkon të paktën %1 KB hapësirë të lirë për të kryer instalimin, por disku ka vetëm %2 KB të vlefshme.%n%nGjithsesi, të vijohet?
DirNameTooLong=Është tepër i gjatë emri ose shtegu i dosjes.
InvalidDirName=Nuk është i saktë emri i dosjes.
BadDirName32=Emri i dosjes nuk mund të përmbajë këto shkronja:%n%n%1
DirExistsTitle=Ekziston dosja
DirExists=Dosja:%n%n%1%n%nekziston që më parë. Gjithsesi, të instalohet në këtë dosje?
DirDoesntExistTitle=Nuk ekziston dosja
DirDoesntExist=Nuk ekziston dosja:%n%n%1%n%n. Të krijohet dosja?

; *** "Select Components" wizard page
WizardSelectComponents=Zgjidhen komponentët
SelectComponentsDesc=Cilët komponentë do të instalohen?
SelectComponentsLabel2=Zgjidhni komponentët që do të instaloni; mos shënoni komponentë që nuk ju duhen. Klikoni Tjetër kur të jeni gati.
FullInstallation=Instaloj të plotë
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instaloj të ngjeshur
CustomInstallation=Instaloj me zgjedhje
NoUninstallWarningTitle=Komponentë ekzistues
NoUninstallWarning=Sistemuesi vëren se këta komponentë janë instaluar më parë në kompjuter:%n%n%1%n%nNuk do të çinstalohen nëse nuk i zgjidhni.%n%nGjithsesi, të vijohet?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Kjo zgjedhje kërkon jo më pak se [mb] MB hapësirë të lirë në disk.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Zgjidhen detyrat shtesë
SelectTasksDesc=Çfarë detyrash të tjera do të kryhen?
SelectTasksLabel2=Zgjidhni detyrat shtesë që duhet të kryejë Sistemuesi kur të instalojë [name], pastaj klikoni Tjetër.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Zgjidhet dosja në Menynë Nis
SelectStartMenuFolderDesc=Ku do të vendosen shkurtoret e programit?
SelectStartMenuFolderLabel3=Shkurtoret e programit do të krijohen në këtë dosje të Menysë Nis.
SelectStartMenuFolderBrowseLabel=Për të vijuar, klikoni Tjetër. Për të zgjedhur një dosje të ndryshme, klikoni Shfletoj.
MustEnterGroupName=Të shkruhet emri i dosjes.
GroupNameTooLong=Është tepër i gjatë emri ose shtegu i dosjes.
InvalidGroupName=Nuk është i saktë emri i dosjes.
BadGroupName=Emri i dosjes nuk duhet të përmbajë asnjë nga këto shkronja:%n%n%1
NoProgramGroupCheck2=&Nuk krijoj dosje në Menynë Nis

; *** "Ready to Install" wizard page
WizardReady=Gati të instalohet
ReadyLabel1=Sistemuesi është gati të instalojë [name] në kompjuter.
ReadyLabel2a=Klikoni Instaloj për të vijuar instalimin ose ndryshoni ndonjë parametër duke klikuar Kthehem.
ReadyLabel2b=Klikoni Instaloj për të vijuar instalimin.
ReadyMemoUserInfo=Informacioni i përdoruesit:
ReadyMemoDir=Destinacioni:
ReadyMemoType=Lloji i sistemimit:
ReadyMemoComponents=Komponentët e zgjedhur:
ReadyMemoGroup=Dosja në Menynë Nis:
ReadyMemoTasks=Detyra shtesë:

; *** "Preparing to Install" wizard page
WizardPreparing=Gati të instalohet
PreparingDesc=Sistemuesi është gati të instalojë [name] në kompjuter.
PreviousInstallNotCompleted=Nuk ka përfunduar instalimi/heqja e programit të mëparshëm. Instalimi do të përfundojë kur kompjuteri të rinisë.%n%nPas rinisjes, hapni sërish Sistemuesin që të kryhet instalimi i [name].
CannotContinue=Nuk mund të vijojë sistemimi. Lutemi të mbyllet duke klikuar Anuloj.
ApplicationsFound=Këto aplikacione përdorin skedarë që do të përditësohen. Rekomandohet që Sistemuesi t'i mbyllë ato automatikisht me lejen tuaj.
ApplicationsFound2=Këto aplikacione përdorin skedarë që do të përditësohen. Rekomandohet që Sistemuesi t'i mbyllë ato automatikisht me lejen tuaj. Pasi instalimi të përfundojë, Sistemuesi do të tentojë t'i hapë sërish.
CloseApplications=&Mbyll automatikisht aplikacionet
DontCloseApplications=&Mos i mbyll aplikacionet
ErrorCloseApplications=Jo të gjitha aplikacionet u mbyllën automatikisht. Rekomandohet të mbyllen të gjithë skedarët para se të vijojë përditësimi.

; *** "Installing" wizard page
WizardInstalling=Instalim
InstallingLabel=Kini durim që Sistemuesi të instalojë [name] në kompjuter.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Përfundon udhërrëfimi për Sistemimin e [name]
FinishedLabelNoIcons=Sistemuesi përfundoi instalimin në kompjuter të [name].
FinishedLabel=Sistemuesi përfundoi instalimin në kompjuter të [name]. Aplikacioni mund të hapet me anë të ikonës së instaluar.
ClickFinish=Klikoni Përfundoj për ta mbyllur Sistemuesin.
FinishedRestartLabel=Duhet të riniset kompjuteri që Sistemuesi të përfundojë instalimin e [name]. Të rinisë tani?
FinishedRestartMessage=Duhet të riniset kompjuteri që Sistemuesi të përfundojë instalimin e [name].%n%nTë rinisë tani?
ShowReadmeCheck=Po, dua të shoh skedarin README
YesRadio=&Po, rinis kompjuterin tani
NoRadio=&Jo, do e rinis kompjuterin më vonë
; used for example as 'Run MyProg.exe'
RunEntryExec=Hap %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Shoh %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Sistemuesi kërkon diskun tjetër
SelectDiskLabel2=Lutemi të vendoset Disku %1 dhe klikoni OK.%n%nNëse skedarët në disk ndodhen në një dosje të ndryshme, përcaktoni shtegun e saktë ose klikoni Shfletoj.
PathLabel=&Shtegu:
FileNotInDir2=Skedari "%1" nuk ndodhet në "%2". Lutemi të vendoset disku i saktë ose të zgjidhet një dosje e ndryshme.
SelectDirectoryLabel=Lutemi të përcaktohet vendi i diskut tjetër.

; *** Installation phase messages
SetupAborted=Nuk përfundoi sistemimi.%n%nLutemi të korrigjohet problemi dhe të provoni sërish.
EntryAbortRetryIgnore=Klikoni Provoj sërish për ta përsëritur, Shmang për të vijuar me çdo mënyrë, ose Ndërpres për të anuluar instalimin.

; *** Installation status messages
StatusClosingApplications=Mbyllen aplikacionet...
StatusCreateDirs=Krijohen direktoritë...
StatusExtractFiles=Nxirren skedarët...
StatusCreateIcons=Krijohen shkurtoret...
StatusCreateIniEntries=Krijohen elementet INI...
StatusCreateRegistryEntries=Krijohen elementet në regjistër...
StatusRegisterFiles=Regjistrohen skedarët...
StatusSavingUninstall=Ruhet informacioni i çinstalimit...
StatusRunProgram=Përfundon instalimi...
StatusRestartingApplications=Rinisen aplikacionet...
StatusRollback=Kthehen ndryshimet...

; *** Misc. errors
ErrorInternal2=Problem i brendshëm: %1
ErrorFunctionFailedNoCode=%1 ndali
ErrorFunctionFailed=%1 ndali; kodi %2
ErrorFunctionFailedWithMessage=%1 ndali; kodi %2.%n%3
ErrorExecutingProgram=Nuk ekzekutohet skedari:%n%1

; *** Registry errors
ErrorRegOpenKey=Problem me hapjen e kodit të regjistrit:%n%1\%2
ErrorRegCreateKey=Problem me krijimin e kodit të regjistrit:%n%1\%2
ErrorRegWriteKey=Problem me shkrimin e kodit të regjistrit:%n%1\%2

; *** INI errors
ErrorIniEntry=Problem me krijimin e elementit INI te skedari "%1".

; *** File copying errors
FileAbortRetryIgnore=Klikoni Provoj sërish për ta përsëritur, Shmang për ta evituar këtë skedarin (nuk rekomandohet), ose Ndërpres për të anuluar instalimin.
FileAbortRetryIgnore2=Klikoni Provoj sërish për ta përsëritur, Shmang për të vijuar me çdo mënyrë (nuk rekomandohet), ose Ndërpres për të anuluar instalimin.
SourceIsCorrupted=Është dëmtuar skedari burimor
SourceDoesntExist=Nuk ekziston skedari burimor "%1"
ExistingFileReadOnly=Skedari ekzistues vetëm mund të lexohet.%n%nKlikoni Provoj sërish për të hequr atributin e leximit dhe për ta provuar sërish, Shmang për ta evituar këtë skedarin, ose Ndërpres për të anuluar çinstalimin.
ErrorReadingExistingDest=Ka një problem me leximin e skedarit ekzistues:
FileExists=Skedari ekziston që më parë.%n%nTë mbishkruhet?
ExistingFileNewer=Skedari ekzistues është me e re sesa ajo që Sistemuesi po provon të instalojë. Rekomandohet që të mbahet skedari ekzistues.%n%nTë mbahet skedari ekzistues?
ErrorChangingAttr=Ka një problem me ndryshimin e atributeve të skedarëve ekzistues:
ErrorCreatingTemp=Ka një problem me krijimin e skedarit në direktorinë e destinacionit:
ErrorReadingSource=Ka një problem me leximin e skedarit nga burimi:
ErrorCopying=Ka një problem me kopjimin e skedarit:
ErrorReplacingExistingFile=Ka një problem me zëvendësimin e skedarit ekzistues:
ErrorRestartReplace=Ndali RestartReplace:
ErrorRenamingTemp=Ka një problem me emërtimin e skedarit në direktorinë e destinacionit:
ErrorRegisterServer=Nuk mund të regjistrohet DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 ndali me kodin e daljes %1
ErrorRegisterTypeLib=Nuk mund të regjistrohet lloji i librarisë: %1

; *** Post-installation errors
ErrorOpeningReadme=Ka një problem me hapjen e skedarit README.
ErrorRestartingComputer=Sistemuesi nuk e rinisi dot kompjuterin. Lutemi ta bëni vetë.

; *** Uninstaller messages
UninstallNotFound=Nuk ekziston skedari "%1". Nuk mund të çinstalohet.
UninstallOpenError=Nuk mund të hapet skedari "%1". Nuk mund të çinstalohet.
UninstallUnsupportedVer=Formati i ditarit të çinstalimit "%1" nuk njihet nga ky version i çinstaluesit. Nuk mund të çinstalohet
UninstallUnknownEntry=Ditari i çinstalimit ka një element të panjohur (%1)
ConfirmUninstall=Të fshihet %1 bashkë me komponentët e vet?
UninstallOnlyOnWin64=Instalimi mund të kryhet vetëm në Windows 64-bit.
OnlyAdminCanUninstall=Instalimi mund të çinstalohet vetëm nga përdoruesit me privilegje administruese.
UninstallStatusLabel=Kini durim ndërkohë që %1 fshihet nga kompjuteri.
UninstalledAll=%1 u fshi me sukses nga kompjuteri.
UninstalledMost=Përfundoi çinstalimi i %1.%n%nNuk mund të fshiheshin disa elemente. Këto mund t'i fshini vetë.
UninstalledAndNeedsRestart=Duhet rinisur kompjuteri që të përfundojë çinstalimi i %1.%n%nTë rinisë tani?
UninstallDataCorrupted=Skedari "%1" është dëmtuar. Nuk mund të çinstalohet.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Të fshihet skedari i ndarë?
ConfirmDeleteSharedFile2=Sistemuesi tregon se ky skedar i ndarë nuk përdoret më nga programet. Të fshihet ky skedar i ndarë?%n%nNëse skedari është duke u përdorur nga ndonjë program tjetër, ai mund të mos punojë siç duhet. Nëse nuk jeni të sigurt, të zgjidhet Jo. Mbajtja e skedarit në sistem nuk shkakton dëm.
SharedFileNameLabel=Emri i skedarit:
SharedFileLocationLabel=Vendi:
WizardUninstalling=Statusi i çinstalimit
StatusUninstalling=Çinstalon %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instalon %1.
ShutdownBlockReasonUninstallingApp=Çinstalon %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versioni %2
AdditionalIcons=Ikona shtesë:
CreateDesktopIcon=Krijoj një ikonë në &tryezë
CreateQuickLaunchIcon=Krijoj një ikonë në &Quick Launch
ProgramOnTheWeb=%1 në internet
UninstallProgram=Çinstaloj %1
LaunchProgram=Hap %1
AssocFileExtension=&Shoqëroj %1 me siglën %2
AssocingFileExtension=%1 shoqërohet me siglën %2...
AutoStartProgramGroupDescription=Fillimi:
AutoStartProgram=Filloj automatikisht %1
AddonHostProgramNotFound=%1 nuk ndodhet në dosjen e përzgjedhur.%n%nGjithsesi, të vijohet?
