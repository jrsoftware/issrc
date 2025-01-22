; *** Inno Setup version 6.4.0+ Albanian messages ***
;
; To download user-contributed translations of this file, go to:
;   https://jrsoftware.org/files/istrans/
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
SetupLdrStartupMessage=Do të instalohet %1. Dëshironi të vijoni?
LdrCannotCreateTemp=Nuk mund të krijohej skedari i përkohshëm. Sistemimi u ndërpre
LdrCannotExecTemp=Nuk mund të ekzekutohej skedari në direktorinë e përkohshme. Sistemimi u ndërpre
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nGabim %2: %3
SetupFileMissing=Mungon skedari %1 në direktorinë e instalimit. Lutemi të korrigjoni problemin ose të siguroni një kopje tjetër të programit.
SetupFileCorrupt=Janë dëmtuar skedarët e Sistemuesit. Lutemi të përdoret një kopje e re e programit.
SetupFileCorruptOrWrongVer=Janë dëmtuar skedarët e sistemuesit ose nuk përshtaten me këtë version të Sistemimit. Lutemi të korrigjoni problemin ose të siguroni një kopje tjetër të programit.
InvalidParameter=Në vijën e komandës u vendos një parametër i pasaktë:%n%n%1
SetupAlreadyRunning=Është duke vepruar Sistemuesi.
WindowsVersionNotSupported=Programi nuk përshtatet me këtë version të Windows-it.
WindowsServicePackRequired=Programi ka nevojë për %1 me Paketë Sigurie %2 ose më të re.
NotOnThisPlatform=Programi nuk do të veprojë në %1.
OnlyOnThisPlatform=Programi duhet të veprojë në %1.
OnlyOnTheseArchitectures=Programi mund të instalohet vetëm në versionet e Windows-it me procesorë për këto modele arkitekturore:%n%n%1
WinVersionTooLowError=Programi ka nevojë për %1 në versionin %2 a më të ri.
WinVersionTooHighError=Programi nuk mund të instalohet në %1 në versionin %2 a më të ri.
AdminPrivilegesRequired=Instalimi i programit duhet kryer nga administratori.
PowerUserPrivilegesRequired=Instalimi i programi duhet kryer nga administratori ose nga një Përdorues me privilegje.
SetupAppRunningError=Sistemuesi vëren se aktualisht po vepron %1.%n%nLutemi ta mbyllni dhe të vijoni duke klikuar OK, ose Anuloj për t'u larguar.
UninstallAppRunningError=Çinstaluesi vëren se aktualisht po vepron %1.%n%nLutemi ta mbyllni dhe të vijoni duke klikuar OK, ose Anuloj për t'u larguar.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Zgjidhet mënyra e kryerjes së instalimit
PrivilegesRequiredOverrideInstruction=Zgjidhni mënyrën e instalimit
PrivilegesRequiredOverrideText1=%1 mund të instalohet për të gjithë përdoruesit (duhen privilegje administrative) ose vetëm për ju.
PrivilegesRequiredOverrideText2=%1 mund të instalohet vetëm për ju ose për të gjithë përdoruesit (duhen privilegje administrative).
PrivilegesRequiredOverrideAllUsers=Instaloj për të &gjithë përdoruesit
PrivilegesRequiredOverrideAllUsersRecommended=Instaloj për të &gjithë përdoruesit (rekomandohet)
PrivilegesRequiredOverrideCurrentUser=Instaloj për &veten time
PrivilegesRequiredOverrideCurrentUserRecommended=Instaloj për &veten time (rekomandohet)

; *** Misc. errors
ErrorCreatingDir=Sistemuesi nuk arriti të krijojë direktorinë "%1"
ErrorTooManyFilesInDir=Nuk mund të krijohen skedarë në direktorinë "%1", sepse ka shumë të tjerë

; *** Setup common messages
ExitSetupTitle=Mbyllet sistemuesi
ExitSetupMessage=Sistemimi nuk ka përfunduar. Programi nuk do të instalohet nëse e mbyllni.%n%nQë instalimi të përfundojë, mund ta hapni Sistemuesin një herë tjetër.%n%nDo e mbyllni Sistemuesin?
AboutSetupMenuItem=&Për Sistemimin...
AboutSetupTitle=Për Sistemimin
AboutSetupMessage=%1 versioni %2%n%3%n%n%1 faqja zyrtare:%n%4
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
ButtonNewFolder=&Hap dosje të re

; *** "Select Language" dialog messages
SelectLanguageTitle=Zgjidhet gjuha e sistemuesit
SelectLanguageLabel=Zgjidhni gjuhën e përdorur gjatë instalimit.

; *** Common wizard text
ClickNext=Vijoni duke klikuar Tjetër ose Anuloj për ta mbyllur Sistemuesin.
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
IncorrectPassword=Fjalëkalimi nuk është i saktë. Lutemi ta provoni prapë.

; *** "License Agreement" wizard page
WizardLicense=Marrëveshja e licensimit
LicenseLabel=Lutemi ta lexoni këtë informacion të rëndësishëm para se të vijoni.
LicenseLabel3=Lutemi ta lexoni Marrëveshjen e licensimit. Duhet të pranoni detyrimet e marrëveshjes para se të vijoni me instalimin.
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
WizardUserInfo=Informacion mbi përdoruesin
UserInfoDesc=Lutemi të vendosni informacionin.
UserInfoName=&Përdoruesi:
UserInfoOrg=&Organizata:
UserInfoSerial=&Numri i serisë:
UserInfoNameRequired=Duhet vendosur emri.

; *** "Select Destination Location" wizard page
WizardSelectDir=Zgjidhet destinacioni
SelectDirDesc=Ku do të instalohet [name]?
SelectDirLabel3=Sistemuesi do e instalojë [name] në këtë dosje.
SelectDirBrowseLabel=Vijoni duke klikuar Tjetër. Klikoni Shfletoj për të zgjedhur një dosje të ndryshme.
DiskSpaceGBLabel=Kërkohet jo më pak se [gb] GB hapësirë e lirë në disk.
DiskSpaceMBLabel=Kërkohet jo më pak se [mb] MB hapësirë e lirë në disk.
CannotInstallToNetworkDrive=Instalimi nuk mund të kryhet në diskun e një rrjeti kompjuterik.
CannotInstallToUNCPath=Instalimi nuk mund të kryhet në një shteg UNC.
InvalidPath=Duhet vendosur shtegu i plotë i diskut, për shembull:%n%nC:\APP%n%nose shtegu UNC sipas formatit:%n%n\\server\share
InvalidDrive=Nuk ekziston ose nuk hapet disku a shpërndarësi UNC i zgjedhur. Lutemi të zgjidhni një tjetër.
DiskSpaceWarningTitle=Nuk mjafton hapësira
DiskSpaceWarning=Sistemuesi kërkon të paktën %1 KB hapësirë të lirë për të kryer instalimin, por disku ka vetëm %2 KB të vlefshme.%n%nGjithsesi, të vijohet?
DirNameTooLong=Është tepër i gjatë emri ose shtegu i dosjes.
InvalidDirName=Emri i dosjes nuk është i saktë.
BadDirName32=Emri i dosjes nuk mund të përmbajë këto shkronja:%n%n%1
DirExistsTitle=Dosja ekziston
DirExists=Dosja:%n%n%1%n%nekziston që më parë. Gjithsesi, doni ta instaloni në këtë dosje?
DirDoesntExistTitle=Nuk ekziston dosja
DirDoesntExist=Nuk ekziston dosja:%n%n%1%n%n. Doni ta krijoni dosjen?

; *** "Select Components" wizard page
WizardSelectComponents=Zgjidhen komponentët
SelectComponentsDesc=Cilët komponentë duhen instaluar?
SelectComponentsLabel2=Zgjidhni komponentët që doni të instaloni; mos shënoni komponentë që nuk ju duhen. Klikoni Tjetër kur të jeni gati.
FullInstallation=Instaloj të plotë
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalim kompakt
CustomInstallation=Instalim i personalizuar
NoUninstallWarningTitle=Komponentët ekzistues
NoUninstallWarning=Sistemuesi vëren se këta komponentë janë instaluar më parë në kompjuter:%n%n%1%n%nNuk do të çinstalohen nëse nuk i zgjidhni.%n%nGjithsesi, doni të vijoni?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Kjo zgjedhje kërkon jo më pak se [gb] GB hapësirë të lirë në disk.
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
MustEnterGroupName=Duhet vendosur emri i dosjes.
GroupNameTooLong=Është tepër i gjatë emri ose shtegu i dosjes.
InvalidGroupName=Emri i dosjes nuk është i saktë.
BadGroupName=Emri i dosjes nuk duhet të përmbajë asnjë nga këto shkronja:%n%n%1
NoProgramGroupCheck2=&Nuk krijoj dosje në Menunë Nis

; *** "Ready to Install" wizard page
WizardReady=Gati për instalim
ReadyLabel1=Sistemuesi është gati të instalojë [name] në kompjuter.
ReadyLabel2a=Klikoni Instaloj për të vijuar instalimin ose ndryshoni ndonjë parametër duke klikuar Kthehem.
ReadyLabel2b=Klikoni Instaloj për të vijuar instalimin.
ReadyMemoUserInfo=Informacion mbi përdoruesin:
ReadyMemoDir=Destinacioni:
ReadyMemoType=Lloji i sistemimit:
ReadyMemoComponents=Komponentët e zgjedhur:
ReadyMemoGroup=Dosja në Menunë Nis:
ReadyMemoTasks=Detyrat shtesë:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=Shkarkon skedarët shtesë...
ButtonStopDownload=&Ndaloj shkarkimin
StopDownload=E konfirmoni ndalimin e shkarkimit?
ErrorDownloadAborted=Shkarkimi u ndërpre
ErrorDownloadFailed=E pamundur të shkarkohej: %1 %2
ErrorDownloadSizeFailed=E pamundur të merrej madhësia e skedarit: %1 %2
ErrorFileHash1=E pamundur të merrej hashi i skedarit: %1
ErrorFileHash2=Hashi i skedarit është i pasaktë: pritej %1, doli %2
ErrorProgress=Vazhdim i pasaktë: %1 of %2
ErrorFileSize=Madhësia e skedarit është e pasaktë: pritej %1, doli %2

; *** TExtractionWizardPage wizard page and Extract7ZipArchive
ExtractionLabel=Nxjerr skedarët shtesë...
ButtonStopExtraction=&Ndaloj nxjerrjen
StopExtraction=E konfirmoni ndalimin e nxjerrjes?
ErrorExtractionAborted=Nxjerrja u ndërpre
ErrorExtractionFailed=E pamundur të nxirrej: %1

; *** "Preparing to Install" wizard page
WizardPreparing=Gati për instalim
PreparingDesc=Sistemuesi është gati për instalimin e [name] në kompjuter.
PreviousInstallNotCompleted=Nuk ka përfunduar instalimi/heqja e programit të mëparshëm. Instalimi do të përfundojë kur kompjuteri të rinisë.%n%nPas rinisjes, hapeni sërish Sistemuesin që të përfundojë instalimin e [name].
CannotContinue=Sistemimi nuk mund të vijojë. Lutemi të mbyllet duke klikuar Anuloj.
ApplicationsFound=Këto aplikacione përdorin skedarë që do të përditësohen. Rekomandohet që Sistemuesi t'i mbyllë ata automatikisht me lejen tuaj.
ApplicationsFound2=Këto aplikacione përdorin skedarë që do të përditësohen. Rekomandohet që Sistemuesi t'i mbyllë ata automatikisht me lejen tuaj. Pasi instalimi të përfundojë, Sistemuesi do të tentojë t'i hapë sërish.
CloseApplications=&Mbyll automatikisht aplikacionet
DontCloseApplications=&Mos i mbyll aplikacionet
ErrorCloseApplications=Jo të gjitha aplikacionet u mbyllën automatikisht. Rekomandohet të mbyllen të gjithë skedarët para se të vijojë përditësimi.
PrepareToInstallNeedsRestart=Duhet rinisur kompjuteri për arsye të sistemimit. Pas rinisjes së kompjuterit, hapeni sërish Sistemuesin që të përfundojë instalimin e [name].%n%nDoni ta rinisni tani kompjuterin?

; *** "Installing" wizard page
WizardInstalling=Instalim
InstallingLabel=Kini durim që Sistemuesi të instalojë [name] në kompjuter.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Përfundon udhërrëfimi për Sistemimin e [name]
FinishedLabelNoIcons=Sistemuesi përfundoi instalimin në kompjuter të [name].
FinishedLabel=Sistemuesi përfundoi instalimin në kompjuter të [name]. Aplikacioni mund të hapet me anë të ikonës së instaluar.
ClickFinish=Klikoni Përfundoj për ta mbyllur Sistemuesin.
FinishedRestartLabel=Duhet të riniset kompjuteri që Sistemuesi të përfundojë instalimin e [name]. Doni ta rinisni tani kompjuterin?
FinishedRestartMessage=Duhet të riniset kompjuteri që Sistemuesi të përfundojë instalimin e [name].%n%nDoni ta rinisni tani kompjuterin?
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
SetupAborted=Nuk përfundoi sistemimi.%n%nLutemi të korrigjoni problemin dhe provojeni prapë.
AbortRetryIgnoreSelectAction=Zgjidhni veprimin
AbortRetryIgnoreRetry=&Provoj prapë
AbortRetryIgnoreIgnore=&Anashkaloj problemin dhe vazhdoj
AbortRetryIgnoreCancel=Anuloj instalimin

; *** Installation status messages
StatusClosingApplications=Mbyll aplikacionet...
StatusCreateDirs=Krijon direktoritë...
StatusExtractFiles=Nxjerr skedarët...
StatusCreateIcons=Krijon shkurtoret...
StatusCreateIniEntries=Krijon elementet INI...
StatusCreateRegistryEntries=Krijon elementet në regjistër...
StatusRegisterFiles=Regjistron skedarët...
StatusSavingUninstall=Ruan informacionin e çinstalimit...
StatusRunProgram=Përfundon instalimin...
StatusRestartingApplications=Rinis aplikacionet...
StatusRollback=Kthen ndryshimet...

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
FileAbortRetryIgnoreSkipNotRecommended=&Anashkaloj skedarin (nuk rekomandohet)
FileAbortRetryIgnoreIgnoreNotRecommended=&Anashkaloj problemin dhe vazhdoj (nuk rekomandohet)
SourceIsCorrupted=Është dëmtuar skedari burimor
SourceDoesntExist=Nuk ekziston skedari burimor "%1"
ExistingFileReadOnly2=Skedari ekzistues nuk mund të zëvendësohet, sepse është vetëm si i lexueshëm.
ExistingFileReadOnlyRetry=&Hiqni atributin e leximit dhe provojeni prapë
ExistingFileReadOnlyKeepExisting=&Mbaj skedarin ekzistues
ErrorReadingExistingDest=Ka një problem me leximin e skedarit ekzistues:
FileExistsSelectAction=Zgjidhni veprimin
FileExists2=Skedari ekziston që më parë.
FileExistsOverwriteExisting=&Mbishkruaj skedarin ekzistues
FileExistsKeepExisting=&Mbaj skedarin ekzistues
FileExistsOverwriteOrKeepAll=&Veproj njësoj për rastet e tjera
ExistingFileNewerSelectAction=Zgjidhni veprimin
ExistingFileNewer2=Skedari ekzistues është më i ri sesa ai që Sistemuesi po provon të instalojë.
ExistingFileNewerOverwriteExisting=&Mbishkruaj skedarin ekzistues
ExistingFileNewerKeepExisting=&Mbaj skedarin ekzistues (rekomandohet)
ExistingFileNewerOverwriteOrKeepAll=&Veproj njësoj për rastet e tjera
ErrorChangingAttr=Ka një problem me ndryshimin e atributeve të skedarëve ekzistues:
ErrorCreatingTemp=Ka një problem me krijimin e skedarit në direktorinë e destinacionit:
ErrorReadingSource=Ka një problem me leximin e skedarit nga burimi:
ErrorCopying=Ka një problem me kopjimin e skedarit:
ErrorReplacingExistingFile=Ka një problem me zëvendësimin e skedarit ekzistues:
ErrorRestartReplace=Ndali RestartReplace:
ErrorRenamingTemp=Ka një problem me emërtimin e skedarit në direktorinë e destinacionit:
ErrorRegisterServer=DLL/OCX nuk mund të regjistrohet: %1
ErrorRegSvr32Failed=RegSvr32 ndali me kodin e daljes %1
ErrorRegisterTypeLib=Lloji i librarisë nuk mund të regjistrohet: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=Gjithë përdoruesit
UninstallDisplayNameMarkCurrentUser=Këtë përdorues

; *** Post-installation errors
ErrorOpeningReadme=Ka një problem me hapjen e skedarit README.
ErrorRestartingComputer=Sistemuesi nuk e rinisi dot kompjuterin. Lutemi ta bëni vetë.

; *** Uninstaller messages
UninstallNotFound=Nuk ekziston skedari "%1". Nuk mund të çinstalohet.
UninstallOpenError=Nuk hapet skedari "%1". Nuk mund të çinstalohet
UninstallUnsupportedVer=Formati i ditarit të çinstalimit "%1" nuk njihet nga ky version i çinstaluesit. Nuk mund të çinstalohet
UninstallUnknownEntry=Ditari i çinstalimit ka një element të panjohur (%1)
ConfirmUninstall=Doni ta fshini %1 bashkë me komponentët e vet?
UninstallOnlyOnWin64=Instalimi mund të kryhet vetëm në Windows 64-bit.
OnlyAdminCanUninstall=Instalimi mund të çinstalohet vetëm nga përdoruesit me privilegje administruese.
UninstallStatusLabel=Kini durim ndërkohë që %1 fshihet nga kompjuteri.
UninstalledAll=%1 u fshi me sukses nga kompjuteri.
UninstalledMost=Përfundoi çinstalimi i %1.%n%nDisa elemente nuk mund të fshiheshin. Këto mund t'i fshini vetë.
UninstalledAndNeedsRestart=Duhet rinisur kompjuteri që të përfundojë çinstalimi i %1.%n%nDoni ta rinisni tani kompjuterin?
UninstallDataCorrupted=Skedari "%1" është dëmtuar. Nuk mund të çinstalohet

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Do e fshini skedarin e ndarë?
ConfirmDeleteSharedFile2=Ky skedar i ndarë nuk përdoret më nga programet, sipas sistemit. Doni që Çinstalimi ta fshijë skedarin?%n%nNëse skedari është duke u përdorur nga ndonjë program tjetër, ai mund të mos punojë siç duhet. Nëse nuk jeni të sigurt, zgjidhni Jo. Nuk ka dëm nga mbajtja e skedarit në sistem.
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
AssocingFileExtension=Shoqëron %1 me siglën %2...
AutoStartProgramGroupDescription=Fillimi:
AutoStartProgram=Filloj automatikisht %1
AddonHostProgramNotFound=%1 nuk ndodhet në dosjen e përzgjedhur.%n%nGjithsesi, doni të vijoni?
