; *** Inno Setup version 5.1.11+ Albanian messages ***
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; Përkthyer nga Besmir Godole
; Posta-e: bgodole@gmail.com
; Më kontaktoni me postë-e për ndonjë gabim ose sugjerim rreth përkthimit.

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
SetupLdrStartupMessage=Tani do instalosh %1. Do vijosh?
LdrCannotCreateTemp=Nuk mund të krijohet një skedë kohëshkurtër. Ndërpritet sistemimi
LdrCannotExecTemp=Nuk mund të ekzekutohet skeda në direktorinë kohëshkurtër. Ndërpritet sistemimi

; *** Startup error messages
LastErrorMessage=%1.%n%nGabim %2: %3
SetupFileMissing=Skeda %1 mungon në direktorinë e instalimit. Të lutem korrigjo problemin ose gjej një kopje të re të programit.
SetupFileCorrupt=Skedat e sistemimit janë prishur. Të lutem gjej një kopje të re të programit.
SetupFileCorruptOrWrongVer=Skedat e sistemimit janë prishur ose nuk pajtohen me këtë version të Sistemimit. Të lutem korrigjo problemin ose gjej një kopje të re të programit.
NotOnThisPlatform=Ky program nuk do veprojë në %1.
OnlyOnThisPlatform=Ky program duhet të veprojë në %1.
OnlyOnTheseArchitectures=Ky program mund të instalohet vetëm në versionet e Windows-it që janë modeluar për këto modele arkitekturore të procesorit:%n%n%1
MissingWOW64APIs=Versioni i Windows-it që ke nuk përmban funksionet që kërkon Sistemimi për të kryer një instalim 64-bit. Për ta korrigjuar këtë problem, të lutem instalo Paketën e Shërbimit %1.
WinVersionTooLowError=Këtij programi i nevojitet %1 me version %2 a më vonë.
WinVersionTooHighError=Ky program nuk mund të instalohet në %1 me versionin %2 a më vonë.
AdminPrivilegesRequired=Kur e instalon këtë program duhet të hysh si administrator.
PowerUserPrivilegesRequired=Kur e instalon këtë program duhet të hysh si administrator ose anëtar i grupit Përdorues me Fuqi.
SetupAppRunningError=Sistemimi diktoi se aktualisht po vepron %1.%n%nTë lutem mbylli tani tërë rastet e tij dhe kliko OK për të vijuar, ose Anulo për të dalë.
UninstallAppRunningError=Çinstalimi diktoi se aktualisht po vepron %1.%n%nTë lutem mbylli tani tërë rastet e tij dhe kliko OK për të vijuar, ose Anulo për të dalë.

; *** Misc. errors
ErrorCreatingDir=Sistemimi nuk arrin të krijojë direktorinë "%1"
ErrorTooManyFilesInDir=Në direktorinë "%1" nuk mund të krijohen skeda sepse ka shumë të tjera

; *** Setup common messages
ExitSetupTitle=Dalje nga Sistemimi
ExitSetupMessage=Sistemimi nuk është plotësuar. Po e mbylle, programi nuk instalohet.%n%nQë instalimi të plotësohet, mund ta lëshosh Sistemimin një herë tjetër.%n%nTë Mbyllet Sistemimi?
AboutSetupMenuItem=&Për Sistemimin...
AboutSetupTitle=Për Sistemimin
AboutSetupMessage=%1 versioni %2%n%3%n%n%1 faqe zyrtare:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Pas
ButtonNext=&Tjetër >
ButtonInstall=&Instalo
ButtonOK=OK
ButtonCancel=Anulo
ButtonYes=&Po
ButtonYesToAll=Po, &Gjithçka
ButtonNo=&Jo
ButtonNoToAll=J&o, Asnjë
ButtonFinish=&Përfundo
ButtonBrowse=&Shfleto...
ButtonWizardBrowse=S&hfleto...
ButtonNewFolder=&Krijo Dosje të Re

; *** "Select Language" dialog messages
SelectLanguageTitle=Zgjedh Gjuhën e Sistemimit
SelectLanguageLabel=Zgjidhe gjuhën e përdorimit gjatë instalimit:

; *** Common wizard text
ClickNext=Kliko Tjetër për të vijuar, ose Anulo që Sistemimi të mbyllet.
BeveledLabel=
BrowseDialogTitle=Shfleton dosjen
BrowseDialogLabel=Zgjidhe një dosje në këtë listë, pastaj kliko OK.
NewFolderName=Dosje e re

; *** "Welcome" wizard page
WelcomeLabel1=Mirë se erdhe te Udhërrëfyesi për Sistemimin e [name]
WelcomeLabel2=[name/ver] do instalohet tani në kompjuter.%n%nPara se të vijosh, rekomandohet të mbyllen aplikimet e tjera.

; *** "Password" wizard page
WizardPassword=Fjalëkalimi
PasswordLabel1=Ky instalim është i mbrojtur me fjalëkalim.
PasswordLabel3=Të lutem shkruaj fjalëkalimin, pastaj kliko Tjetër që të vijosh. Fjalëkalimi duhet vënë me kujdes.
PasswordEditLabel=&Fjalëkalimi:
IncorrectPassword=Fjalëkalimi i shkruar nuk është i saktë. Të lutem provoje përsëri.

; *** "License Agreement" wizard page
WizardLicense=Marrëveshja e Licencës
LicenseLabel=Të lutem lexo informacionin e rëndësishëm më poshtë para se të vijosh.
LicenseLabel3=Të lutem lexo Marrëveshjen e Licencës më poshtë. Para se të vijosh me instalimin, duhet të pranosh detyrimet e kësaj marrëveshjeje.
LicenseAccepted=&Pranoj marrëveshjen
LicenseNotAccepted=&Nuk e pranoj marrëveshjen

; *** "Information" wizard pages
WizardInfoBefore=Informacion
InfoBeforeLabel=Të lutem lexo informacionin e rëndësishëm më poshtë para se të vijosh.
InfoBeforeClickLabel=Kur të jesh gati për të vijuar me Sistemimin, kliko Tjetër.
WizardInfoAfter=Informacion
InfoAfterLabel=Të lutem lexo informacionin e rëndësishëm më poshtë para se të vijosh.
InfoAfterClickLabel=Kur të jesh gati për të vijuar me Sistemimin, kliko Tjetër.

; *** "User Information" wizard page
WizardUserInfo=Informacion i Përdoruesit
UserInfoDesc=Të lutem vendos informacionin tënd.
UserInfoName=&Përdoruesi:
UserInfoOrg=&Organizata:
UserInfoSerial=&Numri i Serisë:
UserInfoNameRequired=Duhet shkruar një emër.

; *** "Select Destination Location" wizard page
WizardSelectDir=Zgjedh Destinacionin
SelectDirDesc=Ku duhet të instalohet [name]?
SelectDirLabel3=Sistemimi do e instalojë [name] në këtë dosje.
SelectDirBrowseLabel=Për të vijuar, kliko Tjetër. Nëse do të zgjedhësh një dosje ndryshe, kliko Shfleto.
DiskSpaceMBLabel=Kërkon jo më pak se [mb] MB hapësirë të lirë në disk.
ToUNCPathname=Sistemimi nuk mund të instalojë në një shteg UNC. Nëse po provon instalimin në rrjet, duhet të lokalizosh drajvin e rrjetit.
InvalidPath=Duhet të shkruhet shtegu i plotë me shkronjën e drajvit; për shembull:%n%nC:\APP%n%nose shtegu UNC në formën:%n%n\\server\share
InvalidDrive=Drajvi ose shpërndarësi UNC i zgjedhur nuk ekziston ose nuk hapet. Të lutem zgjidhe një tjetër.
DiskSpaceWarningTitle=Hapësirë e Pamjaftueshme
DiskSpaceWarning=Sistemimi kërkon të paktën %1 KB hapësirë të lirë për të instaluar, por ky drajv ka vetëm %2 KB me vlerë.%n%nGjithsesi, do vijosh?
DirNameTooLong=Emri ose shtegu i dosjes është tepër i gjatë.
InvalidDirName=Emri i dosjes nuk ka vlerë.
BadDirName32=Emri i dosjes nuk mund të përmbajë këto shkronja:%n%n%1
DirExistsTitle=Dosje Ekzistuese
DirExists=Dosja:%n%n%1%n%nekziston që më parë. Gjithsesi, të instalohet në këtë dosje?
DirDoesntExistTitle=Dosje Joekzistuese
DirDoesntExist=Dosja:%n%n%1%n%nnuk ekziston. Të krijohet kjo dosje?

; *** "Select Components" wizard page
WizardSelectComponents=Zgjedh Përbërësit
SelectComponentsDesc=Cilët përbërës të instalohen?
SelectComponentsLabel2=Zgjidhi përbërësit që do të instalosh; mos i shëno përbërësit që nuk do të instalosh. Kliko Tjetër kur të jesh gati për të vijuar.
FullInstallation=Instalim i Plotë
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalim i Pakët
CustomInstallation=Instalim i Porositur
NoUninstallWarningTitle=Përbërës Ekzistues
NoUninstallWarning=Sistemimi diktoi se këta përbërës janë tashmë të instaluar në kompjuter:%n%n%1%n%nMoszgjedhja e këtyre përbërësve nuk do i çinstalojë ata.%n%nGjithsesi, do vijosh?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Kjo zgjedhje kërkon jo më pak se [mb] MB hapësirë të lirë në disk.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Zgjedh Detyrat Shtesë
SelectTasksDesc=Cilat detyra shtesë të kryhen?
SelectTasksLabel2=Zgjidhi detyrat shtesë që duhet të kryejë Sistemimi gjatë instalimit të [name], pastaj kliko Tjetër.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Zgjedh Dosjen në Menynë Nis
SelectStartMenuFolderDesc=Ku duhet t'i vendosë Sistemimi shkurtoret e programit?
SelectStartMenuFolderLabel3=Sistemimi do i krijojë shkurtoret e programit në këtë dosje të Menysë Nis.
SelectStartMenuFolderBrowseLabel=Për të vijuar, kliko Tjetër. Nëse do të zgjedhësh një dosje ndryshe, kliko Shfleto.
MustEnterGroupName=Duhet shkruar emri i dosjes.
GroupNameTooLong=Emri ose shtegu i dosjes është tepër i gjatë.
InvalidGroupName=Emri i dosjes nuk ka vlerë.
BadGroupName=Emri i dosjes nuk duhet të përmbajë asnjë nga këto shkronja:%n%n%1
NoProgramGroupCheck2=&Mos krijo dosje në Menynë Nis

; *** "Ready to Install" wizard page
WizardReady=Gati të Instalohet
ReadyLabel1=Sistemimi është gati të nisë instalimin e [name] në kompjuter.
ReadyLabel2a=Kliko Instalo për të vijuar me instalimin, ose kliko Pas për të rishikuar apo ndryshuar ndonjë vendosje.
ReadyLabel2b=Kliko Instalo për të vijuar me instalimin.
ReadyMemoUserInfo=Informacioni i përdoruesit:
ReadyMemoDir=Destinacioni:
ReadyMemoType=Lloji i sistemimit:
ReadyMemoComponents=Përbërësit e zgjedhur:
ReadyMemoGroup=Dosja në Menynë Nis:
ReadyMemoTasks=Detyra shtesë:

; *** "Preparing to Install" wizard page
WizardPreparing=Gati të Instalohet
PreparingDesc=Sistemimi është gati të instalojë [name] në kompjuter.
PreviousInstallNotCompleted=Nuk plotësohet instalimi/heqja e programit të mëparshëm. Kompjuteri duhet të rinisë që të plotësohet instalimi.%n%nPasi kompjuteri të riniset, vepro përsëri Sistemimin që të plotësohet instalimi i [name].
CannotContinue=Sistemimi nuk mund të vijojë. Të lutem kliko Anulo për të dalë.

; *** "Installing" wizard page
WizardInstalling=Instalim
InstallingLabel=Të lutem prit që Sistemimi të instalojë [name] në kompjuter.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Plotëson Udhërrëfyesin për Sistemimin e [name]
FinishedLabelNoIcons=Sistemimi përfundon instalimin në kompjuter të [name].
FinishedLabel=Sistemimi përfundon instalimin në kompjuter të [name]. Aplikimi mund të lëshohet duke zgjedhur ikonën e instaluar.
ClickFinish=Kliko Përfundo për të dalë nga Sistemimi.
FinishedRestartLabel=Që të përfundojë instalimi i [name], Sistemimi duhet të rinisë kompjuterin. Të rinisë tani?
FinishedRestartMessage=Që të përfundojë instalimi i [name], Sistemimi duhet të rinisë kompjuterin.%n%nTë rinisë tani?
ShowReadmeCheck=Po, dua të shikoj skedën README
YesRadio=&Po, rinis kompjuterin tani
NoRadio=&Jo, do rinis kompjuterin më vonë
; used for example as 'Run MyProg.exe'
RunEntryExec=Lësho %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Shiko %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Sistemimi Kërkon Diskun Tjetër
SelectDiskLabel2=Të lutem vendos Diskun %1 dhe kliko OK.%n%nNëse skedat në këtë disk mund të ndodhen në një dosje ndryshe nga ajo e shfaqur këtu, vendos shtegun e saktë ose kliko Shfleto.
PathLabel=&Shtegu:
FileNotInDir2=Skeda "%1" nuk ndodhet në "%2". Të lutem vendos diskun e saktë ose zgjidhe një dosje tjetër.
SelectDirectoryLabel=Të lutem përcakto vendin e diskut tjetër.

; *** Installation phase messages
SetupAborted=Nuk plotësohet sistemimi.%n%nTë lutem korrigjo problemin dhe lëshoje atë përsëri.
EntryAbortRetryIgnore=Kliko Riprovo për të provuar përsëri, Përbuz për të vijuar gjithsesi, ose Ndërprit që instalimi të anulohet.

; *** Installation status messages
StatusCreateDirs=Krijon direktoritë...
StatusExtractFiles=Nxjerr skedat...
StatusCreateIcons=Krijon shkurtoret...
StatusCreateIniEntries=Krijon hyrjet INI...
StatusCreateRegistryEntries=Krijon hyrjet në regjistër...
StatusRegisterFiles=Regjistron skedat...
StatusSavingUninstall=Ruan informacionin e çinstalimit...
StatusRunProgram=Përfundon instalimin...
StatusRollback=Kthen ndryshimin...

; *** Misc. errors
ErrorInternal2=Gabim i brendshëm: %1
ErrorFunctionFailedNoCode=%1 ndalon
ErrorFunctionFailed=%1 ndalon; kodi %2
ErrorFunctionFailedWithMessage=%1 ndalon; kodi %2.%n%3
ErrorExecutingProgram=Kjo skedë nuk ekzekutohet:%n%1

; *** Registry errors
ErrorRegOpenKey=Gabim në hapjen e kodit të regjistrit:%n%1\%2
ErrorRegCreateKey=Gabim në krijimin e kodit të regjistrit:%n%1\%2
ErrorRegWriteKey=Gabim në shkrimin e kodit të regjistrit:%n%1\%2

; *** INI errors
ErrorIniEntry=Gabim në krijimin e hyrjes INI te skeda "%1".

; *** File copying errors
FileAbortRetryIgnore=Kliko Riprovo për të provuar përsëri, Përbuz për ta kaluar këtë skedë (nuk rekomandohet), ose Ndërprit që instalimi të anulohet.
FileAbortRetryIgnore2=Kliko Riprovo për të provuar përsëri, Përbuz për të vijuar gjithsesi (nuk rekomandohet), ose Ndërprit që instalimi të anulohet.
SourceIsCorrupted=Është prishur skeda e burimit
SourceDoesntExist=Nuk ekziston skeda e burimit "%1"
ExistingFileReadOnly=Skeda ekzistuese është shënuar vetëm për lexim.%n%nKliko Riprovo për të hequr atributin e leximit dhe provoje përsëri, Përbuz për ta kaluar këtë skedë, ose Ndërprit që çinstalimi të anulohet.
ErrorReadingExistingDest=Ndodhi një gabim gjatë provës për të lexuar skedën ekzistuese:
FileExists=Skeda ekziston që më parë.%n%nTa mbishkruajë Sistemimi atë?
ExistingFileNewer=Skeda ekzistuese është me e re sesa ajo që Sistemimi provon të instalojë. Rekomandohet që të mbash skedën ekzistuese.%n%nDo mbash skedën ekzistuese?
ErrorChangingAttr=Ndodhi një gabim gjatë provës për të ndryshuar atributet e skedës ekzistuese:
ErrorCreatingTemp=Ndodhi një gabim gjatë provës për të krijuar një skedë në direktorinë e destinacionit:
ErrorReadingSource=Ndodhi një gabim gjatë provës për të lexuar skedën burim:
ErrorCopying=Ndodhi një gabim gjatë orvatjes për të kopjuar një skedë:
ErrorReplacingExistingFile=Ndodhi një gabim gjatë orvatjes për të zëvendësuar skedën ekzistuese:
ErrorRestartReplace=Ndalon RestartReplace:
ErrorRenamingTemp=Ndodhi një gabim gjatë riemërtimit të skedës në direktorinë e destinacionit:
ErrorRegisterServer=Nuk mund të regjistrohet DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 ndalon me kodin e daljes %1
ErrorRegisterTypeLib=Nuk mund të regjistrohet lloji i librarisë: %1

; *** Post-installation errors
ErrorOpeningReadme=Ndodhi një gabim gjatë hapjes së skedës README.
ErrorRestartingComputer=Sistemimi nuk arriti të rinisë kompjuterin. Të lutem bëje vetë.

; *** Uninstaller messages
UninstallNotFound=Skeda "%1" nuk ekziston. Nuk mund të çinstalohet.
UninstallOpenError=Skeda "%1" nuk mund të hapet. Nuk mund të çinstalohet.
UninstallUnsupportedVer=Ditari i çinstalimit "%1" ka një format që nuk njihet nga ky version i çinstaluesit. Nuk mund të çinstalohet
UninstallUnknownEntry=Te ditari i çinstalimit vërehet një hyrje e panjohur (%1)
ConfirmUninstall=Je i sigurt për fshirjen e plotë të %1 dhe tërë përbërësit e vet?
UninstallOnlyOnWin64=Ky instalim mund të çinstalohet vetëm në Windows 64-bit.
OnlyAdminCanUninstall=Ky instalim mund të çinstalohet vetëm nga një përdorues me privilegje administrimi.
UninstallStatusLabel=Të lutem prit ndërkohë që %1 fshihet nga kompjuteri.
UninstalledAll=%1 u fshi me sukses nga kompjuteri.
UninstalledMost=Përfundon çinstalimi i %1.%n%nDisa elemente nuk mund të fshiheshin. Këto mund t'i fshish vetë.
UninstalledAndNeedsRestart=Që të plotësohet çinstalimi i %1, kompjuteri duhet të riniset.%n%nTë rinisë tani?
UninstallDataCorrupted="%1" është skedë e prishur. Nuk mund të çinstalohet.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Fshin Skedën e Ndarë?
ConfirmDeleteSharedFile2=Sistemimi tregon se kjo skedë e ndarë nuk përdoret më nga asnjë program. Ta fshijë Çinstalimi këtë skedë të ndarë?%n%nNëse ndonjë program e përdor akoma skedën, kur ajo të fshihet programi mund të mos punojë siç duhet. Nëse nuk je i sigurt, zgjidhe Jo. Lënia e skedës në sistem nuk do shkaktojë dëme.
SharedFileNameLabel=Emri i skedës:
SharedFileLocationLabel=Vendi:
WizardUninstalling=Statusi i Çinstalimit
StatusUninstalling=Çinstalon %1...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versioni %2
AdditionalIcons=Ikona shtesë:
CreateDesktopIcon=Krijo një ikonë në &tryezë
CreateQuickLaunchIcon=Krijo një ikonë në &Quick Launch
ProgramOnTheWeb=%1 në Rrjetë
UninstallProgram=Çinstalo %1
LaunchProgram=Lësho %1
AssocFileExtension=&Shoqëro %1 me siglën e skedës %2
AssocingFileExtension=%1 shoqërohet me siglën e skedës %2...
