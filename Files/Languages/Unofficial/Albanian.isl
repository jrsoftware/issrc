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
; P�rkthyer nga Besmir Godole
; Posta-e: bgodole@gmail.com
; M� kontaktoni me post�-e p�r ndonj� gabim ose sugjerim rreth p�rkthimit.

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
UninstallAppTitle=�instalim
UninstallAppFullTitle=�instalon %1

; *** Misc. common
InformationTitle=Informacion
ConfirmTitle=Miratim
ErrorTitle=Gabim

; *** SetupLdr messages
SetupLdrStartupMessage=Tani do instalosh %1. Do vijosh?
LdrCannotCreateTemp=Nuk mund t� krijohet nj� sked� koh�shkurt�r. Nd�rpritet sistemimi
LdrCannotExecTemp=Nuk mund t� ekzekutohet skeda n� direktorin� koh�shkurt�r. Nd�rpritet sistemimi

; *** Startup error messages
LastErrorMessage=%1.%n%nGabim %2: %3
SetupFileMissing=Skeda %1 mungon n� direktorin� e instalimit. T� lutem korrigjo problemin ose gjej nj� kopje t� re t� programit.
SetupFileCorrupt=Skedat e sistemimit jan� prishur. T� lutem gjej nj� kopje t� re t� programit.
SetupFileCorruptOrWrongVer=Skedat e sistemimit jan� prishur ose nuk pajtohen me k�t� version t� Sistemimit. T� lutem korrigjo problemin ose gjej nj� kopje t� re t� programit.
NotOnThisPlatform=Ky program nuk do veproj� n� %1.
OnlyOnThisPlatform=Ky program duhet t� veproj� n� %1.
OnlyOnTheseArchitectures=Ky program mund t� instalohet vet�m n� versionet e Windows-it q� jan� modeluar p�r k�to modele arkitekturore t� procesorit:%n%n%1
MissingWOW64APIs=Versioni i Windows-it q� ke nuk p�rmban funksionet q� k�rkon Sistemimi p�r t� kryer nj� instalim 64-bit. P�r ta korrigjuar k�t� problem, t� lutem instalo Paket�n e Sh�rbimit %1.
WinVersionTooLowError=K�tij programi i nevojitet %1 me version %2 a m� von�.
WinVersionTooHighError=Ky program nuk mund t� instalohet n� %1 me versionin %2 a m� von�.
AdminPrivilegesRequired=Kur e instalon k�t� program duhet t� hysh si administrator.
PowerUserPrivilegesRequired=Kur e instalon k�t� program duhet t� hysh si administrator ose an�tar i grupit P�rdorues me Fuqi.
SetupAppRunningError=Sistemimi diktoi se aktualisht po vepron %1.%n%nT� lutem mbylli tani t�r� rastet e tij dhe kliko OK p�r t� vijuar, ose Anulo p�r t� dal�.
UninstallAppRunningError=�instalimi diktoi se aktualisht po vepron %1.%n%nT� lutem mbylli tani t�r� rastet e tij dhe kliko OK p�r t� vijuar, ose Anulo p�r t� dal�.

; *** Misc. errors
ErrorCreatingDir=Sistemimi nuk arrin t� krijoj� direktorin� "%1"
ErrorTooManyFilesInDir=N� direktorin� "%1" nuk mund t� krijohen skeda sepse ka shum� t� tjera

; *** Setup common messages
ExitSetupTitle=Dalje nga Sistemimi
ExitSetupMessage=Sistemimi nuk �sht� plot�suar. Po e mbylle, programi nuk instalohet.%n%nQ� instalimi t� plot�sohet, mund ta l�shosh Sistemimin nj� her� tjet�r.%n%nT� Mbyllet Sistemimi?
AboutSetupMenuItem=&P�r Sistemimin...
AboutSetupTitle=P�r Sistemimin
AboutSetupMessage=%1 versioni %2%n%3%n%n%1 faqe zyrtare:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Pas
ButtonNext=&Tjet�r >
ButtonInstall=&Instalo
ButtonOK=OK
ButtonCancel=Anulo
ButtonYes=&Po
ButtonYesToAll=Po, &Gjith�ka
ButtonNo=&Jo
ButtonNoToAll=J&o, Asnj�
ButtonFinish=&P�rfundo
ButtonBrowse=&Shfleto...
ButtonWizardBrowse=S&hfleto...
ButtonNewFolder=&Krijo Dosje t� Re

; *** "Select Language" dialog messages
SelectLanguageTitle=Zgjedh Gjuh�n e Sistemimit
SelectLanguageLabel=Zgjidhe gjuh�n e p�rdorimit gjat� instalimit:

; *** Common wizard text
ClickNext=Kliko Tjet�r p�r t� vijuar, ose Anulo q� Sistemimi t� mbyllet.
BeveledLabel=
BrowseDialogTitle=Shfleton dosjen
BrowseDialogLabel=Zgjidhe nj� dosje n� k�t� list�, pastaj kliko OK.
NewFolderName=Dosje e re

; *** "Welcome" wizard page
WelcomeLabel1=Mir� se erdhe te Udh�rr�fyesi p�r Sistemimin e [name]
WelcomeLabel2=[name/ver] do instalohet tani n� kompjuter.%n%nPara se t� vijosh, rekomandohet t� mbyllen aplikimet e tjera.

; *** "Password" wizard page
WizardPassword=Fjal�kalimi
PasswordLabel1=Ky instalim �sht� i mbrojtur me fjal�kalim.
PasswordLabel3=T� lutem shkruaj fjal�kalimin, pastaj kliko Tjet�r q� t� vijosh. Fjal�kalimi duhet v�n� me kujdes.
PasswordEditLabel=&Fjal�kalimi:
IncorrectPassword=Fjal�kalimi i shkruar nuk �sht� i sakt�. T� lutem provoje p�rs�ri.

; *** "License Agreement" wizard page
WizardLicense=Marr�veshja e Licenc�s
LicenseLabel=T� lutem lexo informacionin e r�nd�sish�m m� posht� para se t� vijosh.
LicenseLabel3=T� lutem lexo Marr�veshjen e Licenc�s m� posht�. Para se t� vijosh me instalimin, duhet t� pranosh detyrimet e k�saj marr�veshjeje.
LicenseAccepted=&Pranoj marr�veshjen
LicenseNotAccepted=&Nuk e pranoj marr�veshjen

; *** "Information" wizard pages
WizardInfoBefore=Informacion
InfoBeforeLabel=T� lutem lexo informacionin e r�nd�sish�m m� posht� para se t� vijosh.
InfoBeforeClickLabel=Kur t� jesh gati p�r t� vijuar me Sistemimin, kliko Tjet�r.
WizardInfoAfter=Informacion
InfoAfterLabel=T� lutem lexo informacionin e r�nd�sish�m m� posht� para se t� vijosh.
InfoAfterClickLabel=Kur t� jesh gati p�r t� vijuar me Sistemimin, kliko Tjet�r.

; *** "User Information" wizard page
WizardUserInfo=Informacion i P�rdoruesit
UserInfoDesc=T� lutem vendos informacionin t�nd.
UserInfoName=&P�rdoruesi:
UserInfoOrg=&Organizata:
UserInfoSerial=&Numri i Seris�:
UserInfoNameRequired=Duhet shkruar nj� em�r.

; *** "Select Destination Location" wizard page
WizardSelectDir=Zgjedh Destinacionin
SelectDirDesc=Ku duhet t� instalohet [name]?
SelectDirLabel3=Sistemimi do e instaloj� [name] n� k�t� dosje.
SelectDirBrowseLabel=P�r t� vijuar, kliko Tjet�r. N�se do t� zgjedh�sh nj� dosje ndryshe, kliko Shfleto.
DiskSpaceMBLabel=K�rkon jo m� pak se [mb] MB hap�sir� t� lir� n� disk.
ToUNCPathname=Sistemimi nuk mund t� instaloj� n� nj� shteg UNC. N�se po provon instalimin n� rrjet, duhet t� lokalizosh drajvin e rrjetit.
InvalidPath=Duhet t� shkruhet shtegu i plot� me shkronj�n e drajvit; p�r shembull:%n%nC:\APP%n%nose shtegu UNC n� form�n:%n%n\\server\share
InvalidDrive=Drajvi ose shp�rndar�si UNC i zgjedhur nuk ekziston ose nuk hapet. T� lutem zgjidhe nj� tjet�r.
DiskSpaceWarningTitle=Hap�sir� e Pamjaftueshme
DiskSpaceWarning=Sistemimi k�rkon t� pakt�n %1 KB hap�sir� t� lir� p�r t� instaluar, por ky drajv ka vet�m %2 KB me vler�.%n%nGjithsesi, do vijosh?
DirNameTooLong=Emri ose shtegu i dosjes �sht� tep�r i gjat�.
InvalidDirName=Emri i dosjes nuk ka vler�.
BadDirName32=Emri i dosjes nuk mund t� p�rmbaj� k�to shkronja:%n%n%1
DirExistsTitle=Dosje Ekzistuese
DirExists=Dosja:%n%n%1%n%nekziston q� m� par�. Gjithsesi, t� instalohet n� k�t� dosje?
DirDoesntExistTitle=Dosje Joekzistuese
DirDoesntExist=Dosja:%n%n%1%n%nnuk ekziston. T� krijohet kjo dosje?

; *** "Select Components" wizard page
WizardSelectComponents=Zgjedh P�rb�r�sit
SelectComponentsDesc=Cil�t p�rb�r�s t� instalohen?
SelectComponentsLabel2=Zgjidhi p�rb�r�sit q� do t� instalosh; mos i sh�no p�rb�r�sit q� nuk do t� instalosh. Kliko Tjet�r kur t� jesh gati p�r t� vijuar.
FullInstallation=Instalim i Plot�
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalim i Pak�t
CustomInstallation=Instalim i Porositur
NoUninstallWarningTitle=P�rb�r�s Ekzistues
NoUninstallWarning=Sistemimi diktoi se k�ta p�rb�r�s jan� tashm� t� instaluar n� kompjuter:%n%n%1%n%nMoszgjedhja e k�tyre p�rb�r�sve nuk do i �instaloj� ata.%n%nGjithsesi, do vijosh?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Kjo zgjedhje k�rkon jo m� pak se [mb] MB hap�sir� t� lir� n� disk.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Zgjedh Detyrat Shtes�
SelectTasksDesc=Cilat detyra shtes� t� kryhen?
SelectTasksLabel2=Zgjidhi detyrat shtes� q� duhet t� kryej� Sistemimi gjat� instalimit t� [name], pastaj kliko Tjet�r.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Zgjedh Dosjen n� Menyn� Nis
SelectStartMenuFolderDesc=Ku duhet t'i vendos� Sistemimi shkurtoret e programit?
SelectStartMenuFolderLabel3=Sistemimi do i krijoj� shkurtoret e programit n� k�t� dosje t� Menys� Nis.
SelectStartMenuFolderBrowseLabel=P�r t� vijuar, kliko Tjet�r. N�se do t� zgjedh�sh nj� dosje ndryshe, kliko Shfleto.
MustEnterGroupName=Duhet shkruar emri i dosjes.
GroupNameTooLong=Emri ose shtegu i dosjes �sht� tep�r i gjat�.
InvalidGroupName=Emri i dosjes nuk ka vler�.
BadGroupName=Emri i dosjes nuk duhet t� p�rmbaj� asnj� nga k�to shkronja:%n%n%1
NoProgramGroupCheck2=&Mos krijo dosje n� Menyn� Nis

; *** "Ready to Install" wizard page
WizardReady=Gati t� Instalohet
ReadyLabel1=Sistemimi �sht� gati t� nis� instalimin e [name] n� kompjuter.
ReadyLabel2a=Kliko Instalo p�r t� vijuar me instalimin, ose kliko Pas p�r t� rishikuar apo ndryshuar ndonj� vendosje.
ReadyLabel2b=Kliko Instalo p�r t� vijuar me instalimin.
ReadyMemoUserInfo=Informacioni i p�rdoruesit:
ReadyMemoDir=Destinacioni:
ReadyMemoType=Lloji i sistemimit:
ReadyMemoComponents=P�rb�r�sit e zgjedhur:
ReadyMemoGroup=Dosja n� Menyn� Nis:
ReadyMemoTasks=Detyra shtes�:

; *** "Preparing to Install" wizard page
WizardPreparing=Gati t� Instalohet
PreparingDesc=Sistemimi �sht� gati t� instaloj� [name] n� kompjuter.
PreviousInstallNotCompleted=Nuk plot�sohet instalimi/heqja e programit t� m�parsh�m. Kompjuteri duhet t� rinis� q� t� plot�sohet instalimi.%n%nPasi kompjuteri t� riniset, vepro p�rs�ri Sistemimin q� t� plot�sohet instalimi i [name].
CannotContinue=Sistemimi nuk mund t� vijoj�. T� lutem kliko Anulo p�r t� dal�.

; *** "Installing" wizard page
WizardInstalling=Instalim
InstallingLabel=T� lutem prit q� Sistemimi t� instaloj� [name] n� kompjuter.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Plot�son Udh�rr�fyesin p�r Sistemimin e [name]
FinishedLabelNoIcons=Sistemimi p�rfundon instalimin n� kompjuter t� [name].
FinishedLabel=Sistemimi p�rfundon instalimin n� kompjuter t� [name]. Aplikimi mund t� l�shohet duke zgjedhur ikon�n e instaluar.
ClickFinish=Kliko P�rfundo p�r t� dal� nga Sistemimi.
FinishedRestartLabel=Q� t� p�rfundoj� instalimi i [name], Sistemimi duhet t� rinis� kompjuterin. T� rinis� tani?
FinishedRestartMessage=Q� t� p�rfundoj� instalimi i [name], Sistemimi duhet t� rinis� kompjuterin.%n%nT� rinis� tani?
ShowReadmeCheck=Po, dua t� shikoj sked�n README
YesRadio=&Po, rinis kompjuterin tani
NoRadio=&Jo, do rinis kompjuterin m� von�
; used for example as 'Run MyProg.exe'
RunEntryExec=L�sho %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Shiko %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Sistemimi K�rkon Diskun Tjet�r
SelectDiskLabel2=T� lutem vendos Diskun %1 dhe kliko OK.%n%nN�se skedat n� k�t� disk mund t� ndodhen n� nj� dosje ndryshe nga ajo e shfaqur k�tu, vendos shtegun e sakt� ose kliko Shfleto.
PathLabel=&Shtegu:
FileNotInDir2=Skeda "%1" nuk ndodhet n� "%2". T� lutem vendos diskun e sakt� ose zgjidhe nj� dosje tjet�r.
SelectDirectoryLabel=T� lutem p�rcakto vendin e diskut tjet�r.

; *** Installation phase messages
SetupAborted=Nuk plot�sohet sistemimi.%n%nT� lutem korrigjo problemin dhe l�shoje at� p�rs�ri.
EntryAbortRetryIgnore=Kliko Riprovo p�r t� provuar p�rs�ri, P�rbuz p�r t� vijuar gjithsesi, ose Nd�rprit q� instalimi t� anulohet.

; *** Installation status messages
StatusCreateDirs=Krijon direktorit�...
StatusExtractFiles=Nxjerr skedat...
StatusCreateIcons=Krijon shkurtoret...
StatusCreateIniEntries=Krijon hyrjet INI...
StatusCreateRegistryEntries=Krijon hyrjet n� regjist�r...
StatusRegisterFiles=Regjistron skedat...
StatusSavingUninstall=Ruan informacionin e �instalimit...
StatusRunProgram=P�rfundon instalimin...
StatusRollback=Kthen ndryshimin...

; *** Misc. errors
ErrorInternal2=Gabim i brendsh�m: %1
ErrorFunctionFailedNoCode=%1 ndalon
ErrorFunctionFailed=%1 ndalon; kodi %2
ErrorFunctionFailedWithMessage=%1 ndalon; kodi %2.%n%3
ErrorExecutingProgram=Kjo sked� nuk ekzekutohet:%n%1

; *** Registry errors
ErrorRegOpenKey=Gabim n� hapjen e kodit t� regjistrit:%n%1\%2
ErrorRegCreateKey=Gabim n� krijimin e kodit t� regjistrit:%n%1\%2
ErrorRegWriteKey=Gabim n� shkrimin e kodit t� regjistrit:%n%1\%2

; *** INI errors
ErrorIniEntry=Gabim n� krijimin e hyrjes INI te skeda "%1".

; *** File copying errors
FileAbortRetryIgnore=Kliko Riprovo p�r t� provuar p�rs�ri, P�rbuz p�r ta kaluar k�t� sked� (nuk rekomandohet), ose Nd�rprit q� instalimi t� anulohet.
FileAbortRetryIgnore2=Kliko Riprovo p�r t� provuar p�rs�ri, P�rbuz p�r t� vijuar gjithsesi (nuk rekomandohet), ose Nd�rprit q� instalimi t� anulohet.
SourceIsCorrupted=�sht� prishur skeda e burimit
SourceDoesntExist=Nuk ekziston skeda e burimit "%1"
ExistingFileReadOnly=Skeda ekzistuese �sht� sh�nuar vet�m p�r lexim.%n%nKliko Riprovo p�r t� hequr atributin e leximit dhe provoje p�rs�ri, P�rbuz p�r ta kaluar k�t� sked�, ose Nd�rprit q� �instalimi t� anulohet.
ErrorReadingExistingDest=Ndodhi nj� gabim gjat� prov�s p�r t� lexuar sked�n ekzistuese:
FileExists=Skeda ekziston q� m� par�.%n%nTa mbishkruaj� Sistemimi at�?
ExistingFileNewer=Skeda ekzistuese �sht� me e re sesa ajo q� Sistemimi provon t� instaloj�. Rekomandohet q� t� mbash sked�n ekzistuese.%n%nDo mbash sked�n ekzistuese?
ErrorChangingAttr=Ndodhi nj� gabim gjat� prov�s p�r t� ndryshuar atributet e sked�s ekzistuese:
ErrorCreatingTemp=Ndodhi nj� gabim gjat� prov�s p�r t� krijuar nj� sked� n� direktorin� e destinacionit:
ErrorReadingSource=Ndodhi nj� gabim gjat� prov�s p�r t� lexuar sked�n burim:
ErrorCopying=Ndodhi nj� gabim gjat� orvatjes p�r t� kopjuar nj� sked�:
ErrorReplacingExistingFile=Ndodhi nj� gabim gjat� orvatjes p�r t� z�vend�suar sked�n ekzistuese:
ErrorRestartReplace=Ndalon RestartReplace:
ErrorRenamingTemp=Ndodhi nj� gabim gjat� riem�rtimit t� sked�s n� direktorin� e destinacionit:
ErrorRegisterServer=Nuk mund t� regjistrohet DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 ndalon me kodin e daljes %1
ErrorRegisterTypeLib=Nuk mund t� regjistrohet lloji i libraris�: %1

; *** Post-installation errors
ErrorOpeningReadme=Ndodhi nj� gabim gjat� hapjes s� sked�s README.
ErrorRestartingComputer=Sistemimi nuk arriti t� rinis� kompjuterin. T� lutem b�je vet�.

; *** Uninstaller messages
UninstallNotFound=Skeda "%1" nuk ekziston. Nuk mund t� �instalohet.
UninstallOpenError=Skeda "%1" nuk mund t� hapet. Nuk mund t� �instalohet.
UninstallUnsupportedVer=Ditari i �instalimit "%1" ka nj� format q� nuk njihet nga ky version i �instaluesit. Nuk mund t� �instalohet
UninstallUnknownEntry=Te ditari i �instalimit v�rehet nj� hyrje e panjohur (%1)
ConfirmUninstall=Je i sigurt p�r fshirjen e plot� t� %1 dhe t�r� p�rb�r�sit e vet?
UninstallOnlyOnWin64=Ky instalim mund t� �instalohet vet�m n� Windows 64-bit.
OnlyAdminCanUninstall=Ky instalim mund t� �instalohet vet�m nga nj� p�rdorues me privilegje administrimi.
UninstallStatusLabel=T� lutem prit nd�rkoh� q� %1 fshihet nga kompjuteri.
UninstalledAll=%1 u fshi me sukses nga kompjuteri.
UninstalledMost=P�rfundon �instalimi i %1.%n%nDisa elemente nuk mund t� fshiheshin. K�to mund t'i fshish vet�.
UninstalledAndNeedsRestart=Q� t� plot�sohet �instalimi i %1, kompjuteri duhet t� riniset.%n%nT� rinis� tani?
UninstallDataCorrupted="%1" �sht� sked� e prishur. Nuk mund t� �instalohet.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Fshin Sked�n e Ndar�?
ConfirmDeleteSharedFile2=Sistemimi tregon se kjo sked� e ndar� nuk p�rdoret m� nga asnj� program. Ta fshij� �instalimi k�t� sked� t� ndar�?%n%nN�se ndonj� program e p�rdor akoma sked�n, kur ajo t� fshihet programi mund t� mos punoj� si� duhet. N�se nuk je i sigurt, zgjidhe Jo. L�nia e sked�s n� sistem nuk do shkaktoj� d�me.
SharedFileNameLabel=Emri i sked�s:
SharedFileLocationLabel=Vendi:
WizardUninstalling=Statusi i �instalimit
StatusUninstalling=�instalon %1...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versioni %2
AdditionalIcons=Ikona shtes�:
CreateDesktopIcon=Krijo nj� ikon� n� &tryez�
CreateQuickLaunchIcon=Krijo nj� ikon� n� &Quick Launch
ProgramOnTheWeb=%1 n� Rrjet�
UninstallProgram=�instalo %1
LaunchProgram=L�sho %1
AssocFileExtension=&Shoq�ro %1 me sigl�n e sked�s %2
AssocingFileExtension=%1 shoq�rohet me sigl�n e sked�s %2...
