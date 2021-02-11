; ******************************************************
; ***                                                ***
; *** Inno Setup version 6.1.0+ Slovak messages      ***
; ***                                                ***
; *** Original Author:                               ***
; ***                                                ***
; ***  Milan Potancok (milan.potancok AT gmail.com)  ***
; ***                                                ***
; *** Contributors:                                  ***
; ***                                                ***
; ***   Ivo Bauer (bauer AT ozm.cz)                  ***
; ***                                                ***
; ***   Tomas Falb (tomasf AT pobox.sk)              ***
; ***   Slappy (slappy AT pobox.sk)                  ***
; ***   Comments: (mitems58 AT gmail.com)            ***
; ***                                                ***
; *** Update: 28.01.2021                             ***
; ***                                                ***
; ******************************************************
;
; 

[LangOptions]
LanguageName=Sloven<010D>ina
LanguageID=$041b
LanguageCodePage=1250

[Messages]

; *** Application titles
SetupAppTitle=Sprievodca in�tal�ciou
SetupWindowTitle=Sprievodca in�tal�ciou - %1
UninstallAppTitle=Sprievodca odin�tal�ciou
UninstallAppFullTitle=Sprievodca odin�tal�ciou - %1

; *** Misc. common
InformationTitle=Inform�cie
ConfirmTitle=Potvrdenie
ErrorTitle=Chyba

; *** SetupLdr messages
SetupLdrStartupMessage=V�ta V�s Sprievodca in�tal�ciou produktu %1. Prajete si pokra�ova�?
LdrCannotCreateTemp=Nie je mo�n� vytvori� do�asn� s�bor. Sprievodca in�tal�ciou sa ukon��
LdrCannotExecTemp=Nie je mo�n� spusti� s�bor v do�asnom adres�ri. Sprievodca in�tal�ciou sa ukon��
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nChyba %2: %3
SetupFileMissing=In�tala�n� adres�r neobsahuje s�bor %1. Opravte t�to chybu, alebo si zaobstarajte nov� k�piu tohto produktu.
SetupFileCorrupt=S�bory Sprievodcu in�tal�ciou s� po�koden�. Zaobstarajte si nov� k�piu tohto produktu.
SetupFileCorruptOrWrongVer=S�bory Sprievodcu in�tal�ciou s� po�koden� alebo sa nezhoduj� s touto verziou Sprievodcu in�tal�ciou. Opravte t�to chybu, alebo si zaobstarajte nov� k�piu tohto produktu.
InvalidParameter=Nespr�vny parameter na pr�kazovom riadku: %n%n%1
SetupAlreadyRunning=In�tal�cia u� prebieha.
WindowsVersionNotSupported=Tento program nepodporuje va�u verziu syst�mu Windows.
WindowsServicePackRequired=Tento program vy�aduje %1 Service Pack %2 alebo nov��.
NotOnThisPlatform=Tento produkt sa ned� spusti� v %1.
OnlyOnThisPlatform=Tento produkt mus� by� spusten� v %1.
OnlyOnTheseArchitectures=Tento produkt je mo�n� nain�talova� iba vo verzi�ch MS Windows s podporou architekt�ry procesorov:%n%n%1
WinVersionTooLowError=Tento produkt vy�aduje %1 verzie %2 alebo vy��ej.
WinVersionTooHighError=Tento produkt sa ned� nain�talova� vo %1 verzie %2 alebo vy��ej.
AdminPrivilegesRequired=Na in�tal�ciu tohto produktu mus�te by� prihl�sen� s pr�vami administr�tora.
PowerUserPrivilegesRequired=Na in�tal�ciu tohto produktu mus�te by� prihl�sen� s pr�vami Administr�tora alebo �lena skupiny Power Users.
SetupAppRunningError=Sprievodca in�tal�ciou zistil, �e produkt %1 je teraz spusten�.%n%nUkon�te v�etky spusten� in�tancie tohto produktu a pokra�ujte kliknut�m na tla�idlo "OK", alebo ukon�te in�tal�ciu tla�idlom "Zru�i�".
UninstallAppRunningError=Sprievodca odin�tal�ciou zistil, �e produkt %1 je teraz spusten�.%n%nUkon�te v�etky spusten� in�tancie tohto produktu a pokra�ujte kliknut�m na tla�idlo "OK", alebo ukon�te in�tal�ciu tla�idlom "Zru�i�".

; *** Startup questions
PrivilegesRequiredOverrideTitle=Vyberte in�tala�n� m�d in�tal�tora
PrivilegesRequiredOverrideInstruction=Vyberte in�tala�n� m�d
PrivilegesRequiredOverrideText1=%1 sa m��e nain�talova� pre v�etk�ch u��vate�ov (vy�aduje administr�torsk� pr�va), alebo len pre V�s.
PrivilegesRequiredOverrideText2=%1 sa m��e nain�talova� len pre V�s, alebo pre v�etk�ch u��vate�ov (vy�aduj� sa Administr�torsk� pr�va).
PrivilegesRequiredOverrideAllUsers=In�talova� pre &v�etk�ch u��vate�ov
PrivilegesRequiredOverrideAllUsersRecommended=In�talova� pre &v�etk�ch u��vate�ov (odpor��an�)
PrivilegesRequiredOverrideCurrentUser=In�talova� len pre &m�a
PrivilegesRequiredOverrideCurrentUserRecommended=In�talova� len pre &m�a (odpor��an�)

; *** Misc. errors
ErrorCreatingDir=Sprievodca in�tal�ciou nemohol vytvori� adres�r "%1"
ErrorTooManyFilesInDir=Ned� sa vytvori� s�bor v adres�ri "%1", preto�e tento adres�r u� obsahuje pr�li� ve�a s�borov

; *** Setup common messages
ExitSetupTitle=Ukon�i� Sprievodcu in�tal�ciou
ExitSetupMessage=In�tal�cia nebola kompletne dokon�en�. Ak teraz ukon��te Sprievodcu in�tal�ciou, produkt nebude nain�talovan�.%n%nSprievodcu in�tal�ciou m��ete znovu spusti� nesk�r a dokon�i� tak in�tal�ciu.%n%nUkon�i� Sprievodcu in�tal�ciou?
AboutSetupMenuItem=&O Sprievodcovi in�tal�cie...
AboutSetupTitle=O Sprievodcovi in�tal�cie
AboutSetupMessage=%1 verzia %2%n%3%n%n%1 domovsk� str�nka:%n%4
AboutSetupNote=
TranslatorNote=Slovak translation maintained by Milan Potancok (milan.potancok AT gmail.com), Ivo Bauer (bauer AT ozm.cz), Tomas Falb (tomasf AT pobox.sk) + Slappy (slappy AT pobox.sk)

; *** Buttons
ButtonBack=< &Sp�
ButtonNext=&�alej >
ButtonInstall=&In�talova�
ButtonOK=OK
ButtonCancel=Zru�i�
ButtonYes=&�no
ButtonYesToAll=�no &v�etk�m
ButtonNo=&Nie
ButtonNoToAll=Ni&e v�etk�m
ButtonFinish=&Dokon�i�
ButtonBrowse=&Prech�dza�...
ButtonWizardBrowse=&Prech�dza�...
ButtonNewFolder=&Vytvori� nov� adres�r

; *** "Select Language" dialog messages
SelectLanguageTitle=V�ber jazyka Sprievodcu in�tal�ciou
SelectLanguageLabel=Zvo�te jazyk, ktor� sa m� pou�i� pri in�tal�cii.

; *** Common wizard text
ClickNext=Pokra�ujte kliknut�m na tla�idlo "�alej", alebo ukon�te sprievodcu in�tal�ciou tla�idlom "Zru�i�".
BeveledLabel=
BrowseDialogTitle=N�js� adres�r
BrowseDialogLabel=Z dole uveden�ho zoznamu vyberte adres�r a kliknite na "OK".
NewFolderName=Nov� adres�r

; *** "Welcome" wizard page
WelcomeLabel1=V�ta V�s Sprievodca in�tal�ciou produktu [name].
WelcomeLabel2=Produkt [name/ver] sa nain�taluje do tohto po��ta�a.%n%nSk�r, ako budete pokra�ova�, odpor��ame ukon�i� v�etky spusten� aplik�cie.

; *** "Password" wizard page
WizardPassword=Heslo
PasswordLabel1=T�to in�tal�cia je chr�nen� heslom.
PasswordLabel3=Zadajte heslo a pokra�ujte kliknut�m na tla�idlo "�alej". Pri zad�van� hesla rozli�ujte mal� a ve�k� p�smen�.
PasswordEditLabel=&Heslo:
IncorrectPassword=Zadan� heslo nie je spr�vne. Sk�ste to e�te raz pros�m.

; *** "License Agreement" wizard page
WizardLicense=Licen�n� zmluva
LicenseLabel=Sk�r, ako budete pokra�ova�, pre��tajte si tieto d�le�it� inform�cie, pros�m.
LicenseLabel3=Pre��tajte si t�to Licen�n� zmluvu pros�m. Aby mohla in�tal�cia pokra�ova�, mus�te s�hlasi� s podmienkami tejto zmluvy.
LicenseAccepted=&S�hlas�m s podmienkami Licen�nej zmluvy
LicenseNotAccepted=&Nes�hlas�m s podmienkami Licen�nej zmluvy

; *** "Information" wizard pages
WizardInfoBefore=Inform�cie
InfoBeforeLabel=Sk�r, ako budete pokra�ova�, pre��tajte si tieto d�le�it� inform�cie, pros�m.
InfoBeforeClickLabel=Pokra�ujte v in�tal�cii kliknut�m na tla�idlo "�alej".
WizardInfoAfter=Inform�cie
InfoAfterLabel=Sk�r, ako budete pokra�ova�, pre��tajte si tieto d�le�it� inform�cie pros�m.
InfoAfterClickLabel=Pokra�ujte v in�tal�cii kliknut�m na tla�idlo "�alej".

; *** "User Information" wizard page
WizardUserInfo=Inform�cie o pou��vate�ovi
UserInfoDesc=Zadajte po�adovan� inform�cie pros�m.
UserInfoName=&Pou��vate�sk� meno:
UserInfoOrg=&Organiz�cia:
UserInfoSerial=&S�riov� ��slo:
UserInfoNameRequired=Meno pou��vate�a mus� by� zadan�.

; *** "Select Destination Location" wizard page
WizardSelectDir=Vyberte cie�ov� adres�r
SelectDirDesc=Kde m� by� produkt [name] nain�talovan�?
SelectDirLabel3=Sprievodca nain�taluje produkt [name] do nasleduj�ceho adres�ra.
SelectDirBrowseLabel=Pokra�ujte kliknut�m na tla�idlo "�alej". Ak chcete vybra� in� adres�r, kliknite na tla�idlo "Prech�dza�".
DiskSpaceGBLabel=In�tal�cia vy�aduje najmenej [gb] GB miesta v disku.
DiskSpaceMBLabel=In�tal�cia vy�aduje najmenej [mb] MB miesta v disku.
CannotInstallToNetworkDrive=Sprievodca in�tal�ciou nem��e in�talova� do sie�ovej jednotky.
CannotInstallToUNCPath=Sprievodca in�tal�ciou nem��e in�talova� do UNC umiestnenia.
InvalidPath=Mus�te zada� �pln� cestu vr�tane p�smena jednotky; napr�klad:%n%nC:\Aplik�cia%n%nalebo cestu UNC v tvare:%n%n\\Server\Zdie�an� adres�r
InvalidDrive=Vami vybran� jednotka alebo cesta UNC neexistuje, alebo nie je dostupn�. Vyberte in� umiestnenie pros�m.
DiskSpaceWarningTitle=Nedostatok miesta v disku
DiskSpaceWarning=Sprievodca in�tal�ciou vy�aduje najmenej %1 KB vo�n�ho miesta pre in�tal�ciu produktu, ale vo vybranej jednotke je dostupn�ch iba %2 KB.%n%nAj napriek tomu chcete pokra�ova�?
DirNameTooLong=N�zov adres�ra alebo cesta s� pr�li� dlh�.
InvalidDirName=N�zov adres�ra nie je spr�vny.
BadDirName32=N�zvy adres�rov nesm� obsahova� �iadny z nasleduj�cich znakov:%n%n%1
DirExistsTitle=Adres�r u� existuje
DirExists=Adres�r:%n%n%1%n%nu� existuje. Aj napriek tomu chcete nain�talova� produkt do tohto adres�ra?
DirDoesntExistTitle=Adres�r neexistuje
DirDoesntExist=Adres�r: %n%n%1%n%ne�te neexistuje. M� sa tento adres�r vytvori�?

; *** "Select Components" wizard page
WizardSelectComponents=Vyberte komponenty
SelectComponentsDesc=Ak� komponenty maj� by� nain�talovan�?
SelectComponentsLabel2=Za�krtnite iba komponenty, ktor� chcete nain�talova�; komponenty, ktor� se nemaj� in�talova�, nechajte neza�krtnut�. Pokra�ujte kliknut�m na tla�idlo "�alej".
FullInstallation=�pln� in�tal�cia
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompaktn� in�tal�cia
CustomInstallation=Volite�n� in�tal�cia
NoUninstallWarningTitle=Komponenty existuj�
NoUninstallWarning=Sprievodca in�tal�ciou zistil �e nasleduj�ce komponenty u� s� v tomto po��ta�i nain�talovan�:%n%n%1%n%nAk ich teraz nezahrniete do v�beru, nebud� nesk�r odin�talovan�.%n%nAj napriek tomu chcete pokra�ova�?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Vybran� komponenty vy�aduj� najmenej [gb] GB miesta v disku.
ComponentsDiskSpaceMBLabel=Vybran� komponenty vy�aduj� najmenej [mb] MB miesta v disku.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Vyberte �al�ie �lohy
SelectTasksDesc=Ktor� �al�ie �lohy maj� by� vykonan�?
SelectTasksLabel2=Vyberte �al�ie �lohy, ktor� maj� by� vykonan� po�as in�tal�cie produktu [name] a pokra�ujte kliknut�m na tla�idlo "�alej".

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Vyberte skupinu v ponuke �tart
SelectStartMenuFolderDesc=Kam m� sprievodca in�tal�cie umiestni� z�stupcov aplik�cie?
SelectStartMenuFolderLabel3=Sprievodca in�tal�ciou vytvor� z�stupcov aplik�cie v nasleduj�com adres�ri ponuky �tart.
SelectStartMenuFolderBrowseLabel=Pokra�ujte kliknut�m na tla�idlo �alej. Ak chcete zvoli� in� adres�r, kliknite na tla�idlo "Prech�dza�".
MustEnterGroupName=Mus�te zada� n�zov skupiny.
GroupNameTooLong=N�zov adres�ra alebo cesta s� pr�li� dlh�.
InvalidGroupName=N�zov adres�ra nie je spr�vny.
BadGroupName=N�zov skupiny nesmie obsahova� �iadny z nasleduj�cich znakov:%n%n%1
NoProgramGroupCheck2=&Nevytv�ra� skupinu v ponuke �tart

; *** "Ready to Install" wizard page
WizardReady=In�tal�cia je pripraven�
ReadyLabel1=Sprievodca in�tal�ciou je teraz pripraven� nain�talova� produkt [name] na V� po��ta�.
ReadyLabel2a=Pokra�ujte v in�tal�cii kliknut�m na tla�idlo "In�talova�". Ak chcete zmeni� niektor� nastavenia in�tal�cie, kliknite na tla�idlo "< Sp�".
ReadyLabel2b=Pokra�ujte v in�tal�cii kliknut�m na tla�idlo "In�talova�".
ReadyMemoUserInfo=Inform�cie o pou��vate�ovi:
ReadyMemoDir=Cie�ov� adres�r:
ReadyMemoType=Typ in�tal�cie:
ReadyMemoComponents=Vybran� komponenty:
ReadyMemoGroup=Skupina v ponuke �tart:
ReadyMemoTasks=�al�ie �lohy:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=S�ahovanie dodato�n�ch s�borov...
ButtonStopDownload=&Zastavi� s�ahovanie
StopDownload=Naozaj chcete zastavi� s�ahovanie?
ErrorDownloadAborted=S�ahovanie preru�en�
ErrorDownloadFailed=S�ahovanie zlyhalo: %1 %2
ErrorDownloadSizeFailed=Zlyhalo z�skanie ve�kosti: %1 %2
ErrorFileHash1=Kontrola hodnoty s�boru zlyhala: %1
ErrorFileHash2=Nespr�vna kontroln� hodnota: o�ak�vala sa %1, zisten� %2
ErrorProgress=Nespr�vny priebeh: %1 z %2
ErrorFileSize=Nespr�vna ve�kos� s�boru: o�ak�vala sa %1, zisten� %2

; *** "Preparing to Install" wizard page
WizardPreparing=Pr�prava in�tal�cie
PreparingDesc=Sprievodca in�tal�ciou pripravuje in�tal�ciu produktu [name] do tohto po��ta�a.
PreviousInstallNotCompleted=In�tal�cia/odin�tal�cia predo�l�ho produktu nebola �plne dokon�en�. Dokon�enie tohto procesu vy�aduje re�tart po��ta�a.%n%nPo re�tartovan� po��ta�a znovu spustite Sprievodcu in�tal�ciou, aby bolo mo�n� kompletne dokon�i� in�tal�ciu produktu [name].
CannotContinue=Sprievodca in�tal�ciou nem��e pokra�ova�. Ukon�ite, pros�m, sprievodcu in�tal�ciou kliknut�m na tla�idlo "Zru�i�".
ApplicationsFound=Nasleduj�ce aplik�cie pracuj� so s�bormi, ktor� mus� Sprievodca in�tal�ciou aktualizova�. Odpor��ame, aby ste povolili Sprievodcovi in�tal�ciou automaticky ukon�i� tieto aplik�cie.
ApplicationsFound2=Nasleduj�ce aplik�cie pracuj� so s�bormi, ktor� mus� Sprievodca in�tal�ciou aktualizova�. Odpor��ame, aby ste povolili Sprievodcovi in�tal�ciou automaticky ukon�i� tieto aplik�cie. Po dokon�en� in�tal�cie sa Sprievodca in�tal�ciou pok�si tieto aplik�cie op�tovne spusti�.
CloseApplications=&Automaticky ukon�i� aplik�cie
DontCloseApplications=&Neukon�ova� aplik�cie
ErrorCloseApplications=Sprievodca in�tal�ciou nemohol automaticky zatvori� v�etky aplik�cie. Odpor��ame, aby ste ru�ne ukon�ili v�etky aplik�cie, ktor� pou��vaj� s�bory a ktor� m� Sprievodca aktualizova�.
PrepareToInstallNeedsRestart=Sprievodca in�tal�ciou potrebuje re�tartova� tento po��ta�. Po re�tartovan� po��ta�a znovu spustite tohto Sprievodcu in�tal�ciou, aby sa in�tal�cia [name] dokon�ila.%n%nChcete teraz re�tartova� tento po��ta�?

; *** "Installing" wizard page
WizardInstalling=In�talujem
InstallingLabel=Po�kajte pros�m, pokia� Sprievodca in�tal�ciou dokon�� in�tal�ciu produktu [name] do tohto po��ta�a.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Dokon�uje sa in�tal�cia produktu [name]
FinishedLabelNoIcons=Sprievodca in�tal�ciou dokon�il in�tal�ciu produktu [name] do tohto po��ta�a.
FinishedLabel=Sprievodca in�tal�ciou dokon�il in�tal�ciu produktu [name] do tohto po��ta�a. Produkt je mo�n� spusti� pomocou nain�talovan�ch ikon a z�stupcov.
ClickFinish=Ukon�te Sprievodcu in�tal�ciou kliknut�m na tla�idlo "Dokon�i�".
FinishedRestartLabel=Pre dokon�enie in�tal�cie produktu [name] je nutn� re�tartova� tento po��ta�. �el�te si teraz re�tartova� tento po��ta�?
FinishedRestartMessage=Pre dokon�enie in�tal�cie produktu [name] je nutn� re�tartova� tento po��ta�.%n%n�el�te si teraz re�tartova� tento po��ta�?
ShowReadmeCheck=�no, chcem zobrazi� dokument "�ITAJMA"
YesRadio=&�no, chcem teraz re�tartova� po��ta�
NoRadio=&Nie, po��ta� re�tartujem nesk�r

; used for example as 'Run MyProg.exe'
RunEntryExec=Spusti� %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Zobrazi� %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Sprievodca in�tal�ciou vy�aduje �al�� disk
SelectDiskLabel2=Vlo�te pros�m, disk %1 a kliknite na tla�idlo "OK".%n%nAk sa s�bory tohto disku nach�dzaj� v inom adres�ri ako v tom, ktor� je zobrazen� ni��ie, zadajte spr�vnu cestu alebo kliknite na tla�idlo "Prech�dza�".
PathLabel=&Cesta:
FileNotInDir2=S�bor "%1" sa ned� n�js� v "%2". Vlo�te pros�m, spr�vny disk, alebo zvo�te in� adres�r.
SelectDirectoryLabel=�pecifikujte pros�m, umiestnenie �al�ieho disku.

; *** Installation phase messages
SetupAborted=In�tal�cia nebola �plne dokon�en�.%n%nOpravte chybu a op� spustite Sprievodcu in�tal�ciou pros�m.
AbortRetryIgnoreSelectAction=Vyberte akciu
AbortRetryIgnoreRetry=&Sk�si� znovu
AbortRetryIgnoreIgnore=&Ignorova� chybu a pokra�ova�
AbortRetryIgnoreCancel=Zru�i� in�tal�ciu

; *** Installation status messages
StatusClosingApplications=Ukon�ovanie aplik�ci�...
StatusCreateDirs=Vytv�raj� sa adres�re...
StatusExtractFiles=Rozba�uj� sa s�bory...
StatusCreateIcons=Vytv�raj� sa ikony a z�stupcovia...
StatusCreateIniEntries=Vytv�raj� sa z�znamy v konfigura�n�ch s�boroch...
StatusCreateRegistryEntries=Vytv�raj� sa z�znamy v syst�movom registri...
StatusRegisterFiles=Registruj� sa s�bory...
StatusSavingUninstall=Ukladaj� sa inform�cie potrebn� pre neskor�ie odin�talovanie produktu...
StatusRunProgram=Dokon�uje sa in�tal�cia...
StatusRestartingApplications=Re�tartovanie aplik�ci�...
StatusRollback=Vykonan� zmeny sa vracaj� sp�...

; *** Misc. errors
ErrorInternal2=Intern� chyba: %1
ErrorFunctionFailedNoCode=%1 zlyhala
ErrorFunctionFailed=%1 zlyhala; k�d %2
ErrorFunctionFailedWithMessage=%1 zlyhala; k�d %2.%n%3
ErrorExecutingProgram=Ned� sa spusti� s�bor:%n%1

; *** Registry errors
ErrorRegOpenKey=Do�lo k chybe pri otv�ran� k���a syst�mov�ho registra:%n%1\%2
ErrorRegCreateKey=Do�lo k chybe pri vytv�ran� k���a syst�mov�ho registra:%n%1\%2
ErrorRegWriteKey=Do�lo k chybe pri z�pise k���a do syst�mov�ho registra:%n%1\%2

; *** INI errors
ErrorIniEntry=Do�lo k chybe pri vytv�ran� z�znamu v konfigura�nom s�bore "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Presko�i� tento s�bor (neodpor��an�)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignorova� chybu a pokra�ova� (neodpor��an�)
SourceIsCorrupted=Zdrojov� s�bor je po�koden�
SourceDoesntExist=Zdrojov� s�bor "%1" neexistuje
ExistingFileReadOnly2=Existuj�ci s�bor nie je mo�n� prep�sa�, preto�e je ozna�en� atrib�tom Iba na ��tanie.
ExistingFileReadOnlyRetry=&Odstr�ni� atrib�t Iba na ��tanie a sk�si� znovu
ExistingFileReadOnlyKeepExisting=&Ponecha� existuj�ci s�bor
ErrorReadingExistingDest=Do�lo k chybe pri pokuse o ��tanie existuj�ceho s�boru:
FileExistsSelectAction=Vyberte akciu
FileExists2=S�bor u� existuje.
FileExistsOverwriteExisting=&Prep�sa� existuj�ci s�bor
FileExistsKeepExisting=Ponecha� &existuj�ci s�bor
FileExistsOverwriteOrKeepAll=&Vykona� pre v�etky �al�ie konflikty
ExistingFileNewerSelectAction=Vyberte akciu
ExistingFileNewer2=Existuj�ci s�bor je nov�� ako s�bor, ktor� sa Sprievodca in�tal�ciou pok��a nain�talova�.
ExistingFileNewerOverwriteExisting=&Prep�sa� existuj�ci s�bor
ExistingFileNewerKeepExisting=Ponecha� &existuj�ci s�bor (odpor��an�)
ExistingFileNewerOverwriteOrKeepAll=&Vykona� pre v�etky �al�ie konflikty
ErrorChangingAttr=Do�lo k chybe pri pokuse o modifik�ciu atrib�tov existuj�ceho s�boru:
ErrorCreatingTemp=Do�lo k chybe pri pokuse o vytvorenie s�boru v cie�ovom adres�ri:
ErrorReadingSource=Do�lo k chybe pri pokuse o ��tanie zdrojov�ho s�boru:
ErrorCopying=Do�lo k chybe pri pokuse o skop�rovanie s�boru:
ErrorReplacingExistingFile=Do�lo k chybe pri pokuse o nahradenie existuj�ceho s�boru:
ErrorRestartReplace=Zlyhala funkcia "RestartReplace" Sprievodcu in�tal�ciou:
ErrorRenamingTemp=Do�lo k chybe pri pokuse o premenovanie s�boru v cie�ovom adres�ri:
ErrorRegisterServer=Ned� sa vykona� registr�cia DLL/OCX: %1
ErrorRegSvr32Failed=Volanie RegSvr32 zlyhalo s n�vratov�m k�dom %1
ErrorRegisterTypeLib=Ned� sa vykona� registr�cia typovej kni�nice: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32bitov�
UninstallDisplayNameMark64Bit=64bitov�
UninstallDisplayNameMarkAllUsers=V�etci u��vatelia
UninstallDisplayNameMarkCurrentUser=Aktu�lny u��vate�

; *** Post-installation errors
ErrorOpeningReadme=Do�lo k chybe pri pokuse o otvorenie dokumentu "�ITAJMA".
ErrorRestartingComputer=Sprievodcovi in�tal�ciou sa nepodarilo re�tartova� tento po��ta�. Re�tartujte ho manu�lne pros�m.

; *** Uninstaller messages
UninstallNotFound=S�bor "%1" neexistuje. Produkt sa ned� odin�talova�.
UninstallOpenError=S�bor "%1" nie je mo�n� otvori�. Produkt nie je mo�n� odin�talova�.
UninstallUnsupportedVer=Sprievodcovi odin�tal�ciou sa nepodarilo rozpozna� form�t s�boru obsahuj�ceho inform�cie na odin�talovanie produktu "%1". Produkt sa ned� odin�talova�
UninstallUnknownEntry=V s�bore obsahuj�com inform�cie na odin�talovanie produktu bola zisten� nezn�ma polo�ka (%1)
ConfirmUninstall=Naozaj chcete odin�talova� %1 a v�etky jeho komponenty?
UninstallOnlyOnWin64=Tento produkt je mo�n� odin�talova� iba v 64-bitov�ch verzi�ch MS Windows.
OnlyAdminCanUninstall=K odin�talovaniu tohto produktu mus�te by� prihl�sen� s pr�vami Administr�tora.
UninstallStatusLabel=Po�kajte pros�m, k�m produkt %1 nebude odin�talovan� z tohto po��ta�a.
UninstalledAll=%1 bol �spe�ne odin�talovan� z tohto po��ta�a.
UninstalledMost=%1 bol odin�talovan� z tohto po��ta�a.%n%nNiektor� jeho komponenty sa v�ak nepodarilo odin�talova�. M��ete ich odin�talova� manu�lne.
UninstalledAndNeedsRestart=Na dokon�enie odin�tal�cie produktu %1 je potrebn� re�tartova� tento po��ta�.%n%nChcete ihne� re�tartova� tento po��ta�?
UninstallDataCorrupted=S�bor "%1" je po�koden�. Produkt sa ned� odin�talova�

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Odin�talova� zdie�an� s�bor?
ConfirmDeleteSharedFile2=Syst�m indikuje, �e nasleduj�ci zdie�an� s�bor nie je pou��van� �iadnymi in�mi aplik�ciami. M� Sprievodca odin�tal�ciou tento zdie�an� s�bor odstr�ni�?%n%nAk niektor� aplik�cie tento s�bor pou��vaj�, nemusia po jeho odin�talovan� pracova� spr�vne. Pokia� to neviete spr�vne pos�di�, odpor��ame zvoli� "Nie". Ponechanie tohto s�boru v syst�me nesp�sob� �iadnu �kodu.
SharedFileNameLabel=N�zov s�boru:
SharedFileLocationLabel=Umiestnenie:
WizardUninstalling=Stav odin�talovania
StatusUninstalling=Prebieha odin�talovanie %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=In�talovanie %1.
ShutdownBlockReasonUninstallingApp=Odin�talovanie %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 verzia %2
AdditionalIcons=�al�� z�stupcovia:
CreateDesktopIcon=Vytvori� z�stupcu na &ploche
CreateQuickLaunchIcon=Vytvori� z�stupcu na paneli &R�chle spustenie
ProgramOnTheWeb=Aplik�cia %1 na internete
UninstallProgram=Odin�talova� aplik�ciu %1 
LaunchProgram=Spusti� aplik�ciu %1
AssocFileExtension=Vytvori� &asoci�ciu medzi s�bormi typu %2 a aplik�ciou %1
AssocingFileExtension=Vytv�ra sa asoci�cia medzi s�bormi typu %2 a aplik�ciou %1...
AutoStartProgramGroupDescription=Pri spusten�:
AutoStartProgram=Automaticky spusti� %1
AddonHostProgramNotFound=Nepodarilo sa n�js� %1 v adres�ri, ktor� ste zvolili.%n%nChcete napriek tomu pokra�ova�?
