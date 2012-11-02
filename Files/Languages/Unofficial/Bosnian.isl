; *** Inno Setup version 5.5.0+ English messages ***
;
; Bosnian translation by Kenan Dervisevic (kenan3008@gmail.com)
;

[LangOptions]
LanguageName=Bosanski
LanguageID=$141a
LanguageCodePage=0
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
SetupAppTitle=Instalacija
SetupWindowTitle=Instalacija - %1
UninstallAppTitle=Deinstalacija
UninstallAppFullTitle=%1 Deinstalacija

; *** Misc. common
InformationTitle=Informacija
ConfirmTitle=Potvrda
ErrorTitle=Gre�ka

; *** SetupLdr messages
SetupLdrStartupMessage=Zapo�eli ste instalaciju programa %1. �elite li nastaviti?
LdrCannotCreateTemp=Ne mogu kreirati privremenu datoteku. Instalacija prekinuta
LdrCannotExecTemp=Ne mogu izvr�iti datoteku u privremenom folderu. Instalacija prekinuta

; *** Startup error messages
LastErrorMessage=%1.%n%nGre�ka %2: %3
SetupFileMissing=Datoteka %1 se ne nalazi u instalacijskom folderu. Molimo vas da rije�ite problem ili nabavite novu kopiju programa.
SetupFileCorrupt=Instalacijske datoteke sadr�e gre�ku. Molimo vas da nabavite novu kopiju programa.
SetupFileCorruptOrWrongVer=Instalacijske datoteke sadr�e gre�ku, ili nisu kompatibilne sa ovom verzijom instalacije. Molimo vas rije�ite problem ili nabavite novu kopiju programa.
InvalidParameter=Neispravan parametar je proslije�en komandnoj liniji:%n%n%1
SetupAlreadyRunning=Instalacija je ve� pokrenuta.
WindowsVersionNotSupported=Ovaj program ne podr�ava verziju Windowsa koja je instalirana na ovom ra�unaru.
WindowsServicePackRequired=Ovaj program zahtjeva %1 Service Pack %2 ili noviji.
NotOnThisPlatform=Ovaj program ne radi na %1.
OnlyOnThisPlatform=Ovaj program se mora pokrenuti na %1.
OnlyOnTheseArchitectures=Ovaj program se mo�e instalirati samo na verzijama Windowsa napravljenim za sljede�e arhitekture procesora:%n%n%1
MissingWOW64APIs=Verzija Windowsa koju koristite ne sadr�i funkcionalnosti potrebne da bi instalacijski program mogao instalirati 64-bitnu verziju. Da bi ispravili taj problem, molimo instalirajte Service Pack %1.
WinVersionTooLowError=Ovaj program zahtjeva %1 verzije %2 ili noviju.
WinVersionTooHighError=Ovaj program se ne mo�e instalirati na %1 verziji %2 ili novijoj.
AdminPrivilegesRequired=Morate imati administratorska prava pri instaliranju ovog programa.
PowerUserPrivilegesRequired=Morate imati administratorska prava ili biti �lan grupe Power Users prilikom instaliranja ovog programa.
SetupAppRunningError=Instalacija je detektovala da je %1 pokrenut.%n%nMolimo zatvorite program i sve njegove kopije i potom kliknite Dalje za nastavak ili Odustani za prekid.
UninstallAppRunningError=Deinstalacija je detektovala da je %1 trenutno pokrenut.%n%nMolimo zatvorite program i sve njegove kopije i potom kliknite Dalje za nastavak ili Odustani za prekid.

; *** Misc. errors
ErrorCreatingDir=Instalacija nije mogla kreirati folder "%1"
ErrorTooManyFilesInDir=Instalacija nije mogla kreirati datoteku u folderu "%1" zato �to on sadr�i previ�e datoteka

; *** Setup common messages
ExitSetupTitle=Prekid instalacije
ExitSetupMessage=Instalacija nije zavr�ena. Ako sada iza�ete, program ne�e biti instaliran.%n%nInstalaciju mo�ete pokrenuti kasnije u slu�aju da je �elite zavr�iti.%n%nPrekid instalacije?
AboutSetupMenuItem=&O instalaciji...
AboutSetupTitle=O instalaciji
AboutSetupMessage=%1 verzija %2%n%3%n%n%1 po�etna stranica:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< Na&zad
ButtonNext=Da&lje >
ButtonInstall=&Instaliraj
ButtonOK=U redu
ButtonCancel=Otka�i
ButtonYes=&Da
ButtonYesToAll=Da za &sve
ButtonNo=&Ne
ButtonNoToAll=N&e za sve
ButtonFinish=&Zavr�i
ButtonBrowse=&Izaberi...
ButtonWizardBrowse=Iza&beri...
ButtonNewFolder=&Napravi novi folder

; *** "Select Language" dialog messages
SelectLanguageTitle=Izaberite jezik instalacije
SelectLanguageLabel=Izaberite jezik koji �elite koristiti pri instalaciji:

; *** Common wizard text
ClickNext=Kliknite na Dalje za nastavak ili Otka�i za prekid instalacije.
BeveledLabel=
BrowseDialogTitle=Izaberite folder
BrowseDialogLabel=Izaberite folder iz liste ispod, pa onda kliknite na U redu.
NewFolderName=Novi folder

; *** "Welcome" wizard page
WelcomeLabel1=Dobro do�li u instalaciju programa [name]
WelcomeLabel2=Ovaj program �e instalirati [name/ver] na va� ra�unar.%n%nPreporu�ujemo da zatvorite sve druge programe prije nastavka i da privremeno onemogu�ite va� antivirus i firewall.

; *** "Password" wizard page
WizardPassword=�ifra
PasswordLabel1=Instalacija je za�ti�ena �ifrom.
PasswordLabel3=Upi�ite �ifru i kliknite Dalje za nastavak. �ifre su osjetljive na mala i velika slova.
PasswordEditLabel=&�ifra:
IncorrectPassword=Upisali ste pogre�nu �ifru. Poku�ajte ponovo.

; *** "License Agreement" wizard page
WizardLicense=Ugovor o kori�tenju
LicenseLabel=Molimo vas da, prije nastavka, pa�ljivo pro�itajte sljede�e informacije.
LicenseLabel3=Molimo vas da pa�ljivo pro�itate Ugovor o kori�tenju. Morate prihvatiti uslove ugovora kako biste mogli nastaviti s instalacijom.
LicenseAccepted=&Prihvatam ugovor
LicenseNotAccepted=&Ne prihvatam ugovor

; *** "Information" wizard pages
WizardInfoBefore=Informacija
InfoBeforeLabel=Molimo vas da, prije nastavka, pro�itate sljede�e informacije.
InfoBeforeClickLabel=Kada budete spremni nastaviti instalaciju, kliknite na Dalje.
WizardInfoAfter=Informacija
InfoAfterLabel=Molimo vas da, prije nastavka, pro�itate sljede�e informacije.
InfoAfterClickLabel=Kada budete spremni nastaviti instalaciju, kliknite na Dalje.

; *** "User Information" wizard page
WizardUserInfo=Informacije o korisniku
UserInfoDesc=Upi�ite va�e li�ne informacije.
UserInfoName=&Ime korisnika:
UserInfoOrg=&Organizacija:
UserInfoSerial=&Serijski broj:
UserInfoNameRequired=Morate upisati ime.

; *** "Select Destination Location" wizard page
WizardSelectDir=Odaberite odredi�ni folder
SelectDirDesc=Gdje �elite da instalirate [name]?
SelectDirLabel3=Instalacija �e instalirati [name] u sljede�i folder.
SelectDirBrowseLabel=Za nastavak, kliknite Dalje. Ako �elite izabrati drugi folder, kliknite Izaberi.
DiskSpaceMBLabel=Ovaj program zahtjeva najmanje [mb] MB slobodnog prostora na disku.
CannotInstallToNetworkDrive=Instalacija nije mogu�a na mre�nom disku.
CannotInstallToUNCPath=Instalacija nije mogu�a za UNC putanju.
InvalidPath=Morate unijeti punu putanju zajedno sa slovom diska; npr:%n%nC:\APP%n%nili UNC putanju u obliku:%n%n\\server\share
InvalidDrive=Disk ili UNC share koji ste odabrali ne postoji ili je nedostupan. Odaberite neki drugi.
DiskSpaceWarningTitle=Nedovoljno prostora na disku
DiskSpaceWarning=Instalacija zahtjeva bar %1 KB slobodnog prostora, a odabrani disk ima samo %2 KB na raspolaganju.%n%nDa li �elite nastaviti?
DirNameTooLong=Naziv ili putanja do foldera su predugi.
InvalidDirName=Naziv foldera nije ispravan.
BadDirName32=Naziv foldera ne smije sadr�avati niti jedan od sljede�ih znakova:%n%n%1
DirExistsTitle=Folder postoji
DirExists=Folder:%n%n%1%n%nve� postoji. �elite li i dalje izvr�iti instalaciju u njega?
DirDoesntExistTitle=Folder ne postoji
DirDoesntExist=Folder:%n%n%1%n%nne postoji. �elite li ga napraviti?

; *** "Select Components" wizard page
WizardSelectComponents=Odaberite komponente
SelectComponentsDesc=Koje komponente �elite instalirati?
SelectComponentsLabel2=Odaberite komponente koje �elite instalirati ili uklonite kva�icu pored komponenti koje ne �elite. Kliknite Dalje kad budete spremni da nastavite.
FullInstallation=Puna instalacija
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompaktna instalacija
CustomInstallation=Instalacija prema �elji
NoUninstallWarningTitle=Komponente postoje
NoUninstallWarning=Instalacija je detektovala da na va�em ra�unaru ve� postoje sljede�e komponente:%n%n%1%n%nAko ove komponente ne odaberete, ne�e do�i do njihove deinstalacije.%n%n�elite li ipak nastaviti?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Trenutni izbor zahtjeva bar [mb] MB prostora na disku.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Izaberite dodatne radnje
SelectTasksDesc=Koje dodatne radnje �elite da se izvr�e?
SelectTasksLabel2=Izaberite radnje koje �e se izvr�iti tokom instalacije programa [name], onda kliknite Dalje.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Izaberite programsku grupu
SelectStartMenuFolderDesc=Gdje instalacija treba da napravi pre�ice?
SelectStartMenuFolderLabel3=Izaberite folder iz Start menija u koji �elite da instalacija kreira pre�icu, a zatim kliknite na Dalje.
SelectStartMenuFolderBrowseLabel=Za nastavak, kliknite Dalje. Ako �elite da izaberete drugi folder, kliknite Izaberi.
MustEnterGroupName=Morate unijeti ime programske grupe.
GroupNameTooLong=Naziv foldera ili putanje je predug.
InvalidGroupName=Naziv foldera nije ispravan.
BadGroupName=Naziv foldera ne smije sadr�avati niti jedan od sljede�ih znakova:%n%n%1
NoProgramGroupCheck2=&Ne kreiraj %1 programsku grupu

; *** "Ready to Install" wizard page
WizardReady=Spreman za instalaciju
ReadyLabel1=Sada smo spremni za instalaciju [name] na va� ra�unar.
ReadyLabel2a=Kliknite na Instaliraj ako �elite instalirati program ili na Nazad ako �elite pregledati ili promjeniti postavke.
ReadyLabel2b=Kliknite na Instaliraj ako �elite nastaviti sa instalacijom programa.
ReadyMemoUserInfo=Informacije o korisniku:
ReadyMemoDir=Odredi�ni folder:
ReadyMemoType=Tip instalacije:
ReadyMemoComponents=Odabrane komponente:
ReadyMemoGroup=Programska grupa:
ReadyMemoTasks=Dodatne radnje:

; *** "Preparing to Install" wizard page
WizardPreparing=Pripremam instalaciju
PreparingDesc=Pripreme za instalaciju [name] na va� ra�unar.
PreviousInstallNotCompleted=Instalacija/deinstalacija prethodnog programa nije zavr�ena. Morate restartovati va� ra�unar kako bi zavr�ili tu instalaciju.%n%nNakon toga, ponovno pokrenite ovaj program kako bi dovr�ili instalaciju za [name].
CannotContinue=Instalacija ne mo�e nastaviti. Molimo vas da kliknete na Odustani za izlaz.
ApplicationsFound=Sljede�e aplikacije koriste datoteke koje ova instalacija treba da nadogradi. Preporu�ujemo vam da omogu�ite instalaciji da automatski zatvori ove aplikacije.
ApplicationsFound2=Sljede�e aplikacije koriste datoteke koje ova instalacija treba da nadogradi. Preporu�ujemo vam da omogu�ite instalaciji da automatski zatvori ove aplikacije. Nakon �to se sve zavr�i, bit �e izvr�en poku�aj ponovnog pokretanja ovih aplikacija.
CloseApplications=&Automatski zatvori aplikacije
DontCloseApplications=&Ne zatvaraj aplikacije

; *** "Installing" wizard page
WizardInstalling=Instaliram
InstallingLabel=Pri�ekajte dok se ne zavr�i instalacija programa [name] na va� ra�unar.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Zavr�avam instalaciju [name]
FinishedLabelNoIcons=Instalacija programa [name] je zavr�ena.
FinishedLabel=Instalacija programa [name] je zavr�ena. Program mo�ete pokrenuti koriste�i instalirane ikone.
ClickFinish=Kliknite na Zavr�i da biste iza�li iz instalacije.
FinishedRestartLabel=Da biste instalaciju programa [name] zavr�ili, potrebno je restartovati ra�unar. �elite li to sada u�initi?
FinishedRestartMessage=Zavr�etak instalacije programa [name] zahtjeva restart va�eg ra�unara.%n%n�elite li to sada u�initi?
ShowReadmeCheck=Da, �elim pro�itati README datoteku.
YesRadio=&Da, restartuj ra�unar sada
NoRadio=&Ne, restartovat �u ra�unar kasnije
; used for example as 'Run MyProg.exe'
RunEntryExec=Pokreni %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Pro�itaj %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Instalacija treba sljede�i disk
SelectDiskLabel2=Molimo ubacite Disk %1 i kliknite U redu.%n%nAko se datoteke na ovom disku nalaze u drugom folderu a ne u onom prikazanom ispod, unesite ispravnu putanju ili kliknite na Izaberi.
PathLabel=&Putanja:
FileNotInDir2=Datoteka "%1" ne postoji u "%2". Molimo vas ubacite odgovoraju�i disk ili odaberete drugi folder.
SelectDirectoryLabel=Molimo odaberite lokaciju sljede�eg diska.

; *** Installation phase messages
SetupAborted=Instalacija nije zavr�ena.%n%nMolimo vas da rije�ite problem i opet pokrenete instalaciju.
EntryAbortRetryIgnore=Kliknite na Retry da poku�ate opet, Ignore da nastavite, ili Abort da prekinete instalaciju.

; *** Installation status messages
StatusClosingApplications=Zatvaram aplikacije...
StatusCreateDirs=Kreiram foldere...
StatusExtractFiles=Raspakujem datoteke...
StatusCreateIcons=Kreiram pre�ice...
StatusCreateIniEntries=Kreiram INI datoteke...
StatusCreateRegistryEntries=Kreiram podatke za registracijsku bazu...
StatusRegisterFiles=Registrujem datoteke...
StatusSavingUninstall=Snimam deinstalacijske informacije...
StatusRunProgram=Zavr�avam instalaciju...
StatusRestartingApplications=Restartujem aplikaciju...
StatusRollback=Poni�tavam promjene...

; *** Misc. errors
ErrorInternal2=Interna gre�ka: %1
ErrorFunctionFailedNoCode=%1 nije uspjelo
ErrorFunctionFailed=%1 nije uspjelo; kod %2
ErrorFunctionFailedWithMessage=%1 nije uspjelo; kod %2.%n%3
ErrorExecutingProgram=Ne mogu pokrenuti datoteku:%n%1

; *** Registry errors
ErrorRegOpenKey=Gre�ka pri otvaranju registracijskog klju�a:%n%1\%2
ErrorRegCreateKey=Gre�ka pri kreiranju registracijskog klju�a:%n%1\%2
ErrorRegWriteKey=Gre�ka pri zapisivanju registracijskog klju�a:%n%1\%2

; *** INI errors
ErrorIniEntry=Gre�ka pri kreiranju INI podataka u datoteci "%1".

; *** File copying errors
FileAbortRetryIgnore=Kliknite Retry da poku�ate ponovo, Ignore da presko�ite ovu datoteku (nije preporu�eno), ili Abort da prekinete instalaciju.
FileAbortRetryIgnore2=Kliknite Retry da poku�ate ponovo, Ignore da presko�ite ovu datoteku (nije preporu�eno), ili Abort da prekinete instalaciju.
SourceIsCorrupted=Izvorna datoteka je o�te�ena
SourceDoesntExist=Izvorna datoteka "%1" ne postoji
ExistingFileReadOnly=Postoje�a datoteka je ozna�ena kao samo za �itanje.%n%nKliknite Retry da uklonite ovu oznaku i poku�ate ponovo, Ignore da presko�ite ovu datoteku, ili Abort da prekinete instalaciju.
ErrorReadingExistingDest=Do�lo je do gre�ke prilikom poku�aja �itanja postoje�e datoteke:
FileExists=Datoteka ve� postoji.%n%n�elite li pisati preko nje?
ExistingFileNewer=Postoje�a datoteka je novija od one koju poku�avate instalirati. Preporu�ujemo vam da zadr�ite postoje�u datoteku.%n%n�elite li zadr�ati postoje�u datoteku?
ErrorChangingAttr=Pojavila se gre�ka prilikom poku�aja promjene atributa postoje�e datoteke:
ErrorCreatingTemp=Pojavila se gre�ka prilikom poku�aja kreiranja datoteke u odredi�nom folderu:
ErrorReadingSource=Pojavila se gre�ka prilikom poku�aja �itanja izvorne datoteke:
ErrorCopying=Pojavila se gre�ka prilikom poku�aja kopiranja datoteke:
ErrorReplacingExistingFile=Pojavila se gre�ka prilikom poku�aja zamjene datoteke:
ErrorRestartReplace=Ponovno pokretanje i zamjena nije uspjela:
ErrorRenamingTemp=Pojavila se gre�ka prilikom poku�aja preimenovanja datoteke u odredi�nom folderu:
ErrorRegisterServer=Ne mogu registrovati DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 nije ispravno izvr�en, kod na kraju izvr�avanja %1
ErrorRegisterTypeLib=Ne mogu registrovati tip biblioteke: %1

; *** Post-installation errors
ErrorOpeningReadme=Pojavila se gre�ka prilikom poku�aja otvaranja README datoteke.
ErrorRestartingComputer=Instalacija ne mo�e restartovati va� ra�unar. Molimo vas da to u�inite ru�no.

; *** Uninstaller messages
UninstallNotFound=Datoteka "%1" ne postoji. Deinstalacija prekinuta.
UninstallOpenError=Datoteka "%1" se ne mo�e otvoriti. Deinstalacija nije mogu�a
UninstallUnsupportedVer=Deinstalacijska log datoteka "%1" je u formatu koji nije prepoznat od ove verzije deinstalera. Nije mogu�a deinstalacija
UninstallUnknownEntry=Nepoznat zapis (%1) je pronadjen u deinstalacijskoj log datoteci
ConfirmUninstall=Da li ste sigurni da �elite ukloniti %1 i sve njegove komponente?
UninstallOnlyOnWin64=Ovaj program se mo�e deinstalirati samo na 64-bitnom Windowsu.
OnlyAdminCanUninstall=Ova instalacija mo�e biti uklonjena samo od korisnika sa administratorskim privilegijama.
UninstallStatusLabel=Molimo pri�ekajte dok %1 ne bude uklonjen s va�eg ra�unara.
UninstalledAll=Program %1 je uspje�no uklonjen sa va�eg ra�unara.
UninstalledMost=Deinstalacija programa %1 je zavr�ena.%n%nNeke elemente nije bilo mogu�e ukloniti. Molimo vas da to u�inite ru�no.
UninstalledAndNeedsRestart=Da bi zavr�ili deinstalaciju %1, Va� ra�unar morate restartati%n%n�elite li to u�initi sada?
UninstallDataCorrupted="%1" datoteka je o�te�ena. Deinstalacija nije mogu�a.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Ukloni dijeljenu datoteku
ConfirmDeleteSharedFile2=Sistem ukazuje da sljede�e dijeljene datoteke ne koristi nijedan drugi program. �elite li da Deinstalacija ukloni te dijeljene datoteke?%n%nAko neki programi i dalje koriste te datoteke, a one se obri�u, ti programi ne�e raditi ispravno. Ako niste sigurni, odaberite Ne. Ostavljanje datoteka ne�e uzrokovati �tetu va�em sistemu.
SharedFileNameLabel=Datoteka:
SharedFileLocationLabel=Putanja:
WizardUninstalling=Status deinstalacije
StatusUninstalling=Deinstaliram %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instaliram %1.
ShutdownBlockReasonUninstallingApp=Deinstaliram %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 verzija %2
AdditionalIcons=Dodatne ikone:
CreateDesktopIcon=Kreiraj &desktop ikonu
CreateQuickLaunchIcon=Kreiraj &Quick Launch ikonu
ProgramOnTheWeb=%1 na webu
UninstallProgram=Deinstaliraj %1
LaunchProgram=Pokreni %1
AssocFileExtension=&Asociraj %1 sa %2 ekstenzijom
AssocingFileExtension=Asociram %1 sa %2 ekstenzijom...
AutoStartProgramGroupDescription=Pokretanje:
AutoStartProgram=Automatski pokre�i %1
AddonHostProgramNotFound=%1 nije mogao biti prona�en u folderu koji ste odabrali.%n%nDa li i dalje �elite nastaviti s ovom akcijom?
