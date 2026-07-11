; *** Inno Setup version 6.5.0+ Serbian (Latin) messages ***
;
; To download user-contributed translations of this file, go to:
;   https://jrsoftware.org/files/istrans/
;
; Maintained by Davor Nikolić (support@trackworktime.com).
; Based on previous translations of Rancher (theranchcowboy@gmail.com)
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Srpski
LanguageID=$081a
; LanguageCodePage should always be set if possible, even if this file is Unicode
; For English it's set to zero anyway because English only uses ASCII characters
LanguageCodePage=1250
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
;DialogFontName=
;DialogFontSize=9
;DialogFontBaseScaleWidth=7
;DialogFontBaseScaleHeight=15
;WelcomeFontName=Segoe UI
;WelcomeFontSize=14

[Messages]

; *** Application titles
SetupAppTitle=Instalacija
SetupWindowTitle=Instalacija – %1
UninstallAppTitle=Deinstalacija
UninstallAppFullTitle=Deinstalacija programa %1

; *** Misc. common
InformationTitle=Informacije
ConfirmTitle=Potvrda
ErrorTitle=Greška

; *** SetupLdr messages
SetupLdrStartupMessage=Ovim će se instalirati %1. Želite li da nastavite?
LdrCannotCreateTemp=Ne mogu da napravim privremenu datoteku. Instalacija je prekinuta
LdrCannotExecTemp=Ne mogu da pokrenem datoteku u privremenoj fascikli. Instalacija je prekinuta
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nGreška %2: %3
SetupFileMissing=Datoteka %1 nedostaje u instalacionoj fascikli. Ispravite problem ili nabavite novi primerak programa.
SetupFileCorrupt=Instalacione datoteke su oštećene. Nabavite novi primerak programa.
SetupFileCorruptOrWrongVer=Instalacione datoteke su oštećene ili nisu kompatibilne s ovom verzijom instalacionog programa. Ispravite problem ili nabavite novi primerak programa.
InvalidParameter=Neispravan parametar je prenet na komandnu liniju:%n%n%1
SetupAlreadyRunning=Instalacioni program je već pokrenut.
WindowsVersionNotSupported=Ovaj program ne podržava verziju Windowsa koju računar koristi.
WindowsServicePackRequired=Program zahteva %1 servisni paket %2 ili noviji.
NotOnThisPlatform=Program neće raditi na %1.
OnlyOnThisPlatform=Ovaj program može da radi samo na %1.
OnlyOnTheseArchitectures=Ovaj program može da se instalira samo na verzijama Windowsa namenjenim sledećim arhitekturama procesora:%n%n%1
WinVersionTooLowError=Ovaj program zahteva %1 verziju %2 ili noviju.
WinVersionTooHighError=Ovaj program ne može da se instalira na %1 verziji %2 ili novijoj.
AdminPrivilegesRequired=Morate biti prijavljeni kao administrator da biste instalirali program.
PowerUserPrivilegesRequired=Morate biti prijavljeni kao administrator ili kao član grupe Power Users da biste instalirali ovaj program.
SetupAppRunningError=Instalacioni program je otkrio da je %1 trenutno pokrenut.%n%nOdmah zatvorite sve njegove instance, zatim kliknite na „U redu“ da nastavite ili na „Otkaži“ da izađete.
UninstallAppRunningError=Program za deinstalaciju je otkrio da je %1 trenutno pokrenut.%n%nOdmah zatvorite sve njegove instance, zatim kliknite na „U redu“ da nastavite ili na „Otkaži“ da izađete.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Odaberite način instalacije
PrivilegesRequiredOverrideInstruction=Odaberite način instalacije
PrivilegesRequiredOverrideText1=%1 može biti instaliran za sve korisnike (zahteva administrativne privilegije) ili samo za vas.
PrivilegesRequiredOverrideText2=%1 može da se instalira samo za vas ili za sve korisnike (zahteva administrativne privilegije).
PrivilegesRequiredOverrideAllUsers=Instaliraj za &sve korisnike
PrivilegesRequiredOverrideAllUsersRecommended=Instaliraj za &sve korisnike (preporučeno)
PrivilegesRequiredOverrideCurrentUser=Instaliraj samo za &mene
PrivilegesRequiredOverrideCurrentUserRecommended=Instaliraj samo za &mene (preporučeno)

; *** Misc. errors
ErrorCreatingDir=Instalacioni program nije mogao da napravi fasciklu „%1“
ErrorTooManyFilesInDir=Ne mogu da napravim datoteku u fascikli „%1“ jer sadrži previše datoteka

; *** Setup common messages
ExitSetupTitle=Napuštanje instalacije
ExitSetupMessage=Instalacija nije završena. Ako sada izađete, program neće biti instaliran.%n%nInstalacioni program možete ponovo pokrenuti nekom drugom prilikom da biste dovršili instalaciju.%n%nŽelite li da izađete iz instalacionog programa?
AboutSetupMenuItem=&O instalacionom programu...
AboutSetupTitle=O instalacionom programu
AboutSetupMessage=%1 verzija %2%n%3%n%nPočetna stranica programa %1:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Nazad
ButtonNext=&Dalje >
ButtonInstall=&Instaliraj
ButtonOK=U redu
ButtonCancel=Otkaži
ButtonYes=&Da
ButtonYesToAll=D&a za sve
ButtonNo=&Ne
ButtonNoToAll=N&e za sve
ButtonFinish=&Završi
ButtonBrowse=&Pretraži...
ButtonWizardBrowse=&Pretraži...
ButtonNewFolder=&Napravi fasciklu

; *** "Select Language" dialog messages
SelectLanguageTitle=Izbor jezika instalacionog programa
SelectLanguageLabel=Izaberite jezik koji će se koristiti tokom instalacije.

; *** Common wizard text
ClickNext=Kliknite na „Dalje“ da nastavite ili „Otkaži“ da napustite instalaciju.
BeveledLabel=
BrowseDialogTitle=Odabir fascikle
BrowseDialogLabel=Izaberite fasciklu sa spiska i kliknite na „U redu“.
NewFolderName=Nova fascikla

; *** "Welcome" wizard page
WelcomeLabel1=Dobro došli u čarobnjak za instalaciju programa [name]
WelcomeLabel2=Instaliraćete [name/ver] na računar.%n%nPre nego što nastavite, preporučujemo vam da zatvorite sve druge programe.

; *** "Password" wizard page
WizardPassword=Lozinka
PasswordLabel1=Instalacija je zaštićena lozinkom.
PasswordLabel3=Unesite lozinku i kliknite na „Dalje“ da nastavite. Imajte na umu da je lozinka osetljiva na mala i velika slova.
PasswordEditLabel=&Lozinka:
IncorrectPassword=Navedena lozinka nije ispravna. Pokušajte ponovo.

; *** "License Agreement" wizard
WizardLicense=Ugovor o licenci
LicenseLabel=Pažljivo pročitajte sledeće pre nego što nastavite.
LicenseLabel3=Pročitajte sledeći Ugovor o licenci. Morate prihvatiti uslove ovog ugovora pre nego što nastavite s instalacijom.
LicenseAccepted=&Prihvatam ugovor
LicenseNotAccepted=&Ne prihvatam ugovor

; *** "Information" wizard pages
WizardInfoBefore=Informacije
InfoBeforeLabel=Pažljivo pročitajte sledeće pre nego što nastavite.
InfoBeforeClickLabel=Kada budete spremni da nastavite instalaciju, kliknite na „Dalje“.
WizardInfoAfter=Informacije
InfoAfterLabel=Pažljivo pročitajte sledeće pre nego što nastavite.
InfoAfterClickLabel=Kada budete spremni da nastavite instalaciju, kliknite na „Dalje“.

; *** "User Information" wizard page
WizardUserInfo=Korisnički podaci
UserInfoDesc=Unesite svoje podatke.
UserInfoName=&Ime korisnika:
UserInfoOrg=&Organizacija:
UserInfoSerial=&Serijski broj:
UserInfoNameRequired=Morate navesti ime.

; *** "Select Destination Location" wizard page
WizardSelectDir=Odabir odredišne fascikle
SelectDirDesc=Gde želite da se instalira [name]?
SelectDirLabel3=Instalacioni program će instalirati [name] u sledeću fasciklu.
SelectDirBrowseLabel=Kliknite na „Dalje“ da nastavite. Ako želite da izaberete drugu fasciklu, kliknite na „Pretraži...“.
DiskSpaceGBLabel=Potrebno je najmanje [gb] GB slobodnog prostora na disku.
DiskSpaceMBLabel=Potrebno je najmanje [mb] MB slobodnog prostora na disku.
CannotInstallToNetworkDrive=Instalacioni program ne može da izvrši instalaciju na mrežnu jedinicu.
CannotInstallToUNCPath=Instalacioni program ne može da izvrši instalaciju na UNC putanju.
InvalidPath=Morate uneti punu putanju sa slovom disk jedinice; na primer:%n%nC:\APP%n%nili UNC putanju u obliku:%n%n\\server\share
InvalidDrive=Disk jedinica ili UNC deljenje koje ste izabrali ne postoji ili nije dostupno. Izaberite drugo.
DiskSpaceWarningTitle=Nedovoljno prostora na disku
DiskSpaceWarning=Instalacioni program zahteva najmanje %1 kB slobodnog prostora za instalaciju, ali izabrani disk ima samo %2 kB slobodnog prostora.%n%nŽelite li ipak da nastavite?
DirNameTooLong=Naziv fascikle ili putanja je predugačka.
InvalidDirName=Naziv fascikle nije ispravan.
BadDirName32=Naziv fascikle ne sme da sadrži nijedan od sledećih znakova:%n%n%1
DirExistsTitle=Fascikla već postoji
DirExists=Fascikla:%n%n%1%n%nveć postoji. Želite li ipak da instalirate program u nju?
DirDoesntExistTitle=Fascikla ne postoji
DirDoesntExist=Fascikla:%n%n%1%n%nne postoji. Želite li da je napravite?

; *** "Select Components" wizard page
WizardSelectComponents=Odabir komponenata
SelectComponentsDesc=Koje komponente želite da instalirate?
SelectComponentsLabel2=Izaberite komponente koje želite da instalirate, a poništite izbor onih koje ne želite da instalirate. Kliknite na „Dalje“ kada budete spremni da nastavite.
FullInstallation=Puna instalacija
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompaktna instalacija
CustomInstallation=Prilagođena instalacija
NoUninstallWarningTitle=Komponente već postoje
NoUninstallWarning=Instalacioni program je otkrio da su sledeće komponente već instalirane na računaru:%n%n%1%n%nPoništavanje izbora ovih komponenti neće ih deinstalirati.%n%nŽelite li ipak da nastavite?
ComponentSize1=%1 kB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Izabrane stavke zahtevaju najmanje [gb] GB slobodnog prostora.
ComponentsDiskSpaceMBLabel=Izabrane stavke zahtevaju najmanje [mb] MB slobodnog prostora.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Odabir dodatnih zadataka
SelectTasksDesc=Koje dodatne zadatke treba izvršiti?
SelectTasksLabel2=Izaberite dodatne zadatke koje želite da instalacioni program izvrši tokom instalacije programa [name], zatim kliknite na „Dalje“.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Odabir fascikle u meniju „Start“
SelectStartMenuFolderDesc=Gde instalacioni program treba da postavi prečice programa?
SelectStartMenuFolderLabel3=Instalacioni program će napraviti prečice programa u sledećoj fascikli menija „Start“.
SelectStartMenuFolderBrowseLabel=Kliknite na „Dalje“ da nastavite. Ako želite da izaberete drugu fasciklu, kliknite na „Pretraži...“.
MustEnterGroupName=Morate navesti naziv fascikle.
GroupNameTooLong=Naziv fascikle ili putanja je predugačka.
InvalidGroupName=Naziv fascikle nije ispravan.
BadGroupName=Naziv fascikle ne sme da sadrži nijedan od sledećih znakova:%n%n%1
NoProgramGroupCheck2=N&e pravi fasciklu u meniju „Start“

; *** "Ready to Install" wizard page
WizardReady=Spremno za instalaciju
ReadyLabel1=Instalacioni program je sada spreman da započne instaliranje programa [name] na računar.
ReadyLabel2a=Kliknite na „Instaliraj“ da nastavite s instalacijom ili na „Nazad“ ako želite da pregledate ili promenite postavke.
ReadyLabel2b=Kliknite na „Instaliraj“ da nastavite s instalacijom.
ReadyMemoUserInfo=Korisnički podaci:
ReadyMemoDir=Odredišna fascikla:
ReadyMemoType=Vrsta instalacije:
ReadyMemoComponents=Izabrane komponente:
ReadyMemoGroup=Fascikla u meniju „Start“:
ReadyMemoTasks=Dodatni zadaci:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Preuzimanje datoteka...
ButtonStopDownload=&Zaustavi preuzimanje
StopDownload=Da li ste sigurni da želite da zaustavite preuzimanje?
ErrorDownloadAborted=Preuzimanje je prekinuto
ErrorDownloadFailed=Preuzimanje nije uspelo: %1 %2
ErrorDownloadSizeFailed=Dobijanje veličine nije uspelo: %1 %2
ErrorProgress=Neispravan napredak: %1 od %2
ErrorFileSize=Neispravna veličina datoteke: očekivana %1, pronađena %2

; *** TExtractionWizardPage wizard page and Extract7ZipArchive
ExtractingLabel=Raspakivanje datoteka...
ButtonStopExtraction=&Zaustavi raspakivanje
StopExtraction=Da li ste sigurni da želite da zaustavite raspakivanje?
ErrorExtractionAborted=Raspakivanje je prekinuto
ErrorExtractionFailed=Raspakivanje nije uspelo: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=Lozinka je netačna
ArchiveIsCorrupted=Arhiva je oštećena
ArchiveUnsupportedFormat=Format arhive nije podržan

; *** "Preparing to Install" wizard page
WizardPreparing=Priprema za instalaciju
PreparingDesc=Instalacioni program se priprema da instalira [name] na računar.
PreviousInstallNotCompleted=Instalacija ili uklanjanje prethodnog programa nije završeno. Morate ponovo pokrenuti računar da biste dovršili tu instalaciju.%n%nNakon ponovnog pokretanja računara ponovo pokrenite instalacioni program da biste dovršili instalaciju programa [name].
CannotContinue=Instalacioni program ne može da nastavi. Kliknite na „Otkaži“ da izađete.
ApplicationsFound=Sledeće aplikacije koriste datoteke koje instalacioni program treba da ažurira. Preporučuje se da dozvolite instalacionom programu da automatski zatvori ove aplikacije.
ApplicationsFound2=Sledeće aplikacije koriste datoteke koje instalacioni program treba da ažurira. Preporučuje se da dozvolite instalacionom programu da automatski zatvori ove aplikacije. Kada se instalacija završi, instalacioni program će pokušati da ponovo pokrene aplikacije.
CloseApplications=&Automatski zatvori aplikacije
DontCloseApplications=&Ne zatvaraj aplikacije
ErrorCloseApplications=Instalacioni program nije mogao automatski da zatvori sve aplikacije. Preporučuje se da pre nastavka zatvorite sve aplikacije koje koriste datoteke koje instalacioni program treba da ažurira.
PrepareToInstallNeedsRestart=Instalacioni program mora ponovo da pokrene računar. Nakon ponovnog pokretanja računara ponovo pokrenite instalacioni program da biste dovršili instalaciju programa [name].%n%nŽelite li sada da ponovo pokrenete računar?

; *** "Installing" wizard page
WizardInstalling=Instaliranje
InstallingLabel=Sačekajte dok instalacioni program instalira [name] na računar.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Dovršavanje čarobnjaka za instalaciju programa [name]
FinishedLabelNoIcons=Instalacioni program je završio instaliranje programa [name] na računar.
FinishedLabel=Instalacioni program je završio instaliranje programa [name] na računar. Aplikaciju možete pokrenuti izborom instaliranih prečica.
ClickFinish=Kliknite na „Završi“ da izađete iz instalacionog programa.
FinishedRestartLabel=Da bi se dovršila instalacija programa [name], instalacioni program mora ponovo da pokrene računar. Želite li sada da ga ponovo pokrenete?
FinishedRestartMessage=Da bi se dovršila instalacija programa [name], instalacioni program mora ponovo da pokrene računar.%n%nŽelite li sada da ga ponovo pokrenete?
ShowReadmeCheck=Da, želim da pogledam datoteku README
YesRadio=&Da, ponovo pokreni računar sada
NoRadio=&Ne, ponovo ću pokrenuti računar kasnije
; used for example as 'Run MyProg.exe'
RunEntryExec=Pokreni %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Pogledaj %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Instalacionom programu je potreban sledeći disk
SelectDiskLabel2=Ubacite disk %1 i kliknite na „U redu“.%n%nAko se datoteke na ovom disku mogu pronaći u nekoj drugoj fascikli, unesite odgovarajuću putanju ili kliknite na „Pretraži...“.
PathLabel=&Putanja:
FileNotInDir2=Datoteka „%1“ se ne nalazi u „%2“. Ubacite pravi disk ili izaberite drugu fasciklu.
SelectDirectoryLabel=Navedite lokaciju sledećeg diska.

; *** Installation phase messages
SetupAborted=Instalacija nije završena.%n%nIspravite problem i ponovo pokrenite instalacioni program.
AbortRetryIgnoreSelectAction=Odaberite radnju
AbortRetryIgnoreRetry=&Pokušaj opet
AbortRetryIgnoreIgnore=&Zanemari grešku i nastavi
AbortRetryIgnoreCancel=Prekini instalaciju
RetryCancelSelectAction=Odaberite radnju
RetryCancelRetry=&Pokušaj ponovo
RetryCancelCancel=Otkaži

; *** Installation status messages
StatusClosingApplications=Zatvaranje aplikacija...
StatusCreateDirs=Pravljenje fascikli...
StatusExtractFiles=Raspakivanje datoteka...
StatusDownloadFiles=Preuzimanje datoteka...
StatusCreateIcons=Pravljenje prečica...
StatusCreateIniEntries=Pravljenje INI unosa...
StatusCreateRegistryEntries=Pravljenje unosa u registru...
StatusRegisterFiles=Registrovanje datoteka...
StatusSavingUninstall=Čuvanje podataka za deinstalaciju...
StatusRunProgram=Dovršavanje instalacije...
StatusRestartingApplications=Ponovno pokretanje aplikacija...
StatusRollback=Vraćanje izmena...

; *** Misc. errors
ErrorInternal2=Unutrašnja greška: %1
ErrorFunctionFailedNoCode=%1 nije uspelo
ErrorFunctionFailed=%1 nije uspelo; kôd %2
ErrorFunctionFailedWithMessage=%1 nije uspelo; kôd %2.%n%3
ErrorExecutingProgram=Ne mogu da pokrenem datoteku:%n%1

; *** Registry errors
ErrorRegOpenKey=Greška pri otvaranju ključa registra:%n%1\%2
ErrorRegCreateKey=Greška pri stvaranju ključa registra:%n%1\%2
ErrorRegWriteKey=Greška pri upisivanju u ključ registra:%n%1\%2

; *** INI errors
ErrorIniEntry=Greška pri stvaranju INI unosa u datoteci „%1“.

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Preskočite ovu datoteku (ne preporučuje se)
FileAbortRetryIgnoreIgnoreNotRecommended=&Zanemarite grešku i nastavite (ne preporučuje se)
SourceIsCorrupted=Izvorna datoteka je oštećena
SourceDoesntExist=Izvorna datoteka „%1“ ne postoji
SourceVerificationFailed=Verifikacija izvorne datoteke nije uspela: %1
VerificationSignatureDoesntExist=Datoteka potpisa „%1“ ne postoji
VerificationSignatureInvalid=Datoteka potpisa „%1“ je nevažeća
VerificationKeyNotFound=Datoteka potpisa „%1“ koristi nepoznat ključ
VerificationFileNameIncorrect=Naziv datoteke nije tačan
VerificationFileTagIncorrect=Oznaka datoteke nije tačna
VerificationFileSizeIncorrect=Veličina datoteke nije tačna
VerificationFileHashIncorrect=Heš datoteke nije tačan
ExistingFileReadOnly2=Postojeća datoteka ne može da se zameni jer je samo za čitanje.
ExistingFileReadOnlyRetry=&Uklonite atribut samo za čitanje i pokušajte ponovo
ExistingFileReadOnlyKeepExisting=&Zadržite postojeću datoteku
ErrorReadingExistingDest=Došlo je do greške pri pokušaju čitanja postojeće datoteke:
FileExistsSelectAction=Odaberite radnju
FileExists2=Datoteka već postoji.
FileExistsOverwriteExisting=&Zamenite postojeću datoteku
FileExistsKeepExisting=&Zadržite postojeću datoteku
FileExistsOverwriteOrKeepAll=&Uradite ovo i ubuduće
ExistingFileNewerSelectAction=Odaberite radnju
ExistingFileNewer2=Postojeća datoteka je novija od one koju treba instalirati.
ExistingFileNewerOverwriteExisting=&Zamenite postojeću datoteku
ExistingFileNewerKeepExisting=&Zadržite postojeću datoteku (preporučeno)
ExistingFileNewerOverwriteOrKeepAll=&Uradite ovo i ubuduće
ErrorChangingAttr=Došlo je do greške pri pokušaju promene atributa postojeće datoteke:
ErrorCreatingTemp=Došlo je do greške pri stvaranju datoteke u odredišnoj fascikli:
ErrorReadingSource=Došlo je do greške pri čitanju izvorne datoteke:
ErrorCopying=Došlo je do greške pri pokušaju kopiranja datoteke:
ErrorDownloading=Došlo je do greške pri pokušaju preuzimanja datoteke:
ErrorExtracting=Došlo je do greške pri pokušaju raspakivanja arhive:
ErrorReplacingExistingFile=Došlo je do greške pri zameni postojeće datoteke:
ErrorRestartReplace=RestartReplace nije uspelo:
ErrorRenamingTemp=Došlo je do greške pri preimenovanju datoteke u odredišnoj fascikli:
ErrorRegisterServer=Ne mogu da registrujem DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 nije uspeo; izlazni kôd %1
ErrorRegisterTypeLib=Ne mogu da registrujem biblioteku tipova: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=Svi korisnici
UninstallDisplayNameMarkCurrentUser=Trenutni korisnik

; *** Post-installation errors
ErrorOpeningReadme=Došlo je do greške pri otvaranju datoteke README.
ErrorRestartingComputer=Instalacioni program nije mogao ponovo da pokrene računar. Uradite to ručno.

; *** Uninstaller messages
UninstallNotFound=Datoteka „%1“ ne postoji. Ne mogu da deinstaliram program.
UninstallOpenError=Datoteka „%1“ ne može da se otvori. Ne mogu da deinstaliram program
UninstallUnsupportedVer=Datoteka evidencije deinstalacije „%1“ ima format koji ova verzija programa za deinstalaciju ne prepoznaje. Deinstalacija nije moguća
UninstallUnknownEntry=U evidenciji deinstalacije pronađen je nepoznat unos (%1)
ConfirmUninstall=Želite li da potpuno uklonite %1 i sve njegove komponente?
UninstallOnlyOnWin64=Ova instalacija može da se deinstalira samo na 64-bitnom Windowsu.
OnlyAdminCanUninstall=Ovu instalaciju može da deinstalira samo korisnik s administratorskim pravima.
UninstallStatusLabel=Sačekajte da se %1 deinstalira sa računara.
UninstalledAll=%1 je uspešno uklonjen sa računara.
UninstalledMost=Deinstalacija programa %1 je završena.%n%nNeke stavke nisu mogle da se uklone. Možete ih ukloniti ručno.
UninstalledAndNeedsRestart=Da bi se završila deinstalacija programa %1, potrebno je ponovo pokrenuti računar.%n%nŽelite li sada da ga ponovo pokrenete?
UninstallDataCorrupted=Datoteka „%1“ je oštećena. Ne mogu da deinstaliram program

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Ukloniti deljenu datoteku?
ConfirmDeleteSharedFile2=Sistem ukazuje na to da sledeću deljenu datoteku više ne koristi nijedan program. Želite li da je program za deinstalaciju ukloni?%n%nAko neki programi i dalje koriste ovu datoteku, a ona se ukloni, ti programi možda neće ispravno raditi. Ako niste sigurni, izaberite „Ne“. Ostavljanje datoteke na sistemu neće prouzrokovati nikakvu štetu.
SharedFileNameLabel=Naziv datoteke:
SharedFileLocationLabel=Lokacija:
WizardUninstalling=Stanje deinstalacije
StatusUninstalling=Deinstaliranje programa %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instaliram %1.
ShutdownBlockReasonUninstallingApp=Deinstaliram %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 verzija %2
AdditionalIcons=Dodatne prečice:
CreateDesktopIcon=Napravi &prečicu na radnoj površini
CreateQuickLaunchIcon=Napravi prečicu na traci za &brzo pokretanje
ProgramOnTheWeb=%1 na internetu
UninstallProgram=Deinstaliraj %1
LaunchProgram=Pokreni %1
AssocFileExtension=&Poveži %1 sa ekstenzijom datoteke %2
AssocingFileExtension=Povezivanje programa %1 sa ekstenzijom datoteke %2...
AutoStartProgramGroupDescription=Pokretanje:
AutoStartProgram=Automatski pokreni %1
AddonHostProgramNotFound=%1 se ne nalazi u izabranoj fascikli.%n%nŽelite li ipak da nastavite?
