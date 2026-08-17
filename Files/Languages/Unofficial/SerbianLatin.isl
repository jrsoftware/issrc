; *** Inno Setup version 6.5.0+ Serbian (Latin) messages ***
;
; To download user-contributed translations of this file, go to:
;   https://jrsoftware.org/files/istrans/
;
; Maintained by Davor Nikolić (support@trackworktime.com).
; Based on previous translations of Rancher (theranchcowboy@gmail.com)
;
; Note: When translating this text:
; - All translations should back-translate to the same meaning.
; - Do not add to or change the meaning of messages to suit your personal taste.
; - Do not add or remove sentences, or add or omit information within sentences.
; - Do not remove these words: all, only, automatically, now, later, may, must, and not.
; - Do not add periods (.) or colons (:) or ellipses (...) to the end of messages that didn't have them already.
;   Exception: for languages with their own period character, such as Japanese and Chinese, it was added
;   to the end of messages as needed.
; - Do not remove periods or colons or ellipses or question marks from the end of messages.
;   Exception: Thai and Lao remove trailing periods and question marks.
; - Do not replace periods with colons. Replacing '...' with '…' is allowed.
; - Do not add or remove number placeholders (%1, %2, etc.). Changing the order is allowed.
; - Do not add or remove named placeholders ([name], [name/ver], etc.). Do not replace one with another.
; - Do not add or remove line breaks (%n).
; - Do not add accelerators (&) or create collisions. Remove an accelerator only if it cannot be moved to another letter.
; - Do not add new custom messages to the [CustomMessages] section.
; - Do not translate comments like these.
; - Keep AboutSetupNote empty.
; Before you start, decide how you will translate each of these recurring terms:
; - Setup, Uninstall (the program), uninstall (the verb), Cannot uninstall,
;   program, application, component, task,
;   shortcut, Start Menu, folder, directory, path, location, drive,
;   existing file, source file, registry, INI entries, README,
;   computer, Windows, version, administrator, all users, current user,
;   Downloading files, Extracting files, aborted, corrupted, close (applications), restart,
;   Select action, try again, anyway, at least,
;   and the button captions OK, Cancel, Yes, No, Next, Back, Install, Browse, Finish.
; - When in doubt, use Microsoft Terminology Search from:
;   https://msit.powerbi.com/view?r=eyJrIjoiODJmYjU4Y2YtM2M0ZC00YzYxLWE1YTktNzFjYmYxNTAxNjQ0IiwidCI6IjcyZjk4OGJmLTg2ZjEtNDFhZi05MWFiLTJkN2NkMDExZGI0NyIsImMiOjV9
; Removing this section from your translation is allowed, but do not disregard it.

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
SetupWindowTitle=Instalacija - %1
UninstallAppTitle=Deinstalacija
UninstallAppFullTitle=Deinstalacija programa %1

; *** Misc. common
InformationTitle=Informacije
ConfirmTitle=Potvrda
ErrorTitle=Greška

; *** SetupLdr messages
SetupLdrStartupMessage=Program %1 će biti instaliran. Želite li da nastavite?
LdrCannotCreateTemp=Nije moguće napraviti privremenu datoteku. Instalacija je prekinuta
LdrCannotExecTemp=Nije moguće pokrenuti datoteku u privremenoj fascikli. Instalacija je prekinuta
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nGreška %2: %3
SetupFileMissing=Datoteka %1 nedostaje u instalacionoj fascikli. Ispravite problem ili nabavite novi primerak programa.
SetupFileCorrupt=Instalacione datoteke su oštećene. Nabavite novi primerak programa.
SetupFileCorruptOrWrongVer=Instalacione datoteke su oštećene ili nisu kompatibilne s ovom verzijom instalacije. Ispravite problem ili nabavite novi primerak programa.
InvalidParameter=Neispravan parametar je prosleđen u komandnoj liniji:%n%n%1
SetupAlreadyRunning=Instalacija je već pokrenuta.
WindowsVersionNotSupported=Program ne podržava verziju Windows-a koju koristite.
WindowsServicePackRequired=Program zahteva %1 servisni paket %2 ili noviji.
NotOnThisPlatform=Program neće raditi na %1.
OnlyOnThisPlatform=Program mora da se pokrene na %1.
OnlyOnTheseArchitectures=Program se može instalirati samo na verzijama Windows-a namenjenim za sledeće arhitekture procesora:%n%n%1
WinVersionTooLowError=Program zahteva %1 verziju %2 ili noviju.
WinVersionTooHighError=Program nije moguće instalirati na %1 verziju %2 ili noviju.
AdminPrivilegesRequired=Morate biti prijavljeni kao administrator da biste instalirali program.
; 'Power Users group' is an outdated term but should still be translated, not dropped or modernized
PowerUserPrivilegesRequired=Morate biti prijavljeni kao administrator ili kao član grupe „Power Users“ da biste instalirali ovaj program.
; 'instance' may also be translated as 'copy'
SetupAppRunningError=Instalacioni program je utvrdio da je program %1 trenutno pokrenut.%n%nZatvorite ga i kliknite na dugme „U redu“ da nastavite ili „Otkaži“ da napustite instalaciju.
UninstallAppRunningError=Deinstalacioni program je utvrdio da je program %1 trenutno pokrenut.%n%nZatvorite ga i kliknite na dugme „U redu“ da nastavite ili „Otkaži“ da napustite deinstalaciju.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Odaberite način instalacije programa
PrivilegesRequiredOverrideInstruction=Odaberite način instalacije
PrivilegesRequiredOverrideText1=%1 može da se instalira za sve korisnike (zahteva administratorske privilegije) ili samo za vas.
PrivilegesRequiredOverrideText2=%1 može da se instalira samo za vas ili za sve korisnike (zahteva administratorske privilegije).
PrivilegesRequiredOverrideAllUsers=Instaliraj za &sve korisnike
PrivilegesRequiredOverrideAllUsersRecommended=Instaliraj za &sve korisnike (preporučeno)
PrivilegesRequiredOverrideCurrentUser=Instaliraj samo za &mene
PrivilegesRequiredOverrideCurrentUserRecommended=Instaliraj samo za &mene (preporučeno)

; *** Misc. errors
ErrorCreatingDir=Nije moguće napraviti fasciklu „%1“
ErrorTooManyFilesInDir=Nije moguće napraviti datoteku u fascikli „%1“ jer sadrži previše datoteka

; *** Setup common messages
ExitSetupTitle=Napuštanje instalacije
ExitSetupMessage=Instalacija nije završena. Ako sada izađete, program neće biti instaliran.%n%nInstalaciju možete ponovo pokrenuti i dovršiti nekom drugom prilikom.%n%nŽelite li da izađete iz instalacije?
AboutSetupMenuItem=&O instalacionom programu...
AboutSetupTitle=Podaci o instalacionom programu
AboutSetupMessage=%1 %2%n%3%n%nPočetna stranica programa %1:%n%4
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
ButtonBrowse=&Izaberi...
ButtonWizardBrowse=I&zaberi...
ButtonNewFolder=&Napravi novu fasciklu

; *** "Select Language" dialog messages
SelectLanguageTitle=Odabir jezika instalacije
SelectLanguageLabel=Izaberite jezik koji će se koristiti tokom instalacije.

; *** Common wizard text
ClickNext=Kliknite na „Dalje“ da nastavite ili „Otkaži“ da napustite instalaciju.
BeveledLabel=
BrowseDialogTitle=Odabir fascikle
BrowseDialogLabel=Izaberite fasciklu sa spiska ispod, a zatim kliknite na „U redu“.
NewFolderName=Nova fascikla

; *** "Welcome" wizard page
WelcomeLabel1=Dobro došli u čarobnjak za instalaciju programa [name]
WelcomeLabel2=Program [name/ver] će biti instaliran na računar.%n%nPre nego što nastavite, preporučujemo vam da zatvorite sve druge programe.

; *** "Password" wizard page
WizardPassword=Lozinka
PasswordLabel1=Instalacija je zaštićena lozinkom.
PasswordLabel3=Unesite lozinku i kliknite na „Dalje“ da nastavite. Lozinka je osetljiva na mala i velika slova.
PasswordEditLabel=&Lozinka:
IncorrectPassword=Navedena lozinka nije ispravna. Pokušajte ponovo.

; *** "License Agreement" wizard page
WizardLicense=Ugovor o licenci
LicenseLabel=Pročitajte sledeće važne informacije pre nego što nastavite.
LicenseLabel3=Pročitajte Ugovor o licenci koji se nalazi ispod. Morate prihvatiti uslove ovog ugovora pre nego što nastavite sa instalacijom.
LicenseAccepted=&Prihvatam ugovor
LicenseNotAccepted=N&e prihvatam ugovor

; *** "Information" wizard pages
WizardInfoBefore=Informacije
InfoBeforeLabel=Pročitajte sledeće važne informacije pre nego što nastavite.
InfoBeforeClickLabel=Kada budete spremni da nastavite instalaciju, kliknite na „Dalje“.
WizardInfoAfter=Informacije
InfoAfterLabel=Pročitajte sledeće važne informacije pre nego što nastavite.
InfoAfterClickLabel=Kada budete spremni da nastavite instalaciju, kliknite na „Dalje“.

; *** "User Information" wizard page
WizardUserInfo=Korisnički podaci
UserInfoDesc=Unesite svoje podatke.
UserInfoName=&Korisničko ime:
UserInfoOrg=&Organizacija:
UserInfoSerial=&Serijski broj:
UserInfoNameRequired=Morate navesti ime.

; *** "Select Destination Location" wizard page
WizardSelectDir=Odabir odredišne fascikle
SelectDirDesc=Gde treba instalirati [name]?
SelectDirLabel3=Program [name] će biti instaliran u sledeću fasciklu.
SelectDirBrowseLabel=Kliknite na „Dalje“ da nastavite. Ako želite da izaberete drugu fasciklu, kliknite na „Izaberi...“.
DiskSpaceGBLabel=Potrebno je najmanje [gb] GB slobodnog prostora na disku.
DiskSpaceMBLabel=Potrebno je najmanje [mb] MB slobodnog prostora na disku.
CannotInstallToNetworkDrive=Nije moguće instalirati program na mrežni disk.
CannotInstallToUNCPath=Nije moguće instalirati program na UNC putanju.
InvalidPath=Morate navesti punu putanju sa slovom diska; npr.:%n%nC:\APP%n%nili UNC putanju u obliku:%n%n\\server\share
InvalidDrive=Izabrani disk ili UNC deljeni resurs ne postoji ili nije dostupan. Izaberite drugi.
DiskSpaceWarningTitle=Nedovoljno prostora na disku
DiskSpaceWarning=Za instalaciju je potrebno najmanje %1 KB slobodnog prostora, a izabrani disk na raspolaganju ima samo %2 KB.%n%nŽelite li ipak da nastavite?
DirNameTooLong=Naziv fascikle ili putanja je predugačka.
InvalidDirName=Naziv fascikle nije ispravan.
BadDirName32=Naziv fascikle ne sme sadržati nijedan od sledećih znakova:%n%n%1
DirExistsTitle=Fascikla već postoji
DirExists=Fascikla:%n%n%1%n%nveć postoji. Želite li ipak da instalirate program u nju?
DirDoesntExistTitle=Fascikla ne postoji
DirDoesntExist=Fascikla:%n%n%1%n%nne postoji. Želite li da se napravi?

; *** "Select Components" wizard page
WizardSelectComponents=Odabir komponenata
SelectComponentsDesc=Koje komponente treba instalirati?
SelectComponentsLabel2=Izaberite komponente koje želite da instalirate, a poništite izbor onih koje ne želite. Kliknite na „Dalje“ kada budete spremni da nastavite.
; don't translate 'Full' as 'Normal' or 'Default'
FullInstallation=Puna instalacija
; don't translate 'Compact' as 'Minimal' or 'Default'
CompactInstallation=Kompaktna instalacija
CustomInstallation=Prilagođena instalacija
NoUninstallWarningTitle=Komponente već postoje
NoUninstallWarning=Instalacioni program je otkrio da su sledeće komponente već instalirane na računaru:%n%n%1%n%nPoništavanje izbora ovih komponenti ih neće ukloniti.%n%nŽelite li ipak da nastavite?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Izabrane stavke zahtevaju najmanje [gb] GB prostora na disku.
ComponentsDiskSpaceMBLabel=Izabrane stavke zahtevaju najmanje [mb] MB prostora na disku.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Odabir dodatnih zadataka
SelectTasksDesc=Koje dodatne zadatke treba izvršiti?
SelectTasksLabel2=Izaberite dodatne zadatke koje želite da se izvrše pri instaliranju programa [name] i kliknite na „Dalje“.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Odabir fascikle u meniju „Start“
SelectStartMenuFolderDesc=Gde treba postaviti prečice programa?
SelectStartMenuFolderLabel3=Prečice programa će biti postavljene u sledeću fasciklu u meniju „Start“.
SelectStartMenuFolderBrowseLabel=Kliknite na „Dalje“ da nastavite. Ako želite da izaberete drugu fasciklu, kliknite na „Izaberi...“.
MustEnterGroupName=Morate navesti naziv fascikle.
GroupNameTooLong=Naziv fascikle ili putanja je predugačka.
InvalidGroupName=Naziv fascikle nije ispravan.
BadGroupName=Naziv fascikle ne sme sadržati nijedan od sledećih znakova:%n%n%1
NoProgramGroupCheck2=N&e pravi fasciklu u meniju „Start“

; *** "Ready to Install" wizard page
WizardReady=Instalacija je spremna
ReadyLabel1=Program [name] je sada spreman za instalaciju na računar.
ReadyLabel2a=Kliknite na „Instaliraj“ da nastavite sa instalacijom ili na „Nazad“ ako želite da pregledate ili promenite bilo koje postavke.
ReadyLabel2b=Kliknite na „Instaliraj“ da nastavite sa instalacijom.
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
ErrorDownloadSizeFailed=Nije moguće utvrditi veličinu: %1 %2
ErrorProgress=Neispravna vrednost napretka: %1 od %2
ErrorFileSize=Neispravna veličina datoteke: očekivano %1, pronađeno %2

; *** TExtractionWizardPage wizard page and ExtractArchive
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
PreparingDesc=Priprema se instalacija programa [name] na računar.
PreviousInstallNotCompleted=Instalacija ili deinstalacija prethodnog programa nije završena. Potrebno je da ponovo pokrenete računar da bi se ta instalacija završila.%n%nNakon ponovnog pokretanja računara, ponovo pokrenite instalaciju da biste dovršili instaliranje programa [name].
CannotContinue=Nije moguće nastaviti instalaciju. Kliknite na „Otkaži“ da izađete.
ApplicationsFound=Sledeći programi koriste datoteke koje treba da ažurira instalacioni program. Preporučujemo vam da dozvolite instalacionom programu da automatski zatvori ove programe.
ApplicationsFound2=Sledeći programi koriste datoteke koje treba da ažurira instalacioni program. Preporučujemo vam da dozvolite instalacionom programu da automatski zatvori ove programe. Nakon što se instalacija završi, instalacioni program će pokušati da ponovo pokrene programe.
CloseApplications=&Automatski zatvori programe
DontCloseApplications=Ne &zatvaraj programe
ErrorCloseApplications=Instalacioni program nije mogao automatski da zatvori sve programe. Pre nego što nastavite, preporučujemo vam da zatvorite sve programe koji koriste datoteke koje treba da ažurira instalacioni program.
PrepareToInstallNeedsRestart=Potrebno je ponovo pokrenuti računar. Nakon ponovnog pokretanja računara, ponovo pokrenite instalaciju da biste dovršili instaliranje programa [name].%n%nŽelite li da sada ponovo pokrenete računar?

; *** "Installing" wizard page
WizardInstalling=Instaliranje
InstallingLabel=Sačekajte da se [name] instalira na računar.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Završetak čarobnjaka za instalaciju programa [name]
FinishedLabelNoIcons=Instaliranje programa [name] na računar je završeno.
FinishedLabel=Instaliranje programa [name] na računar je završeno. Možete ga pokrenuti preko postavljenih prečica.
ClickFinish=Kliknite na „Završi“ da izađete iz instalacije.
FinishedRestartLabel=Da bi se završila instalacija programa [name], potrebno je ponovo pokrenuti računar. Želite li da ga sada ponovo pokrenete?
FinishedRestartMessage=Da bi se završila instalacija programa [name], potrebno je ponovo pokrenuti računar.%n%nŽelite li da ga sada ponovo pokrenete?
ShowReadmeCheck=Da, želim da pogledam README datoteku
YesRadio=&Da, sada ponovo pokreni računar
NoRadio=Ne, &kasnije ću ponovo pokrenuti računar
; used for example as 'Run MyProg.exe'
RunEntryExec=Pokreni %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Pogledaj %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Instalacionom programu je potreban sledeći disk
SelectDiskLabel2=Ubacite disk %1 i kliknite na „U redu“.%n%nAko se datoteke na ovom disku mogu pronaći u fascikli koja nije prikazana ispod, unesite ispravnu putanju ili kliknite na „Izaberi...“.
PathLabel=Pu&tanja:
FileNotInDir2=Datoteka „%1“ se ne nalazi u „%2“. Ubacite odgovarajući disk ili izaberite drugu fasciklu.
SelectDirectoryLabel=Izaberite putanju do sledećeg diska.

; *** Installation phase messages
SetupAborted=Instalacija nije završena.%n%nIspravite problem i pokrenite je ponovo.
AbortRetryIgnoreSelectAction=Odaberite radnju
AbortRetryIgnoreRetry=&Pokušajte ponovo
AbortRetryIgnoreIgnore=&Zanemarite grešku i nastavite
AbortRetryIgnoreCancel=Otkažite instalaciju
RetryCancelSelectAction=Odaberite radnju
RetryCancelRetry=&Pokušajte ponovo
RetryCancelCancel=Otkaži

; *** Installation status messages
StatusClosingApplications=Zatvaranje programa...
StatusCreateDirs=Pravljenje fascikli...
StatusExtractFiles=Raspakivanje datoteka...
StatusDownloadFiles=Preuzimanje datoteka...
StatusCreateIcons=Postavljanje prečica...
StatusCreateIniEntries=Postavljanje INI unosa...
StatusCreateRegistryEntries=Postavljanje unosa u registar...
StatusRegisterFiles=Registrovanje datoteka...
StatusSavingUninstall=Čuvanje podataka o deinstalaciji...
StatusRunProgram=Završavanje instalacije...
StatusRestartingApplications=Ponovno pokretanje programa...
StatusRollback=Poništavanje izmena...

; *** Misc. errors
ErrorInternal2=Unutrašnja greška: %1
ErrorFunctionFailedNoCode=%1: neuspešno
ErrorFunctionFailed=%1: neuspešno; kod %2
ErrorFunctionFailedWithMessage=%1: neuspešno; kod %2.%n%3
ErrorExecutingProgram=Nije moguće pokrenuti datoteku:%n%1

; *** Registry errors
ErrorRegOpenKey=Greška pri otvaranju ključa u registru:%n%1\%2
ErrorRegCreateKey=Greška pri pravljenju ključa u registru:%n%1\%2
ErrorRegWriteKey=Greška pri upisivanju u ključ registra:%n%1\%2

; *** INI errors
ErrorIniEntry=Greška pri pravljenju INI unosa u datoteci „%1“.

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=Pre&skočite ovu datoteku (ne preporučuje se)
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
ExistingFileReadOnly2=Postojeća datoteka ne može da se zameni jer je označena kao „samo za čitanje“.
ExistingFileReadOnlyRetry=&Uklonite atribut samo za čitanje i pokušajte ponovo
ExistingFileReadOnlyKeepExisting=&Zadržite postojeću datoteku
ErrorReadingExistingDest=Došlo je do greške pri čitanju postojeće datoteke:
FileExistsSelectAction=Odaberite radnju
FileExists2=Datoteka već postoji.
FileExistsOverwriteExisting=&Zamenite postojeću datoteku
FileExistsKeepExisting=Zadr&žite postojeću datoteku
FileExistsOverwriteOrKeepAll=&Uradite ovo i za naredne konflikte
ExistingFileNewerSelectAction=Odaberite radnju
ExistingFileNewer2=Postojeća datoteka je novija od one koju instalacioni program pokušava da instalira.
ExistingFileNewerOverwriteExisting=&Zamenite postojeću datoteku
ExistingFileNewerKeepExisting=Zadr&žite postojeću datoteku (preporučeno)
ExistingFileNewerOverwriteOrKeepAll=&Uradite ovo i za naredne konflikte
ErrorChangingAttr=Došlo je do greške pri izmeni atributa postojeće datoteke:
ErrorCreatingTemp=Došlo je do greške pri pravljenju datoteke u odredišnoj fascikli:
ErrorReadingSource=Došlo je do greške pri čitanju izvorne datoteke:
ErrorCopying=Došlo je do greške pri kopiranju datoteke:
ErrorDownloading=Došlo je do greške pri preuzimanju datoteke:
ErrorExtracting=Došlo je do greške pri raspakivanju arhive:
ErrorReplacingExistingFile=Došlo je do greške pri zameni postojeće datoteke:
; 'RestartReplace' is an internal name, you may keep it as is
ErrorRestartReplace=Nije moguće zameniti:
ErrorRenamingTemp=Došlo je do greške pri preimenovanju datoteke u odredišnoj fascikli:
ErrorRegisterServer=Nije moguće registrovati DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 nije uspeo sa izlaznim kodom %1
ErrorRegisterTypeLib=Nije moguće registrovati biblioteku tipova: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bitni
UninstallDisplayNameMark64Bit=64-bitni
UninstallDisplayNameMarkAllUsers=Svi korisnici
UninstallDisplayNameMarkCurrentUser=Trenutni korisnik

; *** Post-installation errors
ErrorOpeningReadme=Došlo je do greške pri otvaranju README datoteke.
ErrorRestartingComputer=Instalacioni program nije mogao ponovo da pokrene računar. Uradite to ručno.

; *** Uninstaller messages
UninstallNotFound=Datoteka „%1“ ne postoji. Deinstalacija nije moguća.
UninstallOpenError=Datoteka „%1“ ne može da se otvori. Deinstalacija nije moguća
UninstallUnsupportedVer=Log datoteka deinstalacije „%1“ je u formatu koji ova verzija deinstalacionog programa ne prepoznaje. Deinstalacija nije moguća
UninstallUnknownEntry=Nepoznat unos (%1) se pojavio u log datoteci deinstalacije
ConfirmUninstall=Da li ste sigurni da želite u potpunosti da deinstalirate program %1 i sve njegove komponente?
UninstallOnlyOnWin64=Ova instalacija može da se deinstalira samo na 64-bitnom Windows-u.
OnlyAdminCanUninstall=Ovu instalaciju može da deinstalira samo korisnik sa administratorskim privilegijama.
UninstallStatusLabel=Sačekajte da se %1 deinstalira sa računara.
UninstalledAll=Program %1 je uspešno deinstaliran sa računara.
UninstalledMost=Program %1 je deinstaliran.%n%nNeki elementi nisu mogli biti uklonjeni. Možete ih ukloniti ručno.
UninstalledAndNeedsRestart=Da bi se završila deinstalacija programa %1, potrebno je ponovo pokrenuti računar.%n%nŽelite li da sada ponovo pokrenete računar?
UninstallDataCorrupted=Datoteka „%1“ je oštećena. Deinstalacija nije moguća

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Ukloniti deljenu datoteku?
ConfirmDeleteSharedFile2=Sistem prijavljuje da sledeću deljenu datoteku više ne koristi nijedan program. Želite li da je deinstalacioni program ukloni?%n%nAko neki programi i dalje koriste ovu datoteku, a ona bude uklonjena, ti programi možda neće ispravno raditi. Ako niste sigurni, izaberite „Ne“. Ostavljanje datoteke na sistemu neće prouzrokovati nikakvu štetu.
SharedFileNameLabel=Naziv datoteke:
SharedFileLocationLabel=Lokacija:
WizardUninstalling=Stanje deinstalacije
StatusUninstalling=Deinstaliranje programa %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instaliranje programa %1.
ShutdownBlockReasonUninstallingApp=Deinstaliranje programa %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 verzija %2
AdditionalIcons=Dodatne prečice:
CreateDesktopIcon=Postavi prečicu na &radnu površinu
CreateQuickLaunchIcon=P&ostavi prečicu na traku za brzo pokretanje
ProgramOnTheWeb=%1 na internetu
UninstallProgram=Deinstaliraj %1
LaunchProgram=Pokreni %1
AssocFileExtension=&Poveži %1 sa ekstenzijom datoteke %2
AssocingFileExtension=Povezivanje %1 sa ekstenzijom datoteke %2...
AutoStartProgramGroupDescription=Pokretanje:
AutoStartProgram=Automatski pokreni %1
AddonHostProgramNotFound=%1 se ne nalazi u navedenoj fascikli.%n%nŽelite li ipak da nastavite?
