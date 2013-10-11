; *** Inno Setup version 5.5.3+ Montenegrian messages ***
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/files/istrans/
;
; Translated by Drazen Djurisic (kntaur@gmail.com) based on Rancher (theranchcowboy@gmail.com) translate.
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Crnogorski
LanguageID=$081a
LanguageCodePage=1250
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
SetupWindowTitle=Instalacija  %1
UninstallAppTitle=Deinstalacija
UninstallAppFullTitle=Deinstalacija programa %1

; *** Misc. common
InformationTitle=Podaci
ConfirmTitle=Potvrda
ErrorTitle=Greška

; *** SetupLdr messages
SetupLdrStartupMessage=Započeli ste instalaciju programa %1. Želite li nastaviti?
LdrCannotCreateTemp=Ne mogu da napravim privremenu datoteku. Instalacija je prekinuta.
LdrCannotExecTemp=Ne mogu da pokrenem datoteku u privremenoj fascikli. Instalacija je prekinuta.

; *** Startup error messages
LastErrorMessage=%1.%n%nGreška %2: %3
SetupFileMissing=Datoteka %1 nedostaje u instalacionoj fascikli. Ispravite problem ili nabavite novi primjerak programa.
SetupFileCorrupt=Instalacione datoteke su oštećene. Nabavite novi primjerak programa.
SetupFileCorruptOrWrongVer=Instalacione datoteke su oštećene ili nisu saglasne s ovom verzijom instalacije. Ispravite problem ili nabavite novi primjerak programa.
InvalidParameter=Neispravan parametar je prenijet na komandnu liniju: %n%n%1
SetupAlreadyRunning=Instalacija je već pokrenuta.
WindowsVersionNotSupported=Program ne podrava izdanje vindousa koju koristite.
WindowsServicePackRequired=Program zahtijeva %1 servisni paket %2 ili noviji.
NotOnThisPlatform=Program neće raditi na %1.
OnlyOnThisPlatform=Program će raditi na %1.
OnlyOnTheseArchitectures=Program se može instalirati samo na izdanjima vindousa koji rade na sledećim arhitekturama procesora:%n%n%1
MissingWOW64APIs=Izdanje vindousa koje koristite ne sadrži funkcionalnost potrebnu za izvšravanje 64-bitnih instalacija. Instalirajte servisni paket %1 da biste riješili ovaj problem.
WinVersionTooLowError=Program zahtijeva %1, izdanje %2 ili novije.
WinVersionTooHighError=Program ne možete instalirati na %1 izdanju %2 ili novijem.
AdminPrivilegesRequired=Morate biti prijavljeni kao administrator da biste instalirali program.
PowerUserPrivilegesRequired=Morate biti prijavljeni kao administrator ili ovlašćeni korisnik da biste instalirali program.
SetupAppRunningError=Program %1 je trenutno pokrenut.%n%nZatvorite ga i kliknite na dugme U redu da nastavite ili Otkaži da napustite instalaciju.
UninstallAppRunningError=Program %1 je trenutno pokrenut.%n%nZatvorite ga i kliknite na dugme U redu da nastavite ili Otkaži da napustite instalaciju.

; *** Misc. errors
ErrorCreatingDir=Ne mogu da napravim fasciklu %1.
ErrorTooManyFilesInDir=Ne mogu da napravim datoteku u fascikli %1 jer sadrži previše datoteka.

; *** Setup common messages
ExitSetupTitle=Prekid instalacije
ExitSetupMessage=Instalacija nije završena. Ako sada izađete, program neće biti instaliran.%n%nInstalaciju možete pokrenuti i dovršiti nekom dugom prilikom.%n%nPrekid instalacije?
AboutSetupMenuItem=&O programu
AboutSetupTitle=Podaci o programu
AboutSetupMessage=%1 verzija %2%n%3%n%n%1 matična stranica:%n%4
AboutSetupNote=
TranslatorNote=Translated by Drazen Djurisic.

; *** Buttons
ButtonBack=< &Nazad
ButtonNext=&Dalje >
ButtonInstall=&Instaliraj
ButtonOK=&U redu
ButtonCancel=&Otkaži
ButtonYes=&Da
ButtonYesToAll=D&a za sve
ButtonNo=&Ne
ButtonNoToAll=N&e za sve
ButtonFinish=&Završi
ButtonBrowse=&Potraži
ButtonWizardBrowse=&Potraži
ButtonNewFolder=&Napravi fasciklu

; *** "Select Language" dialog messages
SelectLanguageTitle=Odabir jezika
SelectLanguageLabel=Izaberite jezik tokom instalacije:

; *** Common wizard text
ClickNext=Kliknite na Dalje da nastavite ili Otkaži da napustite instalaciju.
BeveledLabel=
BrowseDialogTitle=Odabir fascikle
BrowseDialogLabel=Izaberite fasciklu sa spiska i kliknite na U redu.
NewFolderName=Nova fascikla

; *** "Welcome" wizard page
WelcomeLabel1=Dobro došli na instalaciju programa [name]
WelcomeLabel2=Instaliraće te [name/ver] na Vaš računar.%n%nPre nego to nastavite, preporučjemo Vam da zatvorite sve druge programe.

; *** "Password" wizard page
WizardPassword=Lozinka
PasswordLabel1=Instalacija je zaštićena lozinkom.
PasswordLabel3=Unesite lozinku i kliknite na Dalje da nastavite. Imajte na umu da je lozinka osjetljiva na mala i velika slova.
PasswordEditLabel=&Lozinka:
IncorrectPassword=Navedena lozinka nije ispravna. Pokušajte ponovo.

; *** "License Agreement" wizard
WizardLicense=Ugovor o licenci
LicenseLabel=Pažljivo pročitajte sledeće prije nego što nastavite.
LicenseLabel3=Pročitajte Ugovor o licenci koji se nalazi ispod. Morate prihvatiti uslove ovog ugovora prije nego što nastavite.
LicenseAccepted=&Prihvatam ugovor
LicenseNotAccepted=&Ne prihvatam ugovor

; *** "Information" wizard pages
WizardInfoBefore=Informacije
InfoBeforeLabel=Pažljivo pročitajte sledeće prije nego što nastavite.
InfoBeforeClickLabel=Kada budete spremni da nastavite instalaciju, kliknite na Dalje.
WizardInfoAfter=Informacije
InfoAfterLabel=Pažljivo pročitajte sledeće prije nego što nastavite.
InfoAfterClickLabel=Kada budete spremni da nastavite instalaciju, kliknite na Dalje.

; *** "User Information" wizard page
WizardUserInfo=Korisnički podaci
UserInfoDesc=Unesite svoje podatke.
UserInfoName=&Korisnik:
UserInfoOrg=&Organizacija:
UserInfoSerial=&Serijski broj:
UserInfoNameRequired=Morate navesti ime.

; *** "Select Destination Location" wizard page
WizardSelectDir=Odabir odredišne fascikle
SelectDirDesc=Izaberite mjesto na kom želite da instalirate [name].
SelectDirLabel3=Program će instalirati [name] u sledeću fasciklu.
SelectDirBrowseLabel=Kliknite na Dalje da nastavite. Ako želite da izaberete drugu fasciklu, kliknite na Potraži.
DiskSpaceMBLabel=Potrebno je najmanje [mb] MB slobodnog prostora na disku.
CannotInstallToNetworkDrive=Ne mogu da instaliram na mrežnu jedinicu.
CannotInstallToUNCPath=Ne mogu da instaliram na UNC putanju.
InvalidPath=Morate navesti punu putanju s obilježjem diska (npr.%n%nC:\APP%n%nili putanja u obliku%n%n\\server\share)
InvalidDrive=Disk koji ste izabrali ne postoji ili nije dostupan. Izaberite neki drugi.
DiskSpaceWarningTitle=Nedovoljno prostora na disku
DiskSpaceWarning=Program zahtijeva najmanje %1 kB slobodnog prostora, a izabrani disk na raspolaganju ima samo %2 kB.%n%nŽelite li ipak da nastavite?
DirNameTooLong=Naziv fascikle ili putanja je predugačka.
InvalidDirName=Naziv fascikle nije ispravan.
BadDirName32=Naziv fascikle ne sme sadržati ništa od sledećih:%n%n%1
DirExistsTitle=Fascikla već postoji
DirExists=Fascikla:%n%n%1%n%nveć postoji. Želite li ipak da instalirate program u nju?
DirDoesntExistTitle=Fascikla ne postoji
DirDoesntExist=Fascikla:%n%n%1%n%nne postoji. Želite li da je napravite?

; *** "Select Components" wizard page
WizardSelectComponents=Odabir komponenata
SelectComponentsDesc=Koje komponente želite da instalirate?
SelectComponentsLabel2=Izaberite komponente koje želite da instalirate, a očistite one koje ne želite. Kliknite na Dalje da nastavite.
FullInstallation=Puna instalacija
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Podrazumijevana instalacija
CustomInstallation=Prilagođena instalacija
NoUninstallWarningTitle=Komponente već postoje
NoUninstallWarning=Sledeće komponente već postoje na računaru:%n%n%1%n%nDeštrikliranje ovih komponenti ih neće ukloniti.%n%nŽelite li da nastavite?
ComponentSize1=%1 kB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Izabrane stavke zahtevaju najmanje [mb] MB slobodnog prostora.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Odabir dodatnih zadataka
SelectTasksDesc=Izaberite neke dodatne zadatke.
SelectTasksLabel2=Izaberite dodatne zadatke koje želite da izvršite pri instaliranju programa [name] i kliknite na Dalje.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Odabir fascikle u meniju Start
SelectStartMenuFolderDesc=Izaberite mjesto na kom želite da postavite prečice.
SelectStartMenuFolderLabel3=Instalacija će postaviti prečicu programa u sledećoj fascikli u meniju Start.
SelectStartMenuFolderBrowseLabel=Kliknite na Dalje da nastavite. Ako želite da izaberete drugu fasciklu, kliknite na Potraži.
MustEnterGroupName=Morate navesti naziv fascikle.
GroupNameTooLong=Naziv fascikle ili putanja je predugačka.
InvalidGroupName=Naziv fascikle nije ispravan.
BadGroupName=Naziv fascikle ne smije sadržati ništa od sledećih:%n%n%1
NoProgramGroupCheck2=N&e pravi fasciklu u meniju Start

; *** "Ready to Install" wizard page
WizardReady=Instalacija je spremna
ReadyLabel1=Program je spreman da instalira [name] na računar.
ReadyLabel2a=Kliknite na Instaliraj da započnete instalaciju ili Nazad da ponovo pregledate i promijenite pojedine postavke.
ReadyLabel2b=Kliknite na Instaliraj da započnete instalaciju.
ReadyMemoUserInfo=Korisnički podaci:
ReadyMemoDir=Odredišna fascikla:
ReadyMemoType=Vrsta instalacije:
ReadyMemoComponents=Izabrane komponente:
ReadyMemoGroup=Fascikla u meniju Start:
ReadyMemoTasks=Dodatni zadaci:

; *** "Preparing to Install" wizard page
WizardPreparing=Priprema za instalaciju
PreparingDesc=Program se priprema da instalira [name] na računar.
PreviousInstallNotCompleted=Instalacija ili deinstalacija prethodnog programa nije završena. Potrebno je da ponovo pokrenete računar da bi se instalacija završila.%n%nNakon ponovnog pokretanja, otvorite instalaciju i instalirajte program [name].
CannotContinue=Ne mogu da nastavim instalaciju. Kliknite na Otkaži da izađete.
ApplicationsFound=Sledeći programi koriste datoteke koje treba da ažurira instalacioni program. Preporučujemo vam da dozvolite instalacionom programu da zatvori ove programe.
ApplicationsFound2=Sledeći programi koriste datoteke koje treba da ažurira instalacioni program. Preporučujemo vam da dozvolite instalacionom programu da zatvori ove programe. Nakon to se instalacija zavri, instalacioni program 壠pokuati da ponovo pokrene zatvorene programe.
CloseApplications=&Zatvori programe
DontCloseApplications=&Ne zatvaraj programe
ErrorCloseApplications=Ne mogu da zatvorim sve programe. Pre nego što nastavite, preporučujemo vam da zatvorite sve programe koji koriste datoteke koje treba da ažurira instalacioni program.

; *** "Installing" wizard page
WizardInstalling=Instaliranje
InstallingLabel=Sačekajte da se [name] instalira na računar.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=[name]  završetak instalacije
FinishedLabelNoIcons=Instaliranje programa [name] je završeno.
FinishedLabel=Instaliranje programa [name] je završeno. Možete ga pokrenuti preko postavljenih ikona.
ClickFinish=Kliknite na Završi da izađete.
FinishedRestartLabel=Potrebno je ponovno pokretanje računara da bi se instalacija završila. Želite li da ga ponovo pokrenete?
FinishedRestartMessage=Potrebno je ponovno pokretanje računara da bi se instalacija završila.%n%nŽelite li da ga ponovo pokrenete?
ShowReadmeCheck=Da, želim da pogledam tekstualnu datoteku
YesRadio=&Da, ponovo pokreni računar
NoRadio=&Ne, kasnije ću ga pokrenuti
; used for example as 'Run MyProg.exe'
RunEntryExec=&Pokreni %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Pogledaj %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Sledeći disk
SelectDiskLabel2=Ubacite disk %1 i kliknite na U redu.%n%nAko se datoteke na ovom disku mogu pronaći u nekoj drugoj fascikli, unesite odgovarajuću putanju ili kliknite na Potraži.
PathLabel=&Putanja:
FileNotInDir2=Datoteka %1 se ne nalazi u %2. Ubacite pravi disk ili izaberite drugu fasciklu.
SelectDirectoryLabel=Izaberite mesto sledećeg diska.

; *** Installation phase messages
SetupAborted=Instalacija nije završena.%n%nIspravite problem i pokrenite je ponovo.
EntryAbortRetryIgnore=Kliknite na Pokušaj opet da ponovite radnju, Zanemari da nastavite u svakom slučaju ili Prekini da obustavite instalaciju.

; *** Installation status messages
StatusClosingApplications=Zatvaram programe
StatusCreateDirs=Pravim fascikle
StatusExtractFiles=Raspakujem datoteke
StatusCreateIcons=Postavljam prečice
StatusCreateIniEntries=Postavljam INI unose
StatusCreateRegistryEntries=Postavljam unose u registar
StatusRegisterFiles=Upisujem datoteke
StatusSavingUninstall=Čuvam podatke o deinstalaciji
StatusRunProgram=Završavam instalaciju
StatusRestartingApplications=Ponovo pokrećem programe
StatusRollback=Poništavam izmene

; *** Misc. errors
ErrorInternal2=Unutrašnja greka: %1
ErrorFunctionFailedNoCode=%1 neuspjeh
ErrorFunctionFailed=%1 neuspjeh; kod %2
ErrorFunctionFailedWithMessage=%1 neuspjeh; kod %2.%n%3
ErrorExecutingProgram=Ne mogu da pokrenem datoteku:%n%1

; *** Registry errors
ErrorRegOpenKey=Greška pri otvaranju unosa u registru:%n%1\%2
ErrorRegCreateKey=Greška pri stvaranju unosa u registru:%n%1\%2
ErrorRegWriteKey=Greška pri upisivanju unosa u registar:%n%1\%2

; *** INI errors
ErrorIniEntry=Greška pri stvaranju INI unosa u datoteci %1.

; *** File copying errors
FileAbortRetryIgnore=Kliknite na Pokušaj opet da ponovite radnju, Zanemari da preskočite datoteku (ne preporučuje se) ili Prekini da obustavite instalaciju.
FileAbortRetryIgnore2=Kliknite na Pokušaj opet da ponovite radnju, Zanemari da nastavite u svakom slučaju (ne preporučuje se) ili Prekini da obustavite instalaciju.
SourceIsCorrupted=Izvorna datoteka je oštećena
SourceDoesntExist=Izvorna datoteka %1 ne postoji
ExistingFileReadOnly=Postojeća datoteka je samo za čitanje.%n%nKliknite na Pokušaj opet da uklonite osobinu samo za čitanje i ponovite radnju, Zanemari da preskočite datoteku ili Prekini da obustavite instalaciju.
ErrorReadingExistingDest=Došlo je do greške pri pokušaju čitanja postojeće datoteke:
FileExists=Datoteka već postoji.%n%nŽelite li da je zamijenite?
ExistingFileNewer=Postojeća datoteka je novija od one koju treba postaviti. Preporučujemo vam da zadržite postojeću datoteku.%n%nŽelite li to da uradite?
ErrorChangingAttr=Došlo je do greške pri izmeni osobine sledeće datoteke:
ErrorCreatingTemp=Došlo je do greške pri stvaranju datoteke u odredišnoj fascikli:
ErrorReadingSource=Došlo je do greške pri čitanju izvorne datoteke:
ErrorCopying=Došlo je do greške pri umnožavanju datoteke:
ErrorReplacingExistingFile=Došlo je do greške pri zamjeni postojeće datoteke:
ErrorRestartReplace=Ne mogu da zamijenim:
ErrorRenamingTemp=Dolo je do greške pri preimenovanju datoteke u odredišnoj fascikli:
ErrorRegisterServer=Ne mogu da upišem DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 nije uspio. Greka %1
ErrorRegisterTypeLib=Ne mogu da upišem biblioteku tipova: %1

; *** Post-installation errors
ErrorOpeningReadme=Došlo je do greške pri otvaranju tekstualne datoteke.
ErrorRestartingComputer=Ne mogu ponovo da pokrenem računar. Uradite to sami.

; *** Uninstaller messages
UninstallNotFound=Datoteka %1 ne postoji. Ne mogu da deinstaliram program.
UninstallOpenError=Datoteka %1 ne može da se otvori. Ne mogu da deinstaliram program.
UninstallUnsupportedVer=Izvještaj %1 je u neprepoznatljivom formatu. Ne mogu da deinstaliram program.
UninstallUnknownEntry=Nepoznat unos (%1) se pojavio u izvještaju deinstalacije.
ConfirmUninstall=Želite li da deinstalirate %1 i sve njegove komponente?
UninstallOnlyOnWin64=Program se može deinstalirati samo na 64-bitnom vindousu.
OnlyAdminCanUninstall=Program može deinstalirati samo korisnik s administratorskim pravima.
UninstallStatusLabel=Sačekajte da se %1 deinstalira sa računara.
UninstalledAll=%1 je deinstaliran sa računara.
UninstalledMost=%1 je deinstaliran.%n%nNeke komponente će te ipak morati sami obrisati.
UninstalledAndNeedsRestart=Potrebno je ponovno pokretanje računara da bi se instalacija završila.%n%nŽelite li da ponovo pokrenete računar?
UninstallDataCorrupted=Datoteka %1 je oštećena. Ne mogu da deinstaliram program.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Brisanje dijeljene datoteke
ConfirmDeleteSharedFile2=Sistem je prijavio da sledeću dijeljenu datoteku više ne koristi nijedan program. Želite li da je uklonite?%n%nAko nekim programima i dalje treba ova datoteka a ona je obrisana, ti programi možda neće ispravno raditi. Ako niste sigurni šta da radite, kliknite na Ne. Ostavljanje datoteke na disku neće prouzrokovati nikakvu štetu.
SharedFileNameLabel=Naziv datoteke:
SharedFileLocationLabel=Putanja:
WizardUninstalling=Stanje deinstalacije
StatusUninstalling=Deinstaliram %1

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instaliram %1.
ShutdownBlockReasonUninstallingApp=Deinstaliram %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 verzija %2
AdditionalIcons=Dodatne ikone:
CreateDesktopIcon=&Postavi ikonu na radnu površinu
CreateQuickLaunchIcon=P&ostavi ikonu na traku za brzo pokretanje
ProgramOnTheWeb=%1 na internetu
UninstallProgram=Deinstaliraj %1
LaunchProgram=Pokreni %1
AssocFileExtension=&Poveži %1 sa formatom %2
AssocingFileExtension=Povezujem %1 sa formatom %2
AutoStartProgramGroupDescription=Pokretanje:
AutoStartProgram=Automatski pokreni %1
AddonHostProgramNotFound=%1 se ne nalazi u navedenoj fascikli.%n%nŽelite li ipak da nastavite?