; *** Inno Setup version 6.1.0+ Montenegrin messages ***
;
; To download user-contributed translations of this file, go to:
;   https://jrsoftware.org/files/istrans/
;
; Translated by Drazen Djurisic (kntaur@gmail.com)
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
SetupWindowTitle=Instalacija - %1
UninstallAppTitle=Deinstalacija
UninstallAppFullTitle=Deinstalacija programa %1

; *** Misc. common
InformationTitle=Podaci
ConfirmTitle=Potvrda
ErrorTitle=Greöka

; *** SetupLdr messages
SetupLdrStartupMessage=Instalirat e te %1. éelite li da nastavite?
LdrCannotCreateTemp=Ne mogu da napravim privremenu datoteku. Instalacija obustavljena
LdrCannotExecTemp=Ne mogu da pokrenem datoteku u privremenom direktorijumu. Instalacija obustavljena
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nGreöka %2: %3
SetupFileMissing=Datoteka %1 nedostaje u instalacionom direktorijumu. Ispravite problem ili nabavite novi primjerak programa.
SetupFileCorrupt=Instalacione datoteke su oötecene. Nabavite novi primjerak programa.
SetupFileCorruptOrWrongVer=Instalacione datoteke su oötecene, ili su nekompatibilne sa ovom verzijom instalacije. Ispravite problem ili nabavite novi primjerak programa.
InvalidParameter=Neispravan parametar je prenijet na komandnu liniju:%n%n%1
SetupAlreadyRunning=Instalacija je vec pokrenuta.
WindowsVersionNotSupported=Ova verzija programa nije kompatibilna sa verzijom windows'-a na vaöem racunaru.
WindowsServicePackRequired=Ovaj program zahtijeva %1 servisni paket %2 ili noviji.
NotOnThisPlatform=Program nece raditi na %1.
OnlyOnThisPlatform=Program ce raditi na %1.
OnlyOnTheseArchitectures=Program se moûe instalirati samo na verzijama windows-a koji rade na sledecim arhitekturama procesora:%n%n%1
WinVersionTooLowError=Ovaj program zahtijeva %1 verziju %2 ili noviju.
WinVersionTooHighError=Program se ne moûe instalirati na %1 verziju %2 ili noviju.
AdminPrivilegesRequired=Morate biti prijavljeni kao administrator da bi ste instalirali program.
PowerUserPrivilegesRequired=Morate biti prijavljeni kao administrator ili ovlaöceni korisnik da bi ste instalirali ovaj program.
SetupAppRunningError=Program %1 je trenutno pokrenut.%n%nUgasite ga, kliknite "U redu", ili "Obustavi" za napustite instalaciju.
UninstallAppRunningError=Program %1 je trenutno pokrenut.%n%nUgasite ga, kliknite "U redu", ili "Obustavi" za napustite instalaciju.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Izaberite nacin instalacije
PrivilegesRequiredOverrideInstruction=Izaberite nacin instalacije
PrivilegesRequiredOverrideText1=%1 moûe biti instaliran za sve korisnike (zahtijeva administratorske privilegije), ili samo za vas.
PrivilegesRequiredOverrideText2=%1 moûe biti instaliran za vas samo, ili za sve korisnike (zahtijeva administratorske privilegije).
PrivilegesRequiredOverrideAllUsers=Instalacija za &sve korisnike
PrivilegesRequiredOverrideAllUsersRecommended=Instalacija za &sve korisnike (preporucuje se)
PrivilegesRequiredOverrideCurrentUser=Instalacija &za vas samo
PrivilegesRequiredOverrideCurrentUserRecommended=Instalacija &za vas samo (preporucuje se)

; *** Misc. errors
ErrorCreatingDir=Instalacija ne moûe da napravi direktorijum "%1"
ErrorTooManyFilesInDir=Ne mogu da napravim datoteku u direktorijumu "%1" zato öto sadrûi previöe datoteka

; *** Setup common messages
ExitSetupTitle=Napusti instalaciju
ExitSetupMessage=Instalacija nije kompletna. Ako izadete sad, program nece biti instaliran.%n%nMoûete pokrenuti instalaciju neki drugi put da zavröite instaliranje.%n%nNapusti instalaciju?
AboutSetupMenuItem=&O programu...
AboutSetupTitle=O programu
AboutSetupMessage=%1 verzija %2%n%3%n%n%1 internet stranica:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Nazad
ButtonNext=&Dalje >
ButtonInstall=&Instaliraj
ButtonOK=U redu
ButtonCancel=Obustavi
ButtonYes=&Da
ButtonYesToAll=Da za &sve
ButtonNo=&Ne
ButtonNoToAll=N&e za sve
ButtonFinish=&Zavröi
ButtonBrowse=&Pretraûi...
ButtonWizardBrowse=P&retraûi...
ButtonNewFolder=&Napravi novi direktorijum

; *** "Select Language" dialog messages
SelectLanguageTitle=Izaberite jezik instalacije
SelectLanguageLabel=Izaberite jezik koji ce te koristiti tokom instalacije.

; *** Common wizard text
ClickNext=Kliknite "Dalje" za nastavak, ili "Obustavi" da prekinete instalaciju.
BeveledLabel=
BrowseDialogTitle=Izaberite direktorijum
BrowseDialogLabel=Izaberite direktorijum sa liste ispod, onda kliknite na "U redu".
NewFolderName=Novi direktorijum

; *** "Welcome" wizard page
WelcomeLabel1=Dobro doöli na instalaciju programa [name]
WelcomeLabel2=Instalirat ce te [name/ver] na vaöem racunaru.%n%nPreporucujemo da zatvorite sve ostale programe pa da nastavite sa instalacijom.

; *** "Password" wizard page
WizardPassword=Lozinka
PasswordLabel1=Instalacija je zaöticena lozinkom.
PasswordLabel3=Unesite lozinku i kliknite "Dalje" da nastavite. Lozinka je osjetljiva na velika i mala slova.
PasswordEditLabel=&Lozinka:
IncorrectPassword=Lozinka koju ste unijeli je netacna. Probajte opet.

; *** "License Agreement" wizard page
WizardLicense=Ugovor o licenci
LicenseLabel=Paûljivo procitajte sledece prije nego nastavite.
LicenseLabel3=Procitajte ugovor o licenci koji je ispod. Morate prihvatiti uslove ugovora ako ûelite da nastavite sa instalacijom.
LicenseAccepted=&Prihvatam ugovor
LicenseNotAccepted=&Ne prihvatam ugovor

; *** "Information" wizard pages
WizardInfoBefore=Informacije
InfoBeforeLabel=Paûljivo procitajte sledece prije nego nastavite.
InfoBeforeClickLabel=Kada budete spremni da nastavite instalaciju, kliknite "Dalje".
WizardInfoAfter=Informacije
InfoAfterLabel=Paûljivo procitajte sledece prije nego nastavite.
InfoAfterClickLabel=Kada budete spremni da nastavite instalaciju, kliknite "Dalje".

; *** "User Information" wizard page
WizardUserInfo=Podaci o korisniku
UserInfoDesc=Unesite vaöe podatke.
UserInfoName=&Korisnik:
UserInfoOrg=&Organizacija:
UserInfoSerial=&Serijski broj:
UserInfoNameRequired=Morate unijeti ime.

; *** "Select Destination Location" wizard page
WizardSelectDir=Izaberite lokaciju
SelectDirDesc=Gdje ce [name] biti instaliran?
SelectDirLabel3=Program [name] ce biti instaliran u direktorijumu.
SelectDirBrowseLabel=Za nastavak pritisnite "Dalje". Ako ûelite drugi direktorijum, pritisnite "Potraûi".
DiskSpaceGBLabel=Potrebno je najmanje [gb] GB slobodnog prostora na disku.
DiskSpaceMBLabel=Potrebno je najmanje [mb] MB slobodnog prostora na disku.
CannotInstallToNetworkDrive=Program ne moûete instalirati na mreûnom disku.
CannotInstallToUNCPath=Program ne moûete instalirati na UNC putanji.
InvalidPath=Morate navesti cijelu putanju sa oznakom diska; npr:%n%nC:\APP%n%nili UNC putanja u obliku:%n%n\\server\share
InvalidDrive=Disk ili UNC koji ste naveli ne postoji ili nije dostupan. Izaberite drugi.
DiskSpaceWarningTitle=Nema dovoljno prostora na disku
DiskSpaceWarning=Programu je potrebno %1 KB slobodnog prostora za instalaciju, ali izabrani disk ima samo %2 KB.%n%nDa li ûelite da nastavite bez obzira?
DirNameTooLong=Ime direktorijuma ili putanja je predugacka.
InvalidDirName=Ime direktorijuma nije valjano.
BadDirName32=Ime direktorijuma ne moûe da sadrûi nijedan od sledecih karaktera:%n%n%1
DirExistsTitle=Direktorijum vec postoji
DirExists=Direktorijum:%n%n%1%n%nvec postoji. éelite ili da nastavite sa instalcijom u postojeci direktorijum?
DirDoesntExistTitle=Direktorijum ne postoji
DirDoesntExist=Direktorijum:%n%n%1%n%nne postoji. éelite li da kreiramo navedeni direktorijum?

; *** "Select Components" wizard page
WizardSelectComponents=Odabir komponenata
SelectComponentsDesc=Koje komponente ûelite da instalirate?
SelectComponentsLabel2=Izaberite komponente koje ûelite da instalirate; ocistite komponente koje ne ûelite. Kliknite "Dalje" za nastavak instalacije.
FullInstallation=Kompletna instalacija
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Podrazumijevana instalacija
CustomInstallation=Prilagodjena instalacija
NoUninstallWarningTitle=Komponenta postoji
NoUninstallWarning=Program je pronaöao da su sledece komponente vec instalirane:%n%n%1%n%n≈†trikiranjem ovih komponenti one nece biti deinstalirane.%n%nDa li ûelite da nastavite?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Izabrane stavke zahtijevaju najmanje [gb] GB prostora na disku.
ComponentsDiskSpaceMBLabel=Izabrane stavke zahtijevaju najmanje [mb] MB prostora na disku.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Odabir dodatnih zadataka
SelectTasksDesc=Koje dodatne zadatke ûelite program da izvröi?
SelectTasksLabel2=Izaberite dodatne zadatke koje zelite da  [name] izvröi, onda kliknite "Dalje".

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Odabir direktorijuma u meniju "Start"
SelectStartMenuFolderDesc=Izaberite mjesto gdje ûelite da budu precice?
SelectStartMenuFolderLabel3=Instalacija ce postaviti precice u sledecem direktorijumu "Start" menija.
SelectStartMenuFolderBrowseLabel=Za nastavak pritisnite "Dalje". Ako ûelite drugi direktorijum pritisnite "Potraûi".
MustEnterGroupName=Morate unijeti ime direktorijuma.
GroupNameTooLong=Ime direktorijuma ili putanje je predugacko.
InvalidGroupName=Ime direktorijuma nije valjano.
BadGroupName=Naziv direktorijuma ne smije da sadrûi sledece karaktere:%n%n%1
NoProgramGroupCheck2=&Nemoj kreirati direktorijum u "Start" meniju

; *** "Ready to Install" wizard page
WizardReady=Spreman za instalaciju
ReadyLabel1=Program je spreman da instalira [name] na vaöem racunaru.
ReadyLabel2a=Klikni "Instaliraj" da zapocnete instalaciju ili "Nazad" da ponovo pogledate i promijenite pojedine stavke.
ReadyLabel2b=Klikni "Instaliraj" da zapocnete instalaciju.
ReadyMemoUserInfo=Podaci o korisniku:
ReadyMemoDir=Lokacija direktorijuma:
ReadyMemoType=Vrsta instalacije:
ReadyMemoComponents=Izabrane komponente:
ReadyMemoGroup=Direktorijum u meniju "Start":
ReadyMemoTasks=Dodatni zadaci:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=Snimam dodatne datoteke...
ButtonStopDownload=&Zaustavi snimanje
StopDownload=Jeste li sigurni da ûelite da zaustavite snimanje?
ErrorDownloadAborted=Snimanje obustavljeno
ErrorDownloadFailed=Snimanje neuspjeöno: %1 %2
ErrorDownloadSizeFailed=informacije o velicini netacne: %1 %2
ErrorFileHash1=Hash identifikacija datoteke netacna: %1
ErrorFileHash2=Netacna hash identifikacija datoteke: ocekivan %1, naden %2
ErrorProgress=Neispravan progres: %1 of %2
ErrorFileSize=Neispravna velicina datoteke: ocekivana %1, nadena %2

; *** "Preparing to Install" wizard page
WizardPreparing=Priprema za instalaciju
PreparingDesc=Program se sprema da instalira [name] na vaöem racunaru.
PreviousInstallNotCompleted=Instalacija ili deinstalacija prethodnog programa nije zavröena. Potrebno je da restartujete racunar da bi se instalacija zavröila.%n%nNakon restarta racunara pokrenite instalaciju ponovo da bi se [name] instalirao.
CannotContinue=Instalacija nije moguca. Kliknite "Otkaûi" da izadete.
ApplicationsFound=Sledeci programi koriste datoteke koje treba da aûurira instalacioni program. Preporucujemo da dozvolite instalacionom programu da zatvori ove programe.
ApplicationsFound2=Sledeci programi koriste datoteke koje treba da aûurira instalacioni program. Preporucujemo da dozvolite instalacionom programu da zatvori ove programe. Nakon kompletirane instalacije, instalacija ce pokuöati da restartuje program .
CloseApplications=&Automatski zatvorite program
DontCloseApplications=&Ne zatvaraj program
ErrorCloseApplications=Ne mogu da zatvorim sve programe. Preporucujemo da ugasite sve programe cije datoteke treba da nadogradi instlacija.
PrepareToInstallNeedsRestart=Instalacija mora da restartuje racunar. Nakon restarta racunara, pokrenite instalaciju [name] da bi dovröili instalaciju.%n%néelite li da restartujete racunar?

; *** "Installing" wizard page
WizardInstalling=Instaliram
InstallingLabel=Sacekajte da program instalira [name] na vaö racunar.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Zavröavam sa instalacijom [name] 
FinishedLabelNoIcons=Instalacija [name] je zavröena na vaöem racunaru.
FinishedLabel=Instalacija [name] je zavröena. Program moûete startovati klikom na instaliranu precicu.
ClickFinish=Kliknite na "Zavröi" da izadete.
FinishedRestartLabel=Da bi instalacija [name] bila kompletna, program mora restartovati racunar. Restartovanje racunara?
FinishedRestartMessage=Da bi instalacija [name] bila kompletna, program mora restartovati racunar.%n%nRestartovanje racunara?
ShowReadmeCheck=Da, ûelim da pogledam tekstualnu datoteku
YesRadio=&Da, restartovacu racunar sada
NoRadio=&Ne, restartovacu racunar kasnije
; used for example as 'Run MyProg.exe'
RunEntryExec=Run %1
; used for example as 'View Readme.txt'
RunEntryShellExec=View %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Sledeci disk
SelectDiskLabel2=Ubacite disk %1 i kliknite "U redu".%n%nAko se datoteke na ovom disku mogu pronaci u nekom drugom direktorijumu, unesite odgovarajucu putanju ili kliknite na ‚ÄûPotraûi‚Äú
PathLabel=&Path:
FileNotInDir2=Datoteka "%1" ne postoji na lokaciji "%2". Ubacite pravi disk ili izaberite drugi direktorijum.
SelectDirectoryLabel=Navedite lokaciju sledeceg diska.

; *** Installation phase messages
SetupAborted=Instalacija nije kompletna.%n%nIspravite problem i pokrenite instalaciju ponovo.
AbortRetryIgnoreSelectAction=Odaberite radnju
AbortRetryIgnoreRetry=&pokuöaj ponovo
AbortRetryIgnoreIgnore=&Ignoriöi greöku i nastavi
AbortRetryIgnoreCancel=Prekini instalaciju

; *** Installation status messages
StatusClosingApplications=Zatvaram programe...
StatusCreateDirs=Kreiram direktorijume...
StatusExtractFiles=Raspakujem datoteke...
StatusCreateIcons=Kreiram precice...
StatusCreateIniEntries=Kreiram INI unose...
StatusCreateRegistryEntries=Kreiram unose u registar...
StatusRegisterFiles=Registrujem datoteke...
StatusSavingUninstall=Snimam deinstalacione informacije...
StatusRunProgram=Yavröavam sa instalacijom...
StatusRestartingApplications=Restartujem program...
StatusRollback=Poniötavam izmjene...

; *** Misc. errors
ErrorInternal2=Interna greöka: %1
ErrorFunctionFailedNoCode=%1 neuspjeh
ErrorFunctionFailed=%1 neuspjeh; kod %2
ErrorFunctionFailedWithMessage=%1 neuspjeh; kod %2.%n%3
ErrorExecutingProgram=Ne mogu da pokrenem datoteku:%n%1

; *** Registry errors
ErrorRegOpenKey=Greöka pri unosu u registri:%n%1\%2
ErrorRegCreateKey=Greöka pri unosu u registri:%n%1\%2
ErrorRegWriteKey=Greöka pri unosu u registri:%n%1\%2

; *** INI errors
ErrorIniEntry=Greöka pri stvaranju INI unosa u datoteci "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Izostavite ovu datoteku (ne preporucuje se)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignoriöi greöku i nastaviti instalaciju (ne preporucuje se)
SourceIsCorrupted=Instalaciona datoteka je oötecena
SourceDoesntExist=Instalaciona datoteka "%1" ne postoji
ExistingFileReadOnly2=Postojeca datoteka ne moûe se presnimiti jer je oznacena kao samo za citanje.
ExistingFileReadOnlyRetry=&Uklonite atribut na datoteci samo za citanje i pokuöajte ponovo
ExistingFileReadOnlyKeepExisting=&Zadrûati postojecu datoteku
ErrorReadingExistingDest=Greöka nastala pri citanju vec postojece datoteke:
FileExistsSelectAction=Izaberite operaciju
FileExists2=Datoteka vec postoji.
FileExistsOverwriteExisting=&Presnimite postojecu datoteku
FileExistsKeepExisting=&Zadrûati postojecu datoteku
FileExistsOverwriteOrKeepAll=&Uradi ovo kod sledeceg problema
ExistingFileNewerSelectAction=Izaberi operaciju
ExistingFileNewer2=Postojeca datoteka je novija od ove koju ûelimo da instaliramo.
ExistingFileNewerOverwriteExisting=&Presnimite postojecu datoteku 
ExistingFileNewerKeepExisting=&Zadrûite postojecu datoteku (preporucujemo)
ExistingFileNewerOverwriteOrKeepAll=&Uradi ovo kod sledeceg problema
ErrorChangingAttr=Greöka kod pokuöaja da se promijeni atribut datoteke:
ErrorCreatingTemp=Greöka kod kreiranja datoteke u navedenom direktorijumu:
ErrorReadingSource=Greöka kod pokuöaja citanja instalacione datoteke:
ErrorCopying=Greöka kod pokuöaja snimanja datoteke:
ErrorReplacingExistingFile=Greöka kod pokuöaja presnimavanja postojece datoteke:
ErrorRestartReplace=Ne mogu da zamijenim:
ErrorRenamingTemp=Doölo je do greöke pri pokuöaju da preimenujem datoteku u navedenom direktorijumu
ErrorRegisterServer=Ne mogu da registrujem DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 nije uspio. Greöka %1
ErrorRegisterTypeLib=Ne mogu da upiöem biblioteku tipova: %1

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
ErrorOpeningReadme=Greöka pri otvaranju tekstualne datoteke.
ErrorRestartingComputer=Instalacija ne moûe da restartuje racunar. Uradite to sami.

; *** Uninstaller messages
UninstallNotFound=Datoteka "%1" ne postoji. Ne mogu da deinstaliram.
UninstallOpenError=Datoteka "%1" se ne moûe otvoriti. Ne mogu da deinstaliram
UninstallUnsupportedVer=Izvjeötaj "%1" nije prepoznat ode ove verzije deinstalacije. Ne mogu da deinstaliram
UninstallUnknownEntry=Nepoznat unos (%1) se pojavio u izvjeötaju deinstalacije
ConfirmUninstall=éelite li da deinstalirate %1 kao i sve njegove komponente?
UninstallOnlyOnWin64=Ovu instalaciju je moguce deinstalirati samo na 64-bit Windows-u.
OnlyAdminCanUninstall=Ova instalacija se moûe deinstalirati samo kao administrator.
UninstallStatusLabel=Sacekajte da se %1 deinstalira sa racunara.
UninstalledAll=%1 je uspjeöno deinstaliran.
UninstalledMost=%1 deinstalacija uspjeöna.%n%nNeki elementi nijesu uklonjeni. Moûete ih sami ukloniti.
UninstalledAndNeedsRestart=Da zavröite sa deinstlacijom %1, restartujte vaö racunar.%n%néelite li restart sada?
UninstallDataCorrupted="%1" datoteka je oötecena. Ne mogu da deinstaliram

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Brisati dijeljenu datoteku?
ConfirmDeleteSharedFile2=Sistem je primijetio da sledecu dijeljenu datoteku viöe ne koristi nijedan program. éelite li da deinstaliram dijeljenu datoteku?%n%nAko je neki program koristio dijeljenu datoteku, moguce je da on viöe nece raditi. Ako nijesi siguran izaberi "Ne". Ostavljanje datoteke na vaöem racunaru ne moûete imati problema.
SharedFileNameLabel=Ime datoteke:
SharedFileLocationLabel=Lokacija:
WizardUninstalling=Status deinstlacije
StatusUninstalling=Deinstaliram %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instaliram %1.
ShutdownBlockReasonUninstallingApp=Deinstaliram %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 verzija %2
AdditionalIcons=Dodatne precice:
CreateDesktopIcon=Kreiraj precicu na &desktop 
CreateQuickLaunchIcon=Kreiraj precicu na paleti za &Brzo pokretanje
ProgramOnTheWeb=%1 na internetu
UninstallProgram=Deinstaliraj %1
LaunchProgram=Pokreni %1
AssocFileExtension=&Poveûi %1 sa datotekom %2 
AssocingFileExtension=Povezujem %1 sa datotekom %2 ...
AutoStartProgramGroupDescription=Pokretanje:
AutoStartProgram=Automatski pokreni %1
AddonHostProgramNotFound=%1 nije naden u direktorijumu koji ste izabrali.%n%néelite li da nastavim?
