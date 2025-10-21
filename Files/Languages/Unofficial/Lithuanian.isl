; *** Inno Setup version 6.5.0+ Lithuanian messages ***
;
; To download user-contributed translations of this file, go to:
;   https://jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
; Translated by Robertas Rimas (Loptar AT takas DOT lt)
; Corrected and updated by Rolandas Rudomanskis (rolandasr AT gmail DOT com)
; Corrected and updated to version 6.5.0+ by Dalius Guzauskas (aka Tichij) (tichij AT mail DOT com)

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Lietuvi<0173>
LanguageID=$0427
; LanguageCodePage should always be set if possible, even if this file is Unicode
; For English it's set to zero anyway because English only uses ASCII characters
LanguageCodePage=1257
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
SetupAppTitle=Diegimas
SetupWindowTitle=Diegimas - %1
UninstallAppTitle=Pašalinimas
UninstallAppFullTitle=„%1“ pašalinimas

; *** Misc. common
InformationTitle=Informacija
ConfirmTitle=Patvirtinimas
ErrorTitle=Klaida

; *** SetupLdr messages
SetupLdrStartupMessage=%1 diegimas. Norite tęsti?
LdrCannotCreateTemp=Negaliu sukurti laikinojo failo. Diegimas nutraukiamas
LdrCannotExecTemp=Negaliu įvykdyti failo laikinajame kataloge. Diegimas nutraukiamas
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nKlaida %2: %3
SetupFileMissing=Diegimo kataloge nerastas „%1“ failas. Pašalinkite šią problemą arba įsigykite naują programos kopiją.
SetupFileCorrupt=Įdiegiami failai sugadinti. Įsigykite naują programos kopiją.
SetupFileCorruptOrWrongVer=Įdiegiami failai yra sugadinti arba nesuderinami su diegimo programa. Ištaisykite problemą arba įsigykite naują programos kopiją.
InvalidParameter=Klaidingas parametras buvo gautas iš komandinės eilutės:%n%n%1
SetupAlreadyRunning=Diegimo programa jau yra paleista.
WindowsVersionNotSupported=Ši programa nesuderinama su Jūsų kompiuteryje įdiegta Windows versija.
WindowsServicePackRequired=Ši programa reikalauja %1 Service Pack %2 ar vėlesnės versijos.
NotOnThisPlatform=Ši programa negali būti paleista %1 aplinkoje.
OnlyOnThisPlatform=Ši programa turi būti leidžiama %1 aplinkoje.
OnlyOnTheseArchitectures=Ši programa gali būti įdiegta tik Windows versijose, turinčiose šias procesoriaus architektūras:%n%n%1
WinVersionTooLowError=Ši programa reikalauja %1 %2 ar vėlesnės versijos.
WinVersionTooHighError=Ši programa negali būti įdiegta %1 %2 ar vėlesnės versijos aplinkoje.
AdminPrivilegesRequired=Šios programos diegimui privalote būti prisijungęs Administratoriaus teisėmis.
PowerUserPrivilegesRequired=Šios programos diegimui privalote būti prisijungęs Administratoriaus arba „Power Users“ grupės nario teisėmis.
SetupAppRunningError=Diegimo programa aptiko, kad yra paleista „%1“.%n%nUždarykite visas paleistas šios programos kopijas ir, jei norite tęsti, paspauskite „Gerai“ arba „Nutraukti“, jei norite nutraukti diegimą.
UninstallAppRunningError=Pašalinimo programa aptiko, kad yra paleista „%1“.%n%nUždarykite visas paleistas šios programos kopijas ir, jei norite tęsti, paspauskite „Gerai“ arba „Nutraukti“, jei norite nutraukti diegimą.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Diegimo režimo pasirinkimas
PrivilegesRequiredOverrideInstruction=Pasirinkite diegimo režimą
PrivilegesRequiredOverrideText1=%1 gali būti įdiegta visiems naudotojams (reikalingos administratoriaus teisės) arba tik jums.
PrivilegesRequiredOverrideText2=%1 gali būti įdiegta arba tik jums arba visiems naudotojams (reikalingos administratoriaus teisės).
PrivilegesRequiredOverrideAllUsers=Įdiegti &visiems naudotojams
PrivilegesRequiredOverrideAllUsersRecommended=Įdiegti &visiems naudotojams (rekomenduojama)
PrivilegesRequiredOverrideCurrentUser=Įdiegti tik &man
PrivilegesRequiredOverrideCurrentUserRecommended=Įdiegti tik &man (rekomenduojama)

; *** Misc. errors
ErrorCreatingDir=Diegimo programa negali sukurti katalogo „%1“
ErrorTooManyFilesInDir=Neįmanoma sukurti failo „%1“ kataloge, nes jame per daug failų

; *** Setup common messages
ExitSetupTitle=Uždaryti diegimo programą
ExitSetupMessage=Diegimas nebaigtas. Jei baigsite dabar, programa nebus įdiegta.%n%nJūs galite paleisti diegimo programą kitą kartą, kad pabaigtumėte diegimą.%n%nUždaryti diegimo programą?
AboutSetupMenuItem=&Apie diegimo programą...
AboutSetupTitle=Apie diegimo programą
AboutSetupMessage=%1 versija %2%n%3%n%n%1 puslapis internete:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Atgal
ButtonNext=&Pirmyn >
ButtonInstall=Į&diegti
ButtonOK=Gerai
ButtonCancel=Nutraukti
ButtonYes=&Taip
ButtonYesToAll=Taip &viską
ButtonNo=&Ne
ButtonNoToAll=N&e nieko
ButtonFinish=&Pabaiga
ButtonBrowse=&Nurodyti...
ButtonWizardBrowse=Nu&rodyti...
ButtonNewFolder=&Naujas katalogas

; *** "Select Language" dialog messages
SelectLanguageTitle=Pasirinkite diegimo programos kalbą
SelectLanguageLabel=Pasirinkite diegimo metu naudojamą kalbą.

; *** Common wizard text
ClickNext=Paspauskite „Pirmyn“, jei norite tęsti, arba „Nutraukti“, jei norite išeiti iš diegimo programos.
BeveledLabel=
BrowseDialogTitle=Nurodykite katalogą
BrowseDialogLabel=Pasirinkite katalogą iš sąrašo ir paspauskite „Gerai“.
NewFolderName=Naujas katalogas

; *** "Welcome" wizard page
WelcomeLabel1=Sveiki! Čia „[name]“ diegimo programa.
WelcomeLabel2=Diegimo programa įdiegs „[name]“ Jūsų kompiuteryje.%n%nPrieš tęsiant diegimą, rekomenduojama uždaryti visas nereikalingas programas.

; *** "Password" wizard page
WizardPassword=Slaptažodis
PasswordLabel1=Šis diegimas yra apsaugotas slaptažodžiu.
PasswordLabel3=Įveskite slaptažodį ir spauskite „Pirmyn“, jei norite tęsti diegimą. Atkreipkite dėmesį: didžiosios ir mažosios raidės vertinamos skirtingai (case sensitive).
PasswordEditLabel=&Slaptažodis:
IncorrectPassword=Įvestas slaptažodis yra neteisingas. Pabandykite iš naujo.

; *** "License Agreement" wizard page
WizardLicense=Licencinė sutartis
LicenseLabel=Perskaitykite šią informaciją prieš tęsdami diegimą.
LicenseLabel3=Perskaitykite Licencijos sutartį. Prieš tęsdami diegimą Jūs turite sutikti su reikalavimais.
LicenseAccepted=Aš &sutinku su reikalavimais
LicenseNotAccepted=Aš &nesutinku su reikalavimais

; *** "Information" wizard pages
WizardInfoBefore=Informacija
InfoBeforeLabel=Perskaitykite šią informaciją prieš tęsiant diegimą.
InfoBeforeClickLabel=Kai būsite pasiruošęs tęsti diegimą, spauskite „Pirmyn“.
WizardInfoAfter=Informacija
InfoAfterLabel=Perskaitykite šią informaciją prieš tęsiant diegimą.
InfoAfterClickLabel=Spauskite „Pirmyn“, kai būsite pasiruošę tęsti diegimą.

; *** "User Information" wizard page
WizardUserInfo=Informacija apie naudotoją
UserInfoDesc=Įveskite naudotojo duomenis.
UserInfoName=&Naudotojo vardas:
UserInfoOrg=&Organizacija:
UserInfoSerial=&Serijinis numeris:
UserInfoNameRequired=Jūs privalote įvesti vardą.

; *** "Select Destination Location" wizard page
WizardSelectDir=Pasirinkite diegimo katalogą
SelectDirDesc=Kur turi būti įdiegta „[name]“?
SelectDirLabel3=Diegimo programa įdiegs „[name]“ į nurodytą katalogą.
SelectDirBrowseLabel=Norėdami tęsti diegimą spauskite „Pirmyn“. Jei norite pasirinkti kitą katalogą, spauskite „Nurodyti“.
DiskSpaceGBLabel=Reikia mažiausiai [gb] GB laisvos vietos kietajame diske.
DiskSpaceMBLabel=Reikia mažiausiai [mb] MB laisvos vietos kietajame diske.
CannotInstallToNetworkDrive=Diegimo programa negali diegti į tinklinį diską.
CannotInstallToUNCPath=Diegimo programa negali diegti į UNC tipo katalogą.
InvalidPath=Jūs privalote įrašyti pilną kelią su disko raide; pavyzdžiui:%n%nC:\APP%n% ir negalima nurodyti UNC tipo katalogą:%n%n\\Serveris\share
InvalidDrive=Diskas, kurį nurodėte, neegzistuoja arba yra neprieinamas. Nurodykite kitą diską ir/arba katalogą.
DiskSpaceWarningTitle=Nepakanka laisvos vietos diske
DiskSpaceWarning=Diegimui reikia bent %1 KB laisvos vietos, bet nurodytame diske yra tik %2 KB laisvos vietos.%n%nVis tiek norite tęsti?
DirNameTooLong=Katalogo pavadinimas ar kelias iki jo per ilgas.
InvalidDirName=Nekorektiškas katalogo pavadinimas.
BadDirName32=Katalogo pavadinime neturi būti simbolių:%n%n%1
DirExistsTitle=Tokio katalogo nėra
DirExists=Katalogas:%n%n%1%n%n jau yra. Vis tiek norite diegti programą tame kataloge?
DirDoesntExistTitle=Tokio katalogo nėra.
DirDoesntExist=Katalogas:%n%n%1%n%n neegzistuoja. Norite kad katalogas būtų sukurtas?

; *** "Select Components" wizard page
WizardSelectComponents=Komponentų pasirinkimas
SelectComponentsDesc=Kurie komponentai turi būti įdiegti?
SelectComponentsLabel2=Pažymėkite komponentus, kuriuos norite įdiegti; nuimkite žymes nuo komponentų, kurių nenorite diegti. Kai būsite pasiruošęs tęsti, spauskite „Pirmyn“.
FullInstallation=Pilnas visų komponentų diegimas
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Glaustas diegimas
CustomInstallation=Pasirinktinis diegimas
NoUninstallWarningTitle=Komponentai egzistuoja
NoUninstallWarning=Diegimo programa aptiko, kad šie komponentai jau įdiegti Jūsų kompiuteryje:%n%n%1%n%nJei nuimsite žymes nuo šių komponentų, jie vis tiek nebus ištrinti.%n%nVis tiek norite tęsti diegimą?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Dabartinis Jūsų pasirinkimas reikalauja [gb] GB laisvos vietos diske.
ComponentsDiskSpaceMBLabel=Dabartinis Jūsų pasirinkimas reikalauja [mb] MB laisvos vietos diske.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Nurodykite papildomus veiksmus
SelectTasksDesc=Kokius papildomus veiksmus reikia atlikti?
SelectTasksLabel2=Nurodykite papildomus veiksmus, kuriuos diegimo programa turės atlikti „[name]“ diegimo metu. Kai būsite pasiruošęs tęsti diegimą, spauskite „Pirmyn“.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Nurodykite „Start Menu“ katalogą
SelectStartMenuFolderDesc=Kur diegimo programa turėtų sukurti nuorodas?
SelectStartMenuFolderLabel3=Nuorodos bus sukurtos šiame „Start Menu“ kataloge.
SelectStartMenuFolderBrowseLabel=Norėdami tęsti diegimą spauskite „Pirmyn“. Jei norite parinkti kitą katalogą, spauskite „Nurodyti“.
MustEnterGroupName=Jūs privalote įvesti katalogo pavadinimą.
GroupNameTooLong=Katalogo pavadinimas ar kelias iki jo per ilgas.
InvalidGroupName=Katalogo pavadinimas yra nekorektiškas.
BadGroupName=Katalogo pavadinime neturi būti simbolių:%n%n%1
NoProgramGroupCheck2=&Nekurti „Start Menu“ katalogo

; *** "Ready to Install" wizard page
WizardReady=Pasirengta diegimui
ReadyLabel1=Diegimo programa pasirengusi diegti „[name]“ Jūsų kompiuteryje.
ReadyLabel2a=Spauskite „Įdiegti“, jei norite tęsti diegimą, arba „Atgal“, jeigu norite peržiūrėti nustatymus arba juos pakeisti.
ReadyLabel2b=Spauskite „Įdiegti“, jei norite tęsti diegimą.
ReadyMemoUserInfo=Naudotojo informacija:
ReadyMemoDir=Katalogas diegimui:
ReadyMemoType=Diegimo tipas:
ReadyMemoComponents=Pasirinkti komponentai:
ReadyMemoGroup=„Start Menu“ katalogas:
ReadyMemoTasks=Papildomi veiksmai:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Parsisiunčiami failai...
ButtonStopDownload=&Stabdyti parsisiuntimą
StopDownload=Ar tikrai norite sustabdyti parsisiuntimą?
ErrorDownloadAborted=Parsisiuntimas nutrauktas
ErrorDownloadFailed=Parsisiųsti nepavyko: %1 %2
ErrorDownloadSizeFailed=Nepavyko gauti dydžio: %1 %2
ErrorProgress=Netinkama eiga: %1 iš %2
ErrorFileSize=Neteisingas failo dydis: numatytas %1, rastas %2

; *** TExtractionWizardPage wizard page and ExtractArchive
ExtractingLabel=Failų išpakavimas...
ButtonStopExtraction=&Stabdyti išpakavimą
StopExtraction=Ar tikrai norite sustabdyti išpakavimą?
ErrorExtractionAborted=Išpakavimas nutrauktas
ErrorExtractionFailed=Nepavyko išpakuoti: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=Slaptažodis neteisingas
ArchiveIsCorrupted=Archyvas sugadintas
ArchiveUnsupportedFormat=Nepalaikomas archyvo formatas

; *** "Preparing to Install" wizard page
WizardPreparing=Pasirengimas diegimui
PreparingDesc=Diegimo programa pasirengusi „[name]“ diegimui Jūsų kompiuteryje.
PreviousInstallNotCompleted=Ankstesnės programos diegimas/šalinimas buvo neužbaigtas. Jums reikėtų perkrauti kompiuterį, kad užbaigtumėte diegimą.%n%nKai perkrausite kompiuterį, paleiskite diegimo programą dar kartą, kad pabaigtumėte „[name]“ diegimą.
CannotContinue=Diegimas negali būti tęsiamas. Paspauskite „Nutraukti“ diegimo užbaigimui.
ApplicationsFound=Šios programos naudoja failus, kurie turi būti perrašyti diegimo metu. Rekomenduojama leisti diegimo programai automatiškai uždaryti šias programas.
ApplicationsFound2=Šios programos naudoja failus, kurie turi būti perrašyti diegimo metu. Rekomenduojama leisti diegimo programai automatiškai uždaryti šias programas. Po to, kai diegimas bus baigtas, diegimo programa bandys iš naujo paleisti šias programas.
CloseApplications=&Automatiškai uždaryti programas
DontCloseApplications=&Neuždarinėti programų
ErrorCloseApplications=Diegimo programai nepavyko automatiškai uždaryti visų programų. Prieš tęsiant diegimą, rekomeduojama uždaryti visas programas, naudojančias failus, kurie turi būti perrašyti diegimo metu.
PrepareToInstallNeedsRestart=Diegimo programai reikia perkrauti kompiuterį. Po perkovimo, vėl paleiskite diegimo programą „[name]“ diegimo užbaigimui.%n%nNorite perkrauti jį dabar?

; *** "Installing" wizard page
WizardInstalling=Vyksta diegimas
InstallingLabel=Palaukite kol diegimo programa įdiegs „[name]“ Jūsų kompiuteryje.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=„[name]“ diegimas baigtas
FinishedLabelNoIcons=Diegimo programa baigė „[name]“ diegimą Jūsų kompiuteryje.
FinishedLabel=Diegimo programa baigė „[name]“ diegimą Jūsų kompiuteryje. Programa gali būti paleista pasirinkus atitinkamas nuorodas.
ClickFinish=Spauskite „Pabaiga“, kad uždarytumėte diegimo programą.
FinishedRestartLabel=„[name]“ diegimo užbaigimui, reikia perkrauti kompiuterį. Norite perkrauti jį dabar?
FinishedRestartMessage=„[name]“ diegimo užbaigimui, reikia perkrauti kompiuterį.%n%nNorite perkrauti jį dabar?
ShowReadmeCheck=Taip, aš norėčiau perskaityti „README“ failą
YesRadio=&Taip, aš noriu perkrauti kompiuterį dabar
NoRadio=&Ne, aš perkrausiu kompiuterį vėliau
; used for example as 'Run MyProg.exe'
RunEntryExec=Vykdyti „%1“
; used for example as 'View Readme.txt'
RunEntryShellExec=Peržiūrėti „%1“

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Diegimo programai reikia kito diskelio
SelectDiskLabel2=Idėkite diskelį %1 ir spauskite „Gerai“.%n%nJeigu reikiami failai gali būti rasti kitame kataloge, nei pavaizduota žemiau, įveskite teisingą kelią arba spauskite „Nurodyti“.
PathLabel=&Katalogas:
FileNotInDir2=„%1“ failas nerastas „%2“ kataloge. Įdėkite teisingą diskelį arba nurodykite teisingą kelią.
SelectDirectoryLabel=Nurodykite kito diskelio vietą.

; *** Installation phase messages
SetupAborted=Diegimas nebuvo baigtas.%n%nPašalinkite priežąstį ir pakartokite diegimą vėl.
AbortRetryIgnoreSelectAction=Pasirinkite veiksmą
AbortRetryIgnoreRetry=Bandy&ti vėl
AbortRetryIgnoreIgnore=&Ignoruoti klaidą ir tęsti
AbortRetryIgnoreCancel=Nutraukti diegimą
RetryCancelSelectAction=Pasirinkite veiksmą
RetryCancelRetry=Bandy&ti vėl
RetryCancelCancel=Nutraukti

; *** Installation status messages
StatusClosingApplications=Uždaromos programos...
StatusCreateDirs=Kuriami katalogai...
StatusExtractFiles=Išpakuojami failai...
StatusDownloadFiles=Parsisiunčiami failai...
StatusCreateIcons=Kuriamos nuorodos...
StatusCreateIniEntries=Kuriami INI įrašai...
StatusCreateRegistryEntries=Kuriami registro įrašai...
StatusRegisterFiles=Registruojami failai...
StatusSavingUninstall=Išsaugoma informacija programos pašalinimui...
StatusRunProgram=Baigiamas diegimas...
StatusRestartingApplications=Iš naujo paleidžiamos programos...
StatusRollback=Anuliuojami pakeitimai...

; *** Misc. errors
ErrorInternal2=Vidinė klaida: %1
ErrorFunctionFailedNoCode=%1 nepavyko
ErrorFunctionFailed=%1 nepavyko; kodas %2
ErrorFunctionFailedWithMessage=%1 nepavyko; kodas %2.%n%3
ErrorExecutingProgram=Nepavyko paleisti failo:%n%1

; *** Registry errors
ErrorRegOpenKey=Klaida skaitant registro įrašą:%n%1\%2
ErrorRegCreateKey=Klaida sukuriant registro įrašą:%n%1\%2
ErrorRegWriteKey=Klaida rašant registro įrašą:%n%1\%2

; *** INI errors
ErrorIniEntry=Klaida rašant INI įrašą „%1“ faile.

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=Pralei&sti šį failą (nerekomenduojama)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignoruoti klaidą ir tęsti (nerekomenduojama)
SourceIsCorrupted=Pradinis failas sugadintas
SourceDoesntExist=Pradinio failo „%1“ nėra
SourceVerificationFailed=Nepavyko patikrinti pradinio failo: %1
VerificationSignatureDoesntExist=Parašo failo „%1“ nėra
VerificationSignatureInvalid=Parašo failas „%1“ sugadintas
VerificationKeyNotFound=Parašo failas „%1“ naudoja nežinomą raktą
VerificationFileNameIncorrect=Neteisingas failo vardas
VerificationFileTagIncorrect=Neteisinga failo žymė
VerificationFileSizeIncorrect=Neteisingas failo dydis
VerificationFileHashIncorrect=Neteisinga failo „hash“ reikšmė
ExistingFileReadOnly2=Esamas failas yra pažymėtas „Tik skaitymui“ todėl negali būti pakeistas.
ExistingFileReadOnlyRetry=Pašalinkite at&ributą „Tik skaitymui“ ir bandykite vėl
ExistingFileReadOnlyKeepExisting=Pali&kti esamą failą
ErrorReadingExistingDest=Skaitant esamą failą įvyko klaida:
FileExistsSelectAction=Pasirinkite veiksmą
FileExists2=Toks failas jau yra.
FileExistsOverwriteExisting=&Perrašyti esamą failą
FileExistsKeepExisting=Pali&kti esamą failą
FileExistsOverwriteOrKeepAll=&Daryti taip ir esant kitiems konfliktams
ExistingFileNewerSelectAction=Pasirinkite veiksmą
ExistingFileNewer2=Esamas failas yra naujesnis už tą, kurį diegimo programa bando įrašyti.
ExistingFileNewerOverwriteExisting=&Perrašyti esamą failą
ExistingFileNewerKeepExisting=Pali&kti esamą failą (rekomenduojama)
ExistingFileNewerOverwriteOrKeepAll=&Daryti taip ir esant kitiems konfliktams
ErrorChangingAttr=Keičiant failo atributus įvyko klaida:
ErrorCreatingTemp=Kuriant failą pasirinktame kataloge įvyko klaida:
ErrorReadingSource=Skaitant diegiamąjį failą įvyko klaida:
ErrorCopying=Kopijuojant failą įvyko klaida:
ErrorDownloading=Parsisiunčiant failą įvyko klaida:
ErrorExtracting=Išpakuojant archyvą įvyko klaida:
ErrorReplacingExistingFile=Perrašant esamą failą įvyko klaida:
ErrorRestartReplace=Perkrovimas/Perrašymas nepavyko:
ErrorRenamingTemp=Pervadinant failą pasirinktame kataloge įvyko klaida:
ErrorRegisterServer=Nepavyko užregistruoti DLL/OCX bibliotekos: „%1“
ErrorRegSvr32Failed=RegSvr32 registracijos klaida %1
ErrorRegisterTypeLib=Nepavyko užregistruoti tipų bibliotekos: „%1“

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bitų
UninstallDisplayNameMark64Bit=64-bitų
UninstallDisplayNameMarkAllUsers=Visiems naudotojams
UninstallDisplayNameMarkCurrentUser=Esamam naudotojui

; *** Post-installation errors
ErrorOpeningReadme=Bandant atidaryti „README“ failą įvyko klaida.
ErrorRestartingComputer=Diegimo programa negali perkrauti kompiuterio. Perkraukite kompiuterį įprastu būdu.

; *** Uninstaller messages
UninstallNotFound=„%1“ failo nėra. Pašalinti neįmanoma.
UninstallOpenError=„%1“ failas negali būti atidarytas. Pašalinti neįmanoma.
UninstallUnsupportedVer=Pašalinimo žurnalo failas „%1“ yra pašalinimo programai nesuprantamo formato. Pašalinti neįmanoma.
UninstallUnknownEntry=Nežinomas įrašas (%1) rastas pašalinimo žurnalo faile.
ConfirmUninstall=Esate tikri, kad norite pašalinti „%1“ ir visus priklausančius komponentus?
UninstallOnlyOnWin64=Šis diegimas gali būti pašalintas tik 64 bitų Windows sistemose.
OnlyAdminCanUninstall=Tik administratoriaus teises turintis naudotojas gali pašalinti programą.
UninstallStatusLabel=Palaukite, kol „%1“ bus pašalinta iš Jūsų kompiuterio.
UninstalledAll=„%1“ buvo sėkmingai pašalinta iš Jūsų kompiuterio.
UninstalledMost=„%1“ pašalinimas sėkmingai baigtas.%n%nKai kurie elementai nebuvo ištrinti - juos galite pašalinti rankiniu būdu.
UninstalledAndNeedsRestart=„%1“ pašalinimui užbaigti Jūsų kompiuteris turi būti perkrautas.%n%nNorite perkrauti jį dabar?
UninstallDataCorrupted=„%1“ failas yra sugadintas. Programos pašalinti neįmanoma.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Ištrinti bendruosius failus?
ConfirmDeleteSharedFile2=Aptikta, kad jokia programa nenaudoja bendrųjų failų. Norite ištrinti bendruosius failus? %n%nJeigu kurios nors programos naudoja šiuos failus, ir jie bus ištrinti, tos programos gali veikti neteisingai. Jeigu nesate tikras - spauskite „Ne“. Failo palikimas Jūsų kompiuteryje nesukels jokių problemų.
SharedFileNameLabel=Failo vardas:
SharedFileLocationLabel=Vieta:
WizardUninstalling=Pašalinimo eiga
StatusUninstalling=Šalinama „%1“...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Diegiama „%1“.
ShutdownBlockReasonUninstallingApp=Šalinama „%1“.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versija %2
AdditionalIcons=Papildomos nuorodos:
CreateDesktopIcon=Sukurti nuorodą &Darbalaukyje
CreateQuickLaunchIcon=Sukurti Sparčiosios &Paleisties nuorodą
ProgramOnTheWeb=„%1“ žiniatinklyje
UninstallProgram=Pašalinti „%1“
LaunchProgram=Paleisti „%1“
AssocFileExtension=&Susieti „%1“ programą su failo plėtiniu %2
AssocingFileExtension=„%1“ programa susiejama su failo plėtiniu %2...
AutoStartProgramGroupDescription=Automatinė paleistis:
AutoStartProgram=Automatiškai paleisti „%1“
AddonHostProgramNotFound=„%1“ nerasta Jūsų nurodytame kataloge.%n%nVis tiek norite tęsti?
