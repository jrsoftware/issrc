; *** Inno Setup version 5.1.11+ Lithuanian messages ***
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/is3rdparty.php
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
; Translated by Robertas Rimas (Loptar AT takas DOT lt)
; Corrected and updated by Rolandas Rudomanskis (rolandasr AT gmail DOT com)

[LangOptions]
LanguageName=Lietuvi<0173>
LanguageID=$0427
LanguageCodePage=1257
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
DialogFontName=Tahoma
DialogFontSize=8
WelcomeFontName=Tahoma
WelcomeFontSize=12
TitleFontName=Tahoma
TitleFontSize=24
CopyrightFontName=Tahoma
CopyrightFontSize=8

[Messages]

; *** Application titles
SetupAppTitle=Ádiegimas
SetupWindowTitle=Ádiegimas - %1
UninstallAppTitle=Paðalinimas
UninstallAppFullTitle=„%1“ paðalinimas

; *** Misc. common
InformationTitle=Informacija
ConfirmTitle=Patvirtinimas
ErrorTitle=Klaida

; *** SetupLdr messages
SetupLdrStartupMessage=%1 ádiegimas. Ar norite tæsti?
LdrCannotCreateTemp=Negaliu sukurti laikinosios bylos. Ádiegimas nutraukiamas
LdrCannotExecTemp=Negaliu ávykdyti bylos laikinajame kataloge. Ádiegimas nutraukiamas

; *** Startup error messages
LastErrorMessage=%1.%n%nKlaida %2: %3
SetupFileMissing=Ádiegimo kataloge byla „%1“ nerasta. Praðome iðtaisyti ðià problemà arba ásigyti naujà programos kopijà.
SetupFileCorrupt=Ádiegimo bylos sugadintos. Ásigykite naujà programos kopijà.
SetupFileCorruptOrWrongVer=Ádiegiamos bylos yra sugadintos arba nesuderinamos su ádiegimo programa. Iðtaisykite problemà arba ásigykite naujà programos kopijà.
NotOnThisPlatform=Ði programa negali bûti paleista %1 aplinkoje.
OnlyOnThisPlatform=Ði programa turi bûti leidþiama %1 aplinkoje.
OnlyOnTheseArchitectures=Ði programa gali bûti ádiegta tik Windows versijose, turinèiose ðias procesoriaus architektûras:%n%n%1
MissingWOW64APIs=Windows versija, kurià Jûs naudojate, neturi funkcijø, reikalingø atlikti 64 bitø ádiegimams. Tam, kad iðspræstumëte ðià problemà, ádiekite Service Pack %1.
WinVersionTooLowError=Ði programa reikalauja %1 %2 ar vëlesnës versijos.
WinVersionTooHighError=Ði programa negali bûti ádiegta %1 %2 ar vëlesnës versijos aplinkoje.
AdminPrivilegesRequired=Ðios programos ádiegimui privalote bûti prisijungæs Administratoriaus teisëmis.
PowerUserPrivilegesRequired=Ðios programos ádiegimui privalote bûti prisijungæs Administratoriaus arba Power Users grupës nario teisëmis.
SetupAppRunningError=Ádiegimo programa aptiko, kad yra paleista „%1“.%n%nUþdarykite visas paleistas ðios programos kopijas ir, jei norite tæsti, paspauskite „Gerai“ arba „Atðaukti“, jei norite nutraukti ádiegimà.
UninstallAppRunningError=Paðalinimo programa aptiko, kad yra paleista „%1“.%n%nUþdarykite visas paleistas ðios programos kopijas ir, jei norite tæsti, paspauskite „Gerai“ arba „Atðaukti“, jei norite nutraukti ádiegimà.

; *** Misc. errors
ErrorCreatingDir=Ádiegimo programa negali sukurti katalogo „%1“
ErrorTooManyFilesInDir=Neámanoma sukurti bylos kataloge „%1“, nes jame per daug bylø

; *** Setup common messages
ExitSetupTitle=Uþdaryti ádiegimo programà
ExitSetupMessage=Ádiegimas nebaigtas. Jei baigsite dabar, programa nebus ádiegta.%n%nJûs galite paleisti ádiegimo programà kità kartà, kad pabaigtumëte ádiegimà.%n%nUþdaryti ádiegimo programà?
AboutSetupMenuItem=&Apie ádiegimo programà...
AboutSetupTitle=Apie ádiegimo programà
AboutSetupMessage=%1 versija %2%n%3%n%n%1 puslapis internete:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Atgal
ButtonNext=&Pirmyn >
ButtonInstall=Á&diegti
ButtonOK=Gerai
ButtonCancel=Atðaukti
ButtonYes=&Taip
ButtonYesToAll=Taip &viskà
ButtonNo=&Ne
ButtonNoToAll=N&e nieko
ButtonFinish=&Pabaiga
ButtonBrowse=&Nurodyti...
ButtonWizardBrowse=Nu&rodyti...
ButtonNewFolder=&Naujas katalogas

; *** "Select Language" dialog messages
SelectLanguageTitle=Nurodykite ádiegimo programos kalbà
SelectLanguageLabel=Nurodykite ádiegimo metu naudojamà kalbà:

; *** Common wizard text
ClickNext=Paspauskite „Pirmyn“, jei norite tæsti, arba „Atðaukti“, jei norite iðeiti ið ádiegimo programos.
BeveledLabel=
BrowseDialogTitle=Nurodykite katalogà
BrowseDialogLabel=Pasirinkite katalogà ið sàraðo ir paspauskite „Gerai“.
NewFolderName=Naujas katalogas

; *** "Welcome" wizard page
WelcomeLabel1=Sveiki! Èia „[name]“ ádiegimo programa.
WelcomeLabel2=Ádiegimo programa ádiegs „[name]“ Jûsø kompiuteryje.%n%nPrieð tæsiant ádiegimà, rekomenduojama uþdaryti visas nereikalingas programas.

; *** "Password" wizard page
WizardPassword=Slaptaþodis
PasswordLabel1=Ðis ádiegimas yra apsaugotas slaptaþodþiu.
PasswordLabel3=Áveskite slaptaþodá ir spauskite „Pirmyn“, jei norite tæsti ádiegimà. Atkreipkite dëmesá: didþiosios ir maþosios raidës vertinamos skirtingai (case sensitive).
PasswordEditLabel=&Slaptaþodis:
IncorrectPassword=Ávestas slaptaþodis yra neteisingas. Praðome bandyti ið naujo.

; *** "License Agreement" wizard page
WizardLicense=Licencinë sutartis
LicenseLabel=Praðome perskaityti ðià informacijà prieð tæsdami ádiegimà.
LicenseLabel3=Praðome perskaityti Licencijos sutartá. Prieð tæsdami ádiegimà Jûs turite sutikti su reikalavimais.
LicenseAccepted=Að &sutinku su reikalavimais
LicenseNotAccepted=Að &nesutinku su reikalavimais

; *** "Information" wizard pages
WizardInfoBefore=Informacija
InfoBeforeLabel=Praðome perskaityti ðià informacijà prieð tæsiant ádiegimà.
InfoBeforeClickLabel=Kai bûsite pasiruoðæs tæsti ádiegimà, spauskite „Pirmyn“.
WizardInfoAfter=Informacija
InfoAfterLabel=Praðome perskaityti ðià informacijà prieð tæsiant ádiegimà.
InfoAfterClickLabel=Kai bûsite pasiruoðæs tæsti ádiegimà, spauskite „Pirmyn“.

; *** "User Information" wizard page
WizardUserInfo=Informacija apie vartotojà
UserInfoDesc=Praðome ávesti vartotojo duomenis.
UserInfoName=&Vartotojo vardas:
UserInfoOrg=&Organizacija:
UserInfoSerial=&Serijinis numeris:
UserInfoNameRequired=Jûs privalote ávesti vardà.

; *** "Select Destination Location" wizard page
WizardSelectDir=Pasirinkite ádiegimo katalogà
SelectDirDesc=Kur turi bûti ádiegta „[name]“?
SelectDirLabel3=Ádiegimo programa ádiegs „[name]“ á nurodytà katalogà.
SelectDirBrowseLabel=Norëdami tæsti ádiegimà spauskite „Pirmyn“. Jei norite pasirinkti kità katalogà, spauskite „Nurodyti“.
DiskSpaceMBLabel=Reikia maþiausiai [mb] MB laisvos vietos kietajame diske.
ToUNCPathname=Ádiegimo programa negali ádiegti á UNC tipo katalogà. Jeigu bandote ádiegti programà tinkle, reikia sukurti tinkliná diskà ir nurodyti reikiamà katalogà.
InvalidPath=Jûs privalote áraðyti pilnà kelià su disko raide; pavyzdþiui:%n%nC:\APP%n% ir negalima nurodyti UNC tipo katalogà:%n%n\\Serveris\share
InvalidDrive=Diskas, kurá nurodëte, neegzistuoja arba yra neprieinamas. Praðome nurodyti kità diskà ir/arba katalogà.
DiskSpaceWarningTitle=Nepakanka laisvos vietos diske
DiskSpaceWarning=Ádiegimas reikalauja bent %1 KB laisvos vietos, bet nurodytame diske yra tik %2 KB laisvos vietos.%n%nAr Jûs vis tiek norite tæsti?
DirNameTooLong=Katalogo pavadinimas ar kelias iki jo per ilgas.
InvalidDirName=Nekorektiðkas katalogo pavadinimas.
BadDirName32=Katalogo pavadinime neturi bûti simboliø:%n%n%1
DirExistsTitle=Toks katalogas egzistuoja
DirExists=Katalogas:%n%n%1%n%n jau egzistuoja. Ar vistiek norite ádiegti programà tame kataloge?
DirDoesntExistTitle=Toks katalogas neegzistuoja.
DirDoesntExist=Katalogas:%n%n%1%n%n neegzistuoja. Ar norite kad katalogas bûtø sukurtas?

; *** "Select Components" wizard page
WizardSelectComponents=Komponentø pasirinkimas
SelectComponentsDesc=Kurie komponentai turi bûti ádiegti?
SelectComponentsLabel2=Paþymëkite komponentus, kuriuos norite ádiegti; nuimkite þymes nuo komponentø, kuriø nenorite ádiegti. Kai bûsite pasiruoðæs tæsti, spauskite „Pirmyn“.
FullInstallation=Pilnas visø komponentø ádiegimas
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Glaustas ádiegimas
CustomInstallation=Pasirinktinis ádiegimas
NoUninstallWarningTitle=Komponentai egzistuoja
NoUninstallWarning=Ádiegimo programa aptiko, kad ðie komponentai jau ádiegti Jûsø kompiuteryje:%n%n%1%n%nJei nuimsite þymes nuo ðiø komponentø, jie vis tiek nebus iðtrinti.%n%nAr vis tiek norite tæsti ádiegimà?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Dabartinis Jûsø pasirinkimas reikalauja [mb] MB laisvos vietos diske.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Nurodykite papildomas operacijas
SelectTasksDesc=Kokias papildomas operacijas reikia ávykdyti?
SelectTasksLabel2=Nurodykite papildomas operacijas, kurias ádiegimo programa ávykdys, kai bus diegiama „[name]“. Kai bûsite pasiruoðæs tæsti ádiegimà, spauskite „Pirmyn“.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Nurodykite „Start Menu“ katalogà
SelectStartMenuFolderDesc=Kur ádiegimo programa turëtø sukurti nuorodas?
SelectStartMenuFolderLabel3=Nuorodos bus sukurtos ðiame „Start Menu“ kataloge.
SelectStartMenuFolderBrowseLabel=Norëdami tæsti ádiegimà spauskite „Pirmyn“. Jei norite pasirinkti kità katalogà, spauskite „Nurodyti“.
MustEnterGroupName=Jûs privalote ávesti katalogo pavadinimà.
GroupNameTooLong=Katalogo pavadinimas ar kelias iki jo per ilgas.
InvalidGroupName=Katalogo pavadinimas yra nekorektiðkas
BadGroupName=Katalogo pavadinime neturi bûti simboliø:%n%n%1
NoProgramGroupCheck2=&Nekurti „Start Menu“ katalogo

; *** "Ready to Install" wizard page
WizardReady=Pasirengta ádiegimui
ReadyLabel1=Ádiegimo programa pasirengusi ádiegti „[name]“ Jûsø kompiuteryje.
ReadyLabel2a=Spauskite „Ádiegti“, jei norite tæsti ádiegimà, arba „Atgal“, jeigu norite perþiûrëti nustatymus arba juos pakeisti.
ReadyLabel2b=Spauskite „Ádiegti“, jei norite tæsti ádiegimà.
ReadyMemoUserInfo=Vartotojo informacija:
ReadyMemoDir=Katalogas ádiegimui:
ReadyMemoType=Ádiegimo tipas:
ReadyMemoComponents=Pasirinkti komponentai:
ReadyMemoGroup=„Start Menu“ katalogas:
ReadyMemoTasks=Papildomos operacijos:

; *** "Preparing to Install" wizard page
WizardPreparing=Pasirengimas ádiegimui
PreparingDesc=Ádiegimo programa pasirengusi „[name]“ ádiegimui Jûsø kompiuteryje.
PreviousInstallNotCompleted=Ankstesnës programos ádiegimas/ðalinimas buvo neuþbaigtas. Jums reikëtø perkrauti kompiuterá, kad uþbaigtumëte ádiegimà.%n%nKai perkrausite kompiuterá, paleiskite ádiegimo programà dar kartà, kad pabaigtumëte „[name]“ ádiegimà.
CannotContinue=Ádiegimas negali bûti tæsiamas. Praðome paspausti „Atðaukti“, kad baigtumëte ádiegimà.

; *** "Installing" wizard page
WizardInstalling=Ádiegimas vyksta
InstallingLabel=Praðau palaukti kol ádiegimo programa ádiegs „[name]“ Jûsø kompiuteryje.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=„[name]“ ádiegimas baigtas
FinishedLabelNoIcons=Ádiegimo programa baigë „[name]“ ádiegimà Jûsø kompiuteryje.
FinishedLabel=Ádiegimo programa baigë „[name]“ ádiegimà Jûsø kompiuteryje. Programa gali bûti paleista pasirinkus atitinkamas nuorodas.
ClickFinish=Spauskite „Pabaiga“, kad uþdarytumëte ádiegimo programà.
FinishedRestartLabel=Sëkmingam „[name]“ ádiegimui, reikëtø perkrauti kompiuterá. Ar norite perkrauti já dabar?
FinishedRestartMessage=Sëkmingam „[name]“ ádiegimui, reikëtø perkrauti kompiuterá.%n%nAr norite perkrauti já dabar?
ShowReadmeCheck=Taip, að norëèiau perskaityti „README“ bylà
YesRadio=&Taip, að noriu perkrauti kompiuterá dabar
NoRadio=&Ne, að perkrausiu kompiuterá vëliau
; used for example as 'Run MyProg.exe'
RunEntryExec=Vykdyti „%1“
; used for example as 'View Readme.txt'
RunEntryShellExec=Perþiûrëti „%1“

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Ádiegimo programai reikia kito diskelio
SelectDiskLabel2=Idëkite diskelá %1 ir spauskite „Gerai“.%n%nJeigu reikiamos bylos gali bûti surastos kitame kataloge, nei pavaizduota þemiau, áveskite teisingà kelià arba spauskite „Nurodyti“.
PathLabel=&Katalogas:
FileNotInDir2=Byla „%1“ nesurasta kataloge "%2". Praðome ádëti teisingà diskelá arba nurodyti teisingà kelià.
SelectDirectoryLabel=Praðome nurodyti kito diskelio vietà.

; *** Installation phase messages
SetupAborted=Ádiegimas nebuvo baigtas.%n%nPraðome iðspræsti problemà ir paleisti ádiegimo programà vëliau.
EntryAbortRetryIgnore=Spauskite „Retry“, jeigu norite bandyti vël, „Ignore“ - tæsti vistiek arba „Abort“, kad nutrauktumëte ádiegimà.

; *** Installation status messages
StatusCreateDirs=Kuriami katalogai...
StatusExtractFiles=Iðpakuojamos bylos...
StatusCreateIcons=Kuriamos nuorodos...
StatusCreateIniEntries=Kuriami INI áraðai...
StatusCreateRegistryEntries=Kuriami registro áraðai...
StatusRegisterFiles=Registruojamos bylos...
StatusSavingUninstall=Iðsaugoma informacija programos paðalinimui...
StatusRunProgram=Baigiamas ádiegimas...
StatusRollback=Anuliuojami pakeitimai...

; *** Misc. errors
ErrorInternal2=Vidinë klaida: %1
ErrorFunctionFailedNoCode=%1 nepavyko
ErrorFunctionFailed=%1 nepavyko; kodas %2
ErrorFunctionFailedWithMessage=%1 nepavyko; kodas %2.%n%3
ErrorExecutingProgram=Nepavyko paleisti bylos:%n%1

; *** Registry errors
ErrorRegOpenKey=Klaida skaitant registro áraðà:%n%1\%2
ErrorRegCreateKey=Klaida sukuriant registro áraðà:%n%1\%2
ErrorRegWriteKey=Klaida raðant registro áraðà:%n%1\%2

; *** INI errors
ErrorIniEntry=Klaida raðant INI áraðà byloje "%1".

; *** File copying errors
FileAbortRetryIgnore=Spauskite „Retry“, jeigu norite bandyti dar kartà, „Ignore“ - praleisti bylà (nerekomenduojama) arba „Abort“ - nutraukti ádiegimà.
FileAbortRetryIgnore2=Spauskite Retry, jeigu norite bandyti dar kartà, „Ignore“ - tæsti vistiek (nerekomenduojama) arba „Abort“ - nutraukti ádiegimà.
SourceIsCorrupted=Byla sugadinta
SourceDoesntExist=Byla "%1" neegzistuoja
ExistingFileReadOnly=Egzistuojanti byla turi tik skaitymo atributus.%n%nSpauskite „Retry“ ðio atributo iðtrynimui ir bandyti vël, „Ignore“ - praleisti bylà arba „Abort“ - nutraukti ádiegimà.
ErrorReadingExistingDest=Klaida ávyko skaitant bylà:
FileExists=Tokia byla jau egzistuoja.%n%nAr norite, kad ádiegimo programa perraðytø bylà?
ExistingFileNewer=Egzistuojanti byla yra naujesnë uþ tà, kurià ádiegimo programa bando áraðyti. Rekomenduojama palikti esanèià naujesnæ bylà.%n%nAr norite palikti naujesnæ bylà?
ErrorChangingAttr=Klaida ávyko keièiant bylos atributus:
ErrorCreatingTemp=Klaida ávyko kuriant bylà pasirinktame kataloge:
ErrorReadingSource=Klaida ávyko skaitant diegiamàjà bylà:
ErrorCopying=Klaida ávyko kopijuojant bylà:
ErrorReplacingExistingFile=Klaida ávyko perraðant egzistuojanèià bylà:
ErrorRestartReplace=Perkrovimas/Perraðymas nepavyko:
ErrorRenamingTemp=Klaida ávyko pervadinant bylà pasirinktame kataloge:
ErrorRegisterServer=Nepavyko uþregistruoti DLL/OCX bibliotekos: %1
ErrorRegSvr32Failed=RegSvr32 registracijos klaida %1
ErrorRegisterTypeLib=Nepavyko uþregistruoti tipø bibliotekos: %1

; *** Post-installation errors
ErrorOpeningReadme=Klaida ávyko bandant atidaryti „README“ bylà.
ErrorRestartingComputer=Ádiegimo programa negali perkrauti kompiuterio. Praðome perkrauti kompiuterá áprastu bûdu.

; *** Uninstaller messages
UninstallNotFound=Byla "%1" neegzistuoja. Paðalinti neámanoma.
UninstallOpenError=Byla "%1" negali bûti atidaryta. Paðalinti neámanoma.
UninstallUnsupportedVer=Paðalinimo log byla „%1“ yra formato, kurio nesupranta paðalinimo programa. Paðalinti neámanoma.
UninstallUnknownEntry=Neþinomas áraðas (%1) rastas paðalinimo log byloje
ConfirmUninstall=Ar esate tikri, kad norite paðalinti „%1“ ir visus priklausanèius komponentus?
UninstallOnlyOnWin64=Ðis ádiegimas gali bûti anuliuotas tik 64 bitø Windows sistemose.
OnlyAdminCanUninstall=Tik administratoriaus teises turintis vartotojas gali paðalinti programà.
UninstallStatusLabel=Praðome palaukti, kol „%1“ bus paðalinta ið Jûsø kompiuterio.
UninstalledAll=„%1“ buvo sëkmingai paðalinta ið Jûsø kompiuterio.
UninstalledMost=„%1“ paðalinimas sëkmingai baigtas.%n%nKai kurie elementai nebuvo iðtrinti - juos galite paðalinti rankiniu bûdu.
UninstalledAndNeedsRestart=„%1“ paðalinimui uþbaigti Jûsø kompiuteris turi bûti perkrautas.%n%nAr norite perkrauti já dabar?
UninstallDataCorrupted=„%1“ byla yra sugadinta. Programos paðalinti neámanoma.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Iðtrinti bendràsias bylas?
ConfirmDeleteSharedFile2=Aptikta, kad jokia programa nenaudoja bendrøjø bylø. Ar norite iðtrinti bendràsias bylas? %n%nJeigu kurios nors programos naudoja ðias bylas, ir jos bus iðtrintos, tos programos gali veikti neteisingai. Jeigu nesate tikras - spauskite „Ne“. Bylos palikimas Jûsø kompiuteryje nesukels jokiø problemø.
SharedFileNameLabel=Bylos pavadinimas:
SharedFileLocationLabel=Vieta:
WizardUninstalling=Paðalinimo eiga
StatusUninstalling=Ðalinama „%1“...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versija %2
AdditionalIcons=Papildomos piktogramos:
CreateDesktopIcon=Rodyti piktogramà &Darbalaukyje
CreateQuickLaunchIcon=Rodyti Sparèiosios &Paleisties piktogramà
ProgramOnTheWeb=„%1“ þiniatinklyje
UninstallProgram=Paðalinti „%1“
LaunchProgram=Paleisti „%1“
AssocFileExtension=&Susieti „%1“ programà su bylos plëtiniu %2
AssocingFileExtension=„%1“ programa susiejama su bylos plëtiniu %2...
