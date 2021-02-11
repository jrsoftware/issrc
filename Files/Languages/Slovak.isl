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
SetupAppTitle=Sprievodca inštaláciou
SetupWindowTitle=Sprievodca inštaláciou - %1
UninstallAppTitle=Sprievodca odinštaláciou
UninstallAppFullTitle=Sprievodca odinštaláciou - %1

; *** Misc. common
InformationTitle=Informácie
ConfirmTitle=Potvrdenie
ErrorTitle=Chyba

; *** SetupLdr messages
SetupLdrStartupMessage=Víta Vás Sprievodca inštaláciou produktu %1. Prajete si pokraèova?
LdrCannotCreateTemp=Nie je moné vytvori doèasnı súbor. Sprievodca inštaláciou sa ukonèí
LdrCannotExecTemp=Nie je moné spusti súbor v doèasnom adresári. Sprievodca inštaláciou sa ukonèí
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nChyba %2: %3
SetupFileMissing=Inštalaènı adresár neobsahuje súbor %1. Opravte túto chybu, alebo si zaobstarajte novú kópiu tohto produktu.
SetupFileCorrupt=Súbory Sprievodcu inštaláciou sú poškodené. Zaobstarajte si novú kópiu tohto produktu.
SetupFileCorruptOrWrongVer=Súbory Sprievodcu inštaláciou sú poškodené alebo sa nezhodujú s touto verziou Sprievodcu inštaláciou. Opravte túto chybu, alebo si zaobstarajte novú kópiu tohto produktu.
InvalidParameter=Nesprávny parameter na príkazovom riadku: %n%n%1
SetupAlreadyRunning=Inštalácia u prebieha.
WindowsVersionNotSupported=Tento program nepodporuje vašu verziu systému Windows.
WindowsServicePackRequired=Tento program vyaduje %1 Service Pack %2 alebo novší.
NotOnThisPlatform=Tento produkt sa nedá spusti v %1.
OnlyOnThisPlatform=Tento produkt musí by spustenı v %1.
OnlyOnTheseArchitectures=Tento produkt je moné nainštalova iba vo verziách MS Windows s podporou architektúry procesorov:%n%n%1
WinVersionTooLowError=Tento produkt vyaduje %1 verzie %2 alebo vyššej.
WinVersionTooHighError=Tento produkt sa nedá nainštalova vo %1 verzie %2 alebo vyššej.
AdminPrivilegesRequired=Na inštaláciu tohto produktu musíte by prihlásenı s právami administrátora.
PowerUserPrivilegesRequired=Na inštaláciu tohto produktu musíte by prihlásenı s právami Administrátora alebo èlena skupiny Power Users.
SetupAppRunningError=Sprievodca inštaláciou zistil, e produkt %1 je teraz spustenı.%n%nUkonète všetky spustené inštancie tohto produktu a pokraèujte kliknutím na tlaèidlo "OK", alebo ukonète inštaláciu tlaèidlom "Zruši".
UninstallAppRunningError=Sprievodca odinštaláciou zistil, e produkt %1 je teraz spustenı.%n%nUkonète všetky spustené inštancie tohto produktu a pokraèujte kliknutím na tlaèidlo "OK", alebo ukonète inštaláciu tlaèidlom "Zruši".

; *** Startup questions
PrivilegesRequiredOverrideTitle=Vyberte inštalaènı mód inštalátora
PrivilegesRequiredOverrideInstruction=Vyberte inštalaènı mód
PrivilegesRequiredOverrideText1=%1 sa môe nainštalova pre všetkıch uívate¾ov (vyaduje administrátorské práva), alebo len pre Vás.
PrivilegesRequiredOverrideText2=%1 sa môe nainštalova len pre Vás, alebo pre všetkıch uívate¾ov (vyadujú sa Administrátorské práva).
PrivilegesRequiredOverrideAllUsers=Inštalova pre &všetkıch uívate¾ov
PrivilegesRequiredOverrideAllUsersRecommended=Inštalova pre &všetkıch uívate¾ov (odporúèané)
PrivilegesRequiredOverrideCurrentUser=Inštalova len pre &mòa
PrivilegesRequiredOverrideCurrentUserRecommended=Inštalova len pre &mòa (odporúèané)

; *** Misc. errors
ErrorCreatingDir=Sprievodca inštaláciou nemohol vytvori adresár "%1"
ErrorTooManyFilesInDir=Nedá sa vytvori súbor v adresári "%1", pretoe tento adresár u obsahuje príliš ve¾a súborov

; *** Setup common messages
ExitSetupTitle=Ukonèi Sprievodcu inštaláciou
ExitSetupMessage=Inštalácia nebola kompletne dokonèená. Ak teraz ukonèíte Sprievodcu inštaláciou, produkt nebude nainštalovanı.%n%nSprievodcu inštaláciou môete znovu spusti neskôr a dokonèi tak inštaláciu.%n%nUkonèi Sprievodcu inštaláciou?
AboutSetupMenuItem=&O Sprievodcovi inštalácie...
AboutSetupTitle=O Sprievodcovi inštalácie
AboutSetupMessage=%1 verzia %2%n%3%n%n%1 domovská stránka:%n%4
AboutSetupNote=
TranslatorNote=Slovak translation maintained by Milan Potancok (milan.potancok AT gmail.com), Ivo Bauer (bauer AT ozm.cz), Tomas Falb (tomasf AT pobox.sk) + Slappy (slappy AT pobox.sk)

; *** Buttons
ButtonBack=< &Spä
ButtonNext=&Ïalej >
ButtonInstall=&Inštalova
ButtonOK=OK
ButtonCancel=Zruši
ButtonYes=&Áno
ButtonYesToAll=Áno &všetkım
ButtonNo=&Nie
ButtonNoToAll=Ni&e všetkım
ButtonFinish=&Dokonèi
ButtonBrowse=&Prechádza...
ButtonWizardBrowse=&Prechádza...
ButtonNewFolder=&Vytvori novı adresár

; *** "Select Language" dialog messages
SelectLanguageTitle=Vıber jazyka Sprievodcu inštaláciou
SelectLanguageLabel=Zvo¾te jazyk, ktorı sa má poui pri inštalácii.

; *** Common wizard text
ClickNext=Pokraèujte kliknutím na tlaèidlo "Ïalej", alebo ukonète sprievodcu inštaláciou tlaèidlom "Zruši".
BeveledLabel=
BrowseDialogTitle=Nájs adresár
BrowseDialogLabel=Z dole uvedeného zoznamu vyberte adresár a kliknite na "OK".
NewFolderName=Novı adresár

; *** "Welcome" wizard page
WelcomeLabel1=Víta Vás Sprievodca inštaláciou produktu [name].
WelcomeLabel2=Produkt [name/ver] sa nainštaluje do tohto poèítaèa.%n%nSkôr, ako budete pokraèova, odporúèame ukonèi všetky spustené aplikácie.

; *** "Password" wizard page
WizardPassword=Heslo
PasswordLabel1=Táto inštalácia je chránená heslom.
PasswordLabel3=Zadajte heslo a pokraèujte kliknutím na tlaèidlo "Ïalej". Pri zadávaní hesla rozlišujte malé a ve¾ké písmená.
PasswordEditLabel=&Heslo:
IncorrectPassword=Zadané heslo nie je správne. Skúste to ešte raz prosím.

; *** "License Agreement" wizard page
WizardLicense=Licenèná zmluva
LicenseLabel=Skôr, ako budete pokraèova, preèítajte si tieto dôleité informácie, prosím.
LicenseLabel3=Preèítajte si túto Licenènú zmluvu prosím. Aby mohla inštalácia pokraèova, musíte súhlasi s podmienkami tejto zmluvy.
LicenseAccepted=&Súhlasím s podmienkami Licenènej zmluvy
LicenseNotAccepted=&Nesúhlasím s podmienkami Licenènej zmluvy

; *** "Information" wizard pages
WizardInfoBefore=Informácie
InfoBeforeLabel=Skôr, ako budete pokraèova, preèítajte si tieto dôleité informácie, prosím.
InfoBeforeClickLabel=Pokraèujte v inštalácii kliknutím na tlaèidlo "Ïalej".
WizardInfoAfter=Informácie
InfoAfterLabel=Skôr, ako budete pokraèova, preèítajte si tieto dôleité informácie prosím.
InfoAfterClickLabel=Pokraèujte v inštalácii kliknutím na tlaèidlo "Ïalej".

; *** "User Information" wizard page
WizardUserInfo=Informácie o pouívate¾ovi
UserInfoDesc=Zadajte poadované informácie prosím.
UserInfoName=&Pouívate¾ské meno:
UserInfoOrg=&Organizácia:
UserInfoSerial=&Sériové èíslo:
UserInfoNameRequired=Meno pouívate¾a musí by zadané.

; *** "Select Destination Location" wizard page
WizardSelectDir=Vyberte cie¾ovı adresár
SelectDirDesc=Kde má by produkt [name] nainštalovanı?
SelectDirLabel3=Sprievodca nainštaluje produkt [name] do nasledujúceho adresára.
SelectDirBrowseLabel=Pokraèujte kliknutím na tlaèidlo "Ïalej". Ak chcete vybra inı adresár, kliknite na tlaèidlo "Prechádza".
DiskSpaceGBLabel=Inštalácia vyaduje najmenej [gb] GB miesta v disku.
DiskSpaceMBLabel=Inštalácia vyaduje najmenej [mb] MB miesta v disku.
CannotInstallToNetworkDrive=Sprievodca inštaláciou nemôe inštalova do sieovej jednotky.
CannotInstallToUNCPath=Sprievodca inštaláciou nemôe inštalova do UNC umiestnenia.
InvalidPath=Musíte zada úplnú cestu vrátane písmena jednotky; napríklad:%n%nC:\Aplikácia%n%nalebo cestu UNC v tvare:%n%n\\Server\Zdie¾anı adresár
InvalidDrive=Vami vybraná jednotka alebo cesta UNC neexistuje, alebo nie je dostupná. Vyberte iné umiestnenie prosím.
DiskSpaceWarningTitle=Nedostatok miesta v disku
DiskSpaceWarning=Sprievodca inštaláciou vyaduje najmenej %1 KB vo¾ného miesta pre inštaláciu produktu, ale vo vybranej jednotke je dostupnıch iba %2 KB.%n%nAj napriek tomu chcete pokraèova?
DirNameTooLong=Názov adresára alebo cesta sú príliš dlhé.
InvalidDirName=Názov adresára nie je správny.
BadDirName32=Názvy adresárov nesmú obsahova iadny z nasledujúcich znakov:%n%n%1
DirExistsTitle=Adresár u existuje
DirExists=Adresár:%n%n%1%n%nu existuje. Aj napriek tomu chcete nainštalova produkt do tohto adresára?
DirDoesntExistTitle=Adresár neexistuje
DirDoesntExist=Adresár: %n%n%1%n%nešte neexistuje. Má sa tento adresár vytvori?

; *** "Select Components" wizard page
WizardSelectComponents=Vyberte komponenty
SelectComponentsDesc=Aké komponenty majú by nainštalované?
SelectComponentsLabel2=Zaškrtnite iba komponenty, ktoré chcete nainštalova; komponenty, ktoré se nemajú inštalova, nechajte nezaškrtnuté. Pokraèujte kliknutím na tlaèidlo "Ïalej".
FullInstallation=Úplná inštalácia
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Kompaktná inštalácia
CustomInstallation=Volite¾ná inštalácia
NoUninstallWarningTitle=Komponenty existujú
NoUninstallWarning=Sprievodca inštaláciou zistil e nasledujúce komponenty u sú v tomto poèítaèi nainštalované:%n%n%1%n%nAk ich teraz nezahrniete do vıberu, nebudú neskôr odinštalované.%n%nAj napriek tomu chcete pokraèova?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Vybrané komponenty vyadujú najmenej [gb] GB miesta v disku.
ComponentsDiskSpaceMBLabel=Vybrané komponenty vyadujú najmenej [mb] MB miesta v disku.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Vyberte ïalšie úlohy
SelectTasksDesc=Ktoré ïalšie úlohy majú by vykonané?
SelectTasksLabel2=Vyberte ïalšie úlohy, ktoré majú by vykonané poèas inštalácie produktu [name] a pokraèujte kliknutím na tlaèidlo "Ïalej".

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Vyberte skupinu v ponuke Štart
SelectStartMenuFolderDesc=Kam má sprievodca inštalácie umiestni zástupcov aplikácie?
SelectStartMenuFolderLabel3=Sprievodca inštaláciou vytvorí zástupcov aplikácie v nasledujúcom adresári ponuky Štart.
SelectStartMenuFolderBrowseLabel=Pokraèujte kliknutím na tlaèidlo Ïalej. Ak chcete zvoli inı adresár, kliknite na tlaèidlo "Prechádza".
MustEnterGroupName=Musíte zada názov skupiny.
GroupNameTooLong=Názov adresára alebo cesta sú príliš dlhé.
InvalidGroupName=Názov adresára nie je správny.
BadGroupName=Názov skupiny nesmie obsahova iadny z nasledujúcich znakov:%n%n%1
NoProgramGroupCheck2=&Nevytvára skupinu v ponuke Štart

; *** "Ready to Install" wizard page
WizardReady=Inštalácia je pripravená
ReadyLabel1=Sprievodca inštaláciou je teraz pripravenı nainštalova produkt [name] na Váš poèítaè.
ReadyLabel2a=Pokraèujte v inštalácii kliknutím na tlaèidlo "Inštalova". Ak chcete zmeni niektoré nastavenia inštalácie, kliknite na tlaèidlo "< Spä".
ReadyLabel2b=Pokraèujte v inštalácii kliknutím na tlaèidlo "Inštalova".
ReadyMemoUserInfo=Informácie o pouívate¾ovi:
ReadyMemoDir=Cie¾ovı adresár:
ReadyMemoType=Typ inštalácie:
ReadyMemoComponents=Vybrané komponenty:
ReadyMemoGroup=Skupina v ponuke Štart:
ReadyMemoTasks=Ïalšie úlohy:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=Sahovanie dodatoènıch súborov...
ButtonStopDownload=&Zastavi sahovanie
StopDownload=Naozaj chcete zastavi sahovanie?
ErrorDownloadAborted=Sahovanie prerušené
ErrorDownloadFailed=Sahovanie zlyhalo: %1 %2
ErrorDownloadSizeFailed=Zlyhalo získanie ve¾kosti: %1 %2
ErrorFileHash1=Kontrola hodnoty súboru zlyhala: %1
ErrorFileHash2=Nesprávna kontrolná hodnota: oèakávala sa %1, zistená %2
ErrorProgress=Nesprávny priebeh: %1 z %2
ErrorFileSize=Nesprávna ve¾kos súboru: oèakávala sa %1, zistená %2

; *** "Preparing to Install" wizard page
WizardPreparing=Príprava inštalácie
PreparingDesc=Sprievodca inštaláciou pripravuje inštaláciu produktu [name] do tohto poèítaèa.
PreviousInstallNotCompleted=Inštalácia/odinštalácia predošlého produktu nebola úplne dokonèená. Dokonèenie tohto procesu vyaduje reštart poèítaèa.%n%nPo reštartovaní poèítaèa znovu spustite Sprievodcu inštaláciou, aby bolo moné kompletne dokonèi inštaláciu produktu [name].
CannotContinue=Sprievodca inštaláciou nemôe pokraèova. Ukonèite, prosím, sprievodcu inštaláciou kliknutím na tlaèidlo "Zruši".
ApplicationsFound=Nasledujúce aplikácie pracujú so súbormi, ktoré musí Sprievodca inštaláciou aktualizova. Odporúèame, aby ste povolili Sprievodcovi inštaláciou automaticky ukonèi tieto aplikácie.
ApplicationsFound2=Nasledujúce aplikácie pracujú so súbormi, ktoré musí Sprievodca inštaláciou aktualizova. Odporúèame, aby ste povolili Sprievodcovi inštaláciou automaticky ukonèi tieto aplikácie. Po dokonèení inštalácie sa Sprievodca inštaláciou pokúsi tieto aplikácie opätovne spusti.
CloseApplications=&Automaticky ukonèi aplikácie
DontCloseApplications=&Neukonèova aplikácie
ErrorCloseApplications=Sprievodca inštaláciou nemohol automaticky zatvori všetky aplikácie. Odporúèame, aby ste ruène ukonèili všetky aplikácie, ktoré pouívajú súbory a ktoré má Sprievodca aktualizova.
PrepareToInstallNeedsRestart=Sprievodca inštaláciou potrebuje reštartova tento poèítaè. Po reštartovaní poèítaèa znovu spustite tohto Sprievodcu inštaláciou, aby sa inštalácia [name] dokonèila.%n%nChcete teraz reštartova tento poèítaè?

; *** "Installing" wizard page
WizardInstalling=Inštalujem
InstallingLabel=Poèkajte prosím, pokia¾ Sprievodca inštaláciou dokonèí inštaláciu produktu [name] do tohto poèítaèa.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Dokonèuje sa inštalácia produktu [name]
FinishedLabelNoIcons=Sprievodca inštaláciou dokonèil inštaláciu produktu [name] do tohto poèítaèa.
FinishedLabel=Sprievodca inštaláciou dokonèil inštaláciu produktu [name] do tohto poèítaèa. Produkt je moné spusti pomocou nainštalovanıch ikon a zástupcov.
ClickFinish=Ukonète Sprievodcu inštaláciou kliknutím na tlaèidlo "Dokonèi".
FinishedRestartLabel=Pre dokonèenie inštalácie produktu [name] je nutné reštartova tento poèítaè. eláte si teraz reštartova tento poèítaè?
FinishedRestartMessage=Pre dokonèenie inštalácie produktu [name] je nutné reštartova tento poèítaè.%n%neláte si teraz reštartova tento poèítaè?
ShowReadmeCheck=Áno, chcem zobrazi dokument "ÈITAJMA"
YesRadio=&Áno, chcem teraz reštartova poèítaè
NoRadio=&Nie, poèítaè reštartujem neskôr

; used for example as 'Run MyProg.exe'
RunEntryExec=Spusti %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Zobrazi %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Sprievodca inštaláciou vyaduje ïalší disk
SelectDiskLabel2=Vlote prosím, disk %1 a kliknite na tlaèidlo "OK".%n%nAk sa súbory tohto disku nachádzajú v inom adresári ako v tom, ktorı je zobrazenı nišie, zadajte správnu cestu alebo kliknite na tlaèidlo "Prechádza".
PathLabel=&Cesta:
FileNotInDir2=Súbor "%1" sa nedá nájs v "%2". Vlote prosím, správny disk, alebo zvo¾te inı adresár.
SelectDirectoryLabel=Špecifikujte prosím, umiestnenie ïalšieho disku.

; *** Installation phase messages
SetupAborted=Inštalácia nebola úplne dokonèená.%n%nOpravte chybu a opä spustite Sprievodcu inštaláciou prosím.
AbortRetryIgnoreSelectAction=Vyberte akciu
AbortRetryIgnoreRetry=&Skúsi znovu
AbortRetryIgnoreIgnore=&Ignorova chybu a pokraèova
AbortRetryIgnoreCancel=Zruši inštaláciu

; *** Installation status messages
StatusClosingApplications=Ukonèovanie aplikácií...
StatusCreateDirs=Vytvárajú sa adresáre...
StatusExtractFiles=Rozba¾ujú sa súbory...
StatusCreateIcons=Vytvárajú sa ikony a zástupcovia...
StatusCreateIniEntries=Vytvárajú sa záznamy v konfiguraènıch súboroch...
StatusCreateRegistryEntries=Vytvárajú sa záznamy v systémovom registri...
StatusRegisterFiles=Registrujú sa súbory...
StatusSavingUninstall=Ukladajú sa informácie potrebné pre neskoršie odinštalovanie produktu...
StatusRunProgram=Dokonèuje sa inštalácia...
StatusRestartingApplications=Reštartovanie aplikácií...
StatusRollback=Vykonané zmeny sa vracajú spä...

; *** Misc. errors
ErrorInternal2=Interná chyba: %1
ErrorFunctionFailedNoCode=%1 zlyhala
ErrorFunctionFailed=%1 zlyhala; kód %2
ErrorFunctionFailedWithMessage=%1 zlyhala; kód %2.%n%3
ErrorExecutingProgram=Nedá sa spusti súbor:%n%1

; *** Registry errors
ErrorRegOpenKey=Došlo k chybe pri otváraní k¾úèa systémového registra:%n%1\%2
ErrorRegCreateKey=Došlo k chybe pri vytváraní k¾úèa systémového registra:%n%1\%2
ErrorRegWriteKey=Došlo k chybe pri zápise k¾úèa do systémového registra:%n%1\%2

; *** INI errors
ErrorIniEntry=Došlo k chybe pri vytváraní záznamu v konfiguraènom súbore "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Preskoèi tento súbor (neodporúèané)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignorova chybu a pokraèova (neodporúèané)
SourceIsCorrupted=Zdrojovı súbor je poškodenı
SourceDoesntExist=Zdrojovı súbor "%1" neexistuje
ExistingFileReadOnly2=Existujúci súbor nie je moné prepísa, pretoe je oznaèenı atribútom Iba na èítanie.
ExistingFileReadOnlyRetry=&Odstráni atribút Iba na èítanie a skúsi znovu
ExistingFileReadOnlyKeepExisting=&Ponecha existujúci súbor
ErrorReadingExistingDest=Došlo k chybe pri pokuse o èítanie existujúceho súboru:
FileExistsSelectAction=Vyberte akciu
FileExists2=Súbor u existuje.
FileExistsOverwriteExisting=&Prepísa existujúci súbor
FileExistsKeepExisting=Ponecha &existujúci súbor
FileExistsOverwriteOrKeepAll=&Vykona pre všetky ïalšie konflikty
ExistingFileNewerSelectAction=Vyberte akciu
ExistingFileNewer2=Existujúci súbor je novší ako súbor, ktorı sa Sprievodca inštaláciou pokúša nainštalova.
ExistingFileNewerOverwriteExisting=&Prepísa existujúci súbor
ExistingFileNewerKeepExisting=Ponecha &existujúci súbor (odporúèané)
ExistingFileNewerOverwriteOrKeepAll=&Vykona pre všetky ïalšie konflikty
ErrorChangingAttr=Došlo k chybe pri pokuse o modifikáciu atribútov existujúceho súboru:
ErrorCreatingTemp=Došlo k chybe pri pokuse o vytvorenie súboru v cie¾ovom adresári:
ErrorReadingSource=Došlo k chybe pri pokuse o èítanie zdrojového súboru:
ErrorCopying=Došlo k chybe pri pokuse o skopírovanie súboru:
ErrorReplacingExistingFile=Došlo k chybe pri pokuse o nahradenie existujúceho súboru:
ErrorRestartReplace=Zlyhala funkcia "RestartReplace" Sprievodcu inštaláciou:
ErrorRenamingTemp=Došlo k chybe pri pokuse o premenovanie súboru v cie¾ovom adresári:
ErrorRegisterServer=Nedá sa vykona registrácia DLL/OCX: %1
ErrorRegSvr32Failed=Volanie RegSvr32 zlyhalo s návratovım kódom %1
ErrorRegisterTypeLib=Nedá sa vykona registrácia typovej kninice: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32bitovı
UninstallDisplayNameMark64Bit=64bitovı
UninstallDisplayNameMarkAllUsers=Všetci uívatelia
UninstallDisplayNameMarkCurrentUser=Aktuálny uívate¾

; *** Post-installation errors
ErrorOpeningReadme=Došlo k chybe pri pokuse o otvorenie dokumentu "ÈITAJMA".
ErrorRestartingComputer=Sprievodcovi inštaláciou sa nepodarilo reštartova tento poèítaè. Reštartujte ho manuálne prosím.

; *** Uninstaller messages
UninstallNotFound=Súbor "%1" neexistuje. Produkt sa nedá odinštalova.
UninstallOpenError=Súbor "%1" nie je moné otvori. Produkt nie je moné odinštalova.
UninstallUnsupportedVer=Sprievodcovi odinštaláciou sa nepodarilo rozpozna formát súboru obsahujúceho informácie na odinštalovanie produktu "%1". Produkt sa nedá odinštalova
UninstallUnknownEntry=V súbore obsahujúcom informácie na odinštalovanie produktu bola zistená neznáma poloka (%1)
ConfirmUninstall=Naozaj chcete odinštalova %1 a všetky jeho komponenty?
UninstallOnlyOnWin64=Tento produkt je moné odinštalova iba v 64-bitovıch verziách MS Windows.
OnlyAdminCanUninstall=K odinštalovaniu tohto produktu musíte by prihlásenı s právami Administrátora.
UninstallStatusLabel=Poèkajte prosím, kım produkt %1 nebude odinštalovanı z tohto poèítaèa.
UninstalledAll=%1 bol úspešne odinštalovanı z tohto poèítaèa.
UninstalledMost=%1 bol odinštalovanı z tohto poèítaèa.%n%nNiektoré jeho komponenty sa však nepodarilo odinštalova. Môete ich odinštalova manuálne.
UninstalledAndNeedsRestart=Na dokonèenie odinštalácie produktu %1 je potrebné reštartova tento poèítaè.%n%nChcete ihneï reštartova tento poèítaè?
UninstallDataCorrupted=Súbor "%1" je poškodenı. Produkt sa nedá odinštalova

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Odinštalova zdie¾anı súbor?
ConfirmDeleteSharedFile2=Systém indikuje, e nasledujúci zdie¾anı súbor nie je pouívanı iadnymi inımi aplikáciami. Má Sprievodca odinštaláciou tento zdie¾anı súbor odstráni?%n%nAk niektoré aplikácie tento súbor pouívajú, nemusia po jeho odinštalovaní pracova správne. Pokia¾ to neviete správne posúdi, odporúèame zvoli "Nie". Ponechanie tohto súboru v systéme nespôsobí iadnu škodu.
SharedFileNameLabel=Názov súboru:
SharedFileLocationLabel=Umiestnenie:
WizardUninstalling=Stav odinštalovania
StatusUninstalling=Prebieha odinštalovanie %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Inštalovanie %1.
ShutdownBlockReasonUninstallingApp=Odinštalovanie %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 verzia %2
AdditionalIcons=Ïalší zástupcovia:
CreateDesktopIcon=Vytvori zástupcu na &ploche
CreateQuickLaunchIcon=Vytvori zástupcu na paneli &Rıchle spustenie
ProgramOnTheWeb=Aplikácia %1 na internete
UninstallProgram=Odinštalova aplikáciu %1 
LaunchProgram=Spusti aplikáciu %1
AssocFileExtension=Vytvori &asociáciu medzi súbormi typu %2 a aplikáciou %1
AssocingFileExtension=Vytvára sa asociácia medzi súbormi typu %2 a aplikáciou %1...
AutoStartProgramGroupDescription=Pri spustení:
AutoStartProgram=Automaticky spusti %1
AddonHostProgramNotFound=Nepodarilo sa nájs %1 v adresári, ktorı ste zvolili.%n%nChcete napriek tomu pokraèova?
