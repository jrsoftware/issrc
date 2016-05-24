; *** Inno Setup version 5.5.3+ Icelandic messages ***
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; ID: Icelandic.isl,v 5.5.3+ 2016/04/27 Sigurður N. Þorleifsson, sigurdn@postur.is

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Icelandic
LanguageID=$040F
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
SetupAppTitle=Uppsetning
SetupWindowTitle=Uppsetning fyrir - %1
UninstallAppTitle=Fjarlægja forrit
UninstallAppFullTitle=%1 Fjarlægt

; *** Misc. common
InformationTitle=Upplýsingar
ConfirmTitle=Staðfesta
ErrorTitle=Villa

; *** SetupLdr messages
SetupLdrStartupMessage=Þú er að setja inn %1. Viltu halda áfram?
LdrCannotCreateTemp=Get ekki búið til tímabundna skrá. Hætt við uppsetningu
LdrCannotExecTemp=Ófær um að keyra skrá í Temp möppu, Hætt við uppsetningu

; *** Startup error messages
LastErrorMessage=%1.%n%nVilla %2: %3
SetupFileMissing=Skrána %1 vantar í uppsetningarmöppuna. Vinsamlegast leysið vandann eða útvegið nýtt eintak af forritinu.
SetupFileCorrupt=Uppsetningarskrárnar eru gallaðar. Vinsamlegast útvegið nýtt eintak af forritinu.
SetupFileCorruptOrWrongVer=Uppsetningarskrárnar eru gallaðar, eða virka ekki með þessari útgáfu af forritinu. Vinsamlegast leiðréttið vandann eða útvegið nýtt eintak af forritinu.
InvalidParameter=Ógilt viðfang var sent yfir skipanalínu:%n%n%1
SetupAlreadyRunning=Uppsetning er nú þegar keyrandi.
WindowsVersionNotSupported=Þetta forrit styður ekki þá útgáfu af Windows stýrikerfinu sem tölvan þín notar.
WindowsServicePackRequired=Þetta forrit þarf %1 Þjónustupakka (Service Pack)  %2 eða nýrra.
NotOnThisPlatform=Þetta forrit er ekki samhæft við %1
OnlyOnThisPlatform=Þetta forrit verður að keyra á %1
OnlyOnTheseArchitectures=Þetta forrit er aðeins hægt að setja upp á Windows útgáfum hannaðar fyrir eftirfarandi örgjörva (CPU) kynslóðir:%n%n%1
MissingWOW64APIs=Windows stýrikerfið á tölvunni þinni innheldur ekki alla virkni sem Uppsetningarálfurinn þarf til að framkvæma 64-bita uppsetningu. Til að leiðrétta vandann, vinsamlegast setjið upp Service Pack %1.
WinVersionTooLowError=Þetta forrit þarf %1 útgáfu %2 eða nýrra.
WinVersionTooHighError=Þetta forrit er ekki hægt að setja upp á %1 útgáfu %2 eða nýrra
AdminPrivilegesRequired=Þú verður að vera innskráð(ur) sem kerfisstjóri til að setja þetta forrit upp.
PowerUserPrivilegesRequired=Þú verður að vera innskráð(ur) sem kerfisstjóri eða ofurnotandi til að setja þetta forrit upp.
SetupAppRunningError=Uppsetningar forritið hefur fundið að %1 er í notkun.%n%nVinsamlegast lokaðu öllum eintökum, veldu síðan OK til að halda áfram, eða Cancel til að hætta.
UninstallAppRunningError=Niðurtöku forritið hefur fundið að %1 er í notkun.%n%nVinsamlegast lokaðu öllum eintökum, veldu síðan OK til að halda áfram, eða Cancel til að hætta.

; *** Misc. errors
ErrorCreatingDir=Uppsetningarforritið gat ekki búið til möppuna "%1"
ErrorTooManyFilesInDir=Get ekki búið til skrá í möppunni "%1" vegna þess að hún inniheldur of margar skrár.

; *** Setup common messages
ExitSetupTitle=Stöðva uppsetningu
ExitSetupMessage=Uppsetningu er ekki lokið. Ef þú hættir núna verður forritið ekki sett inn.%n%nÞú getur keyrt uppsetninguna seinna til að setja forritið upp.%n%nViltu stöðva uppsetningu ?
AboutSetupMenuItem=&Um uppsetningu..
AboutSetupTitle=Um uppsetningu
AboutSetupMessage=%1 útgáfa %2%n%3%n%n%1 heimasíða:%n%4
AboutSetupNote=
TranslatorNote=Icelandic translation maintained by Sigurður N. Thorleifsson (sigurdn@postur.is)

; *** Buttons
ButtonBack=< &Bakka
ButtonNext=&Næsta >
ButtonInstall=&Setja upp
ButtonOK=Í l&agi
ButtonCancel=&Hætta við
ButtonYes=&Já
ButtonYesToAll=Já við &öllu
ButtonNo=&Nei
ButtonNoToAll=N&ei við öllu
ButtonFinish=&Ljúka uppsetningu
ButtonBrowse=&Finna...
ButtonWizardBrowse=F&inna...
ButtonNewFolder=&Stofna möppu

; *** "Select Language" dialog messages
SelectLanguageTitle=Veldu tungumál fyrir uppsetningu
SelectLanguageLabel=Veldu tungumál til að nota fyrir þessa uppsetningu

; *** Common wizard text
ClickNext=Smelltu á Næsta til að halda áfram, eða Hætta við til að stöðva uppsetninguna.
BeveledLabel=
BrowseDialogTitle=Leita eftir möppu
BrowseDialogLabel=Veldu möppu í listanum hér fyrir neðan, smelltu síðan á Í lagi.
NewFolderName=Ný mappa

; *** "Welcome" wizard page
WelcomeLabel1=Velkomin(n) í uppsetningarálfinn fyrir [name]
WelcomeLabel2=Núna verður [name/ver] sett inn á tölvuna þína%n%nVið mælum eindregið með því að þú lokir öllum forritum sem eru í gangi á tölvunni þinni áður en þú heldur áfram. Það dregur stórlega úr líkum á vandamálum meðan á uppsetningunni stendur.

; *** "Password" wizard page
WizardPassword=Lykilorð
PasswordLabel1=Þessi uppsetning er varin með lykilorði.
PasswordLabel3=Vinsamlegast sláið inn lykilorðið, ýtið síðan á hnappinn Næsta til að halda áfram. Lykilorð gera greinarmun milli há- og lágstafa.
PasswordEditLabel=&Lykilorð:
IncorrectPassword=Þetta er ekki rétta lykilorðið. Vinsamlega reyndu aftur.

; *** "License Agreement" wizard page
WizardLicense=Leyfissamningur
LicenseLabel=Áríðandi: Vinsamlega lestu þessar upplýsingar áður en þú heldur áfram
LicenseLabel3=Vinsamlegast lestu yfir meðfylgjandi leyfisskilmála. Þú verður að samþykkja að fylgja þessum skilyrðum áður en uppsetnig getur haldið áfram.
LicenseAccepted=Ég &samþykki leyfisskilmálana
LicenseNotAccepted=Ég &hafna þessum skilmálum

; *** "Information" wizard pages
WizardInfoBefore=Upplýsingar
InfoBeforeLabel=Áríðandi: Vinsamlegast lestu eftirfarandi upplýsingar áður en þú heldur áfram.
InfoBeforeClickLabel=Þegar þú vilt halda áfram með uppsetninguna, smelltu á Næsta.
WizardInfoAfter=Upplýsingar
InfoAfterLabel=Áríðandi: Vinsamlegast lestu eftirfarandi upplýsingar áður en þú heldur áfram.
InfoAfterClickLabel=Þegar þú vilt halda áfram með uppsetninguna, smelltu á Næsta.

; *** "User Information" wizard page
WizardUserInfo=Notanda upplýsingar
UserInfoDesc=Vinsamlegast skráðu inn þínar upplýsingar.
UserInfoName=&Notanda nafn:
UserInfoOrg=&Fyrirtæki
UserInfoSerial=&Leyfisnúmer
UserInfoNameRequired=Þú verður að setja inn nafn.

; *** "Select Destination Location" wizard page
WizardSelectDir=Veldu möppu fyrir forritið
SelectDirDesc=Hvar á að setja [name] upp?
SelectDirLabel3=Álfurinn mun setja [name] upp í eftirfarandi möppu.
SelectDirBrowseLabel=Til að halda áfram, smelltu á Næsta. Ef þú villt velja aðra möppu, smelltu á Finna.
DiskSpaceMBLabel=Þetta forrit krefst a.m.k. [mb] MB af diskrými.
CannotInstallToNetworkDrive=Uppsetning getur ekki sett forritið inná netdrif.
CannotInstallToUNCPath=Uppsetningin getur ekki notað UNC skráarslóða.
InvalidPath=Þú verður að gefa upp fullan skráaslóða með drifbókstaf: t.d. %nC:\MAPPA
InvalidDrive=Drifið eða slóðinn sem þú valdir er ekki til. Vinsamlega veldu annað drif.
DiskSpaceWarningTitle=Ekki er nægilegt pláss eftir á disknum
DiskSpaceWarning=Uppsetningarálfurinn þarf minnst %1 KB af diskrými til að setja forritið upp, en valið drif hefur aðeins %2 KB gagnapláss laust.%n%nViltu samt halda áfram?
DirNameTooLong=Nafn á möppu eða skráarslóða er of langt.
InvalidDirName=Þetta er ekki gilt nafn á möppu
BadDirName32=Nafn möppunnar má ekki innihalda neinn af eftirfarandi bókstöfum:%n%n%1
DirExistsTitle=Mappan fannst
DirExists=Mappan:%n%n%1%n%ner nú þegar til. Viltu samt setja forritið upp í þessari möppu?
DirDoesntExistTitle=Mappan er ekki til
DirDoesntExist=Mappan:%n%n%1%n%ner ekki til. Viltu búa til möppu með þessu nafni?

; *** "Select Components" wizard page
WizardSelectComponents=Veldu eiginleika
SelectComponentsDesc=Hvaða eiginleika viltu setja upp?
SelectComponentsLabel2=Veldu þá eiginleika sem þú vilt setja upp: eyddu þeim eiginleikum sem þú vilt ekki setja upp. Smelltu á Næsta þegar þú vilt halda áfram.
FullInstallation=Setja forritið upp í heild sinni
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Samþjöppuð uppsetning
CustomInstallation=Sérsniðin uppsetning
NoUninstallWarningTitle=Þessir eiginleikar eru þegar til staðar
NoUninstallWarning=Uppsetningarálfurinn hefur tekið eftir því að þú hefur nú þegar sett þessa eiginleika upp á tölvuna þína:%n%n%1%n
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Til þess að velja þennan möguleika þarftu a.m.k. 1 MB af lausu diskrými.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Velja fleiri verkliði
SelectTasksDesc=Hvaða viðbótarverkliði viltu framkvæma?
SelectTasksLabel2=Veldu þá verkliði til viðbótar sem þú vilt að uppsetningarálfurinn annist á meðan [name] er sett upp á tölvunni. Smelltu svo á Næsta.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Veldu staðsetningu í ræsivalmyndinni.
SelectStartMenuFolderDesc=Hvar á uppsetningarálfurinn að setja flýtivísa fyrir forritið?
SelectStartMenuFolderLabel3=Álfurinn mun stofna flýtivísa fyrir forritið í ræsivalmyndinni undir eftirfarandi möppu.
SelectStartMenuFolderBrowseLabel=Til að halda áfram, smelltu á Næsta. Ef þú villt velja aðra möppu, smelltu á Finna.
MustEnterGroupName=Þú verður að slá inn nafn á möppu.
GroupNameTooLong=Nafn á möppu eða skráarslóða er of langt.
InvalidGroupName=Nafnið á möppunni er ekki löglegt.
BadGroupName=Eftirfarandi stafir mega ekki vera í nafni möppunnar:%n%n%1
NoProgramGroupCheck2=&Ekki búa til möppu í ræsivalmyndinni

; *** "Ready to Install" wizard page
WizardReady=Yfirlit yfir valdar stillingar.
ReadyLabel1=Uppsetningarálfurinn getur núna byrjað að setja [name] upp á tölvunni.
ReadyLabel2a=Smelltu á Setja upp til að halda áfram með uppsetninguna, eða smelltu á Bakka til þess að lagfæra stillingar forritsins.
ReadyLabel2b=Smelltu á Setja upp til að halda áfram með uppsetninguna.
ReadyMemoUserInfo=Upplýsingar um notanda:
ReadyMemoDir=Uppsetningarmappa:
ReadyMemoType=Gerð uppsetningar:
ReadyMemoComponents=Valdir eiginleikar:
ReadyMemoGroup=Mappa í ræsivalmynd:
ReadyMemoTasks=Viðbótarverkliðir:

; *** "Preparing to Install" wizard page
WizardPreparing=Framkvæmi undirbúning fyrir uppsetningu
PreparingDesc=Uppsetningarálfurinn er að undirbúa uppsetningu á [name] inn á tölvuna þína.
PreviousInstallNotCompleted=Annað uppsetningar forrit hefur ekki lokið við að setja upp / fjarlægja hugbúnað. Þú þarft að endurræsa tölvuna þína til klára þetta ferli fyrst.
CannotContinue=Uppsetnig getur ekki klárað. Vinsamlegast smelltu á hætta við til að stöðva.
ApplicationsFound=Eftirfarandi forrit eru að nota skrár sem uppsetningarálfurinn þarf að skipta út. Við mælum með að þú leyfir að þeim verði lokað sjálfkrafa.
ApplicationsFound2=Eftirfarandi forrit eru að nota skrár sem uppsetningarálfurinn þarf að skipta út. Við mælum með að þú leyfir að þeim verði lokað sjálfkrafa. Eftir að uppsetningu lýkur, verður reynt að endurræsa forritin.
CloseApplications=&Loka forritunum sjálfkrafa
DontCloseApplications=&Ekki loka forritunum
ErrorCloseApplications=Ekki var hægt að loka öllum forritunum. Við mælum með að loka öllum forritum sem hindra aðgang að skrám sem þarf að uppfæra áður en haldið er áfarm.

; *** "Installing" wizard page
WizardInstalling=Uppsetning í gangi
InstallingLabel=Dokaðu við á meðan uppsetningarálfurinn setur [name] upp á tölvunni þinni.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Er að klára uppsetningu á [name]
FinishedLabelNoIcons=Uppsetningarálfurinn er búinn að setja [name] upp á tölvunni þinni.
FinishedLabel=Uppsetningarálfurinn er búinn að setja  [name] upp á tölvunni þinni. Forritið má ræsa með því að tvísmella á íkonin sem sett voru upp fyrir forritið.
ClickFinish=Smelltu á Ljúka uppsetningu til að loka glugganum.
FinishedRestartLabel=Til þess að hægt sé að ljúka uppsetningunni á [name] verður uppsetningarálfurinn að endurræsa tölvuna þína. Viltu endurræsa núna?
FinishedRestartMessage=Til þess að ljúka uppsetningunni á  [name], verður uppsetningarálfurinn að endurræsa tölvuna þína.%n%nViltu endurræsa hana núna?
ShowReadmeCheck=Já, ég vil lesa Lestumig skrána.
YesRadio=&Já, ég vil endurræsa tölvuna núna
NoRadio=&Nei, ég vil endurræsa tölvuna seinna
; used for example as 'Run MyProg.exe'
RunEntryExec=Keyra %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Skoða %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Uppsetningarálfurinn þarf næsta disk
SelectDiskLabel2=Vinsamlegast settu inn disk %1 og smelltu á Í lagi.%n%nEf skrárnar á disknum eru í annarri möppu en þeirri sem hér er að neðan, sláðu inn rétta skráaslóðann eða smelltu á Finna.
PathLabel=%Slóði:
FileNotInDir2=Skráin "%1" fannst ekki á "%2". Vinsamlegast settu inn rétta diskinn eða veldu aðra möppu.
SelectDirectoryLabel=Vinsamlega tilgreindu hvar næsti diskur er staðsettur.

; *** Installation phase messages
SetupAborted=Uppsetningunni var ekki lokið. %n%nVinsamlega leystu vandann og endurræstu uppsetningarálfinn.
EntryAbortRetryIgnore=Smelltu á Reyna aftur til að reyna aftur, Sleppa til að halda samt áfram, eða Hætta við til að hætta við uppsetninguna.

; *** Installation status messages
StatusClosingApplications=Loka forritum...
StatusCreateDirs=Bý til möppur...
StatusExtractFiles=Afþjappa skrár...
StatusCreateIcons=Bý til íkon...
StatusCreateIniEntries=Bý til INI færslur...
StatusCreateRegistryEntries=Bý til registry færslur...
StatusRegisterFiles=Skrifa skrár í registry...
StatusSavingUninstall=Vista upplýsingar til að henda forritinu út...
StatusRunProgram=Klára uppsetningu...
StatusRestartingApplications=Endurræsi forrit...
StatusRollback=Afturkalla breytingar ....

; *** Misc. errors
ErrorInternal2=Keyrsla stöðvuð vegna villu:
ErrorFunctionFailedNoCode=%1 mistókst
ErrorFunctionFailed=%1 mistókst; kóði %2
ErrorFunctionFailedWithMessage=%1 mistókst; kóði %2.%n%3
ErrorExecutingProgram=Gat ekki keyrt upp skrána:%n%1

; *** Registry errors
ErrorRegOpenKey=Villa þegar verið var að opna lykil í registry:%n%1\%2
ErrorRegCreateKey=Villa þegar verið var að búa til registry lykil:%n%1\%2
ErrorRegWriteKey=Villa þegar verið var að skrifa registry lykil:%n%1\%2

; *** INI errors
ErrorIniEntry=Gat ekki stofnað færslu í INI skránni "%1".

; *** File copying errors
FileAbortRetryIgnore=Smelltu á Reyna aftur til að reyna á ný, Sleppa til að sleppa þessari skrá (ekki mælt með), eða Hætta við til að stöðva uppsetninguna.
FileAbortRetryIgnore2=Smelltu á Reyna aftur til að reyna á ný, Sleppa til að halda samt áfram (ekki mælt með), eða Hætta við til að stöðva uppsetninguna.
SourceIsCorrupted=Þessi inntaksskrá er skemmd
SourceDoesntExist=Inntaksskráin "%1" er ekki til
ExistingFileReadOnly=Skráin er stillt á lesaðgang eingöngu (read-only).%n%nSmelltu á Reyna aftur til að fjarlægja read-only eiginleikann og reyna aftur, Sleppa til að sleppa þessari skrá eða Hætta við til að hætta við uppsetninguna.
ErrorReadingExistingDest=Ekki tókst að lesa þessa skrá:
FileExists=Skráin er nú þegar til.%n%nViltu láta yfirskrifa hana?
ExistingFileNewer=Skráin sem fyrir er er nýrri en sú sem uppsetningarálfurinn er að reyna að setja upp. Mælt er með því að þú haldir skránni sem fyrir er. %n%nViltu halda upphaflegri skrá?
ErrorChangingAttr=Ekki tókst að breyta eiginleikum skrárinnar:
ErrorCreatingTemp=Ekki tókst að búa til skrá í úttaksmöppunni:
ErrorReadingSource=Ekki tókst að lesa inntaksskrána:
ErrorCopying=Ekki tókst að afrita skrána:
ErrorReplacingExistingFile=Ekki tókst að skipta út skránni:
ErrorRestartReplace=Það mistókst að endurræsa og skipta út skránni:
ErrorRenamingTemp=Ekki tókst að endurskíra skrána í úttaksmöppunni:
ErrorRegisterServer=Get ekki afskráð DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 hætti með villukóðanum %1
ErrorRegisterTypeLib=Get ekki skráð type library: %1

; *** Post-installation errors
ErrorOpeningReadme=Ekki tókst að opna LESTUMIG skrána.
ErrorRestartingComputer=Uppsetningarálfurinn gat ekki endurræst tölvuna. Vinsamlega endurræstu hana handvirkt.

; *** Uninstaller messages
UninstallNotFound=Skráin "%1" er ekki til. Ekki er hægt að fjarlægja forritið.
UninstallOpenError=Gat ekki opnað skrána "%1". Get ekki hent út forritinu.
UninstallUnsupportedVer=Logskráin "%1" til að fjarlægj forritið er á sniði sem ekki er hægt að nota með þessari útgáfu álfsins. Ekki er hægt að henda forritinu.
UninstallUnknownEntry=Óþekkt færsla fannst í (%1) logskránni sem hendir forritinu.
ConfirmUninstall=Ertu viss um að þú viljir henda %1 og öllum eiginleikum þess?
UninstallOnlyOnWin64=Þessa uppsetningu er aðeins hægt að fjarlægja af 64-bita Windows.
OnlyAdminCanUninstall=Aðeins notandi með kerfisstjóraréttindi hefur leyfi til þess að henda forritinu.
UninstallStatusLabel=Vinsamlegast dokaðu við á meðan %1 er fjarlægt af tölvunni þinni.
UninstalledAll=Það tókst að fjarlægja %1 af tölvunni þinni.
UninstalledMost=Þú ert nú búin(n) að fjarlægja %1 %n%nEkki var hægt að fjarlægja alla hluta forritsins. Þá má fjarlægja handvirkt.
UninstalledAndNeedsRestart=Þú þarft að endurræsa tölvuna til að fjarlægja hugbúnaðinn að fullu.%n%nViltu láta endurræsa tölvuna núna?
UninstallDataCorrupted=Skráin "%1" er gölluð. Ekki er hægt að fjarlægja forritið

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Viltu fjarlægja samnýtta skrá?
ConfirmDeleteSharedFile2=Stýrikerfið gefur til kynna að engin forrit noti lengur eftirfarandi samnýtta(r) skrá(r). Viltu fjarlægja hana/þær?%n%nEf einhver forrit eru enn að nota þessa(r) skrá(r) og henni/þeim er hent, getur verið að viðkomandi forrit hætti að virka eðlilega. Ef þú ert ekki viss, veldu Nei. Það er í lagi að skilja þessa(r) skrá(r) eftir á tölvunni.
SharedFileNameLabel=Skráarnafn:
SharedFileLocationLabel=Staðsetning:
WizardUninstalling=Staða:
StatusUninstalling=Hef fjarlægt %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Er að setja upp %1.
ShutdownBlockReasonUninstallingApp=Er að fjarlægja %1.

; The custom messages below aren't used by Setup itself, but if you make 
; use of them in your scripts, you'll want to translate them. 
    
[CustomMessages]
NameAndVersion=%1 útgáfa %2
AdditionalIcons=Fleiri íkonar:
CreateDesktopIcon=Stofna &desktop íkona
CreateQuickLaunchIcon=Stofna &Quick Launch íkona
ProgramOnTheWeb=%1 á Internetinu
UninstallProgram=Fjarlægja %1
LaunchProgram=Ræsa %1
AssocFileExtension=&Samtengja %1 við eftirfarandi %2 skráarendingu
AssocingFileExtension=Er að samtengja %1 við skráarendinguna %2 ...
AutoStartProgramGroupDescription=Ræsing:
AutoStartProgram=Ræsa sjálfkrafa %1
AddonHostProgramNotFound=%1 fannst ekki í möppunni sem þú valdir.%n%nViltu samt halda áfram?