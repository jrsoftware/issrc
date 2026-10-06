; *** Inno Setup version 6.5.0+ Icelandic messages ***
;
; Translator: Stefán Örvar Sigmundsson
; E-mail: stefan.orvar.sigmundsson@proton.me
; Date: 2026-10-06

[LangOptions]

LanguageName=Íslenska
LanguageID=$040F
LanguageCodePage=1252

[Messages]

; *** Application titles
SetupAppTitle=Uppsetning
SetupWindowTitle=Uppsetning - %1
UninstallAppTitle=Niðurtaka
UninstallAppFullTitle=%1-niðurtaka

; *** Misc. common
InformationTitle=Upplýsingar
ConfirmTitle=Staðfesta
ErrorTitle=Villa

; *** SetupLdr messages
SetupLdrStartupMessage=Þetta mun setja upp %1. Vilt þú halda áfram?
LdrCannotCreateTemp=Ófært um að skapa tímabundna skrá. Uppsetningu hætt
LdrCannotExecTemp=Ófært um að keyra skrá í tímabundna skráasafninu. Uppsetningu hætt
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nVilla %2: %3
SetupFileMissing=Skrána %1 vantar í uppsetningarskráasafnið. Vinsamlega leiðréttu vandamálið eða fáðu nýtt afrit af forritinu.
SetupFileCorrupt=Uppsetningarskrárnar eru spilltar. Vinsamlega fáðu nýtt afrit af forritinu.
SetupFileCorruptOrWrongVer=Uppsetningarskrárnar eru spilltar eða eru ósamrýmanlegar við þessa útgáfu af Uppsetningu. Vinsamlega leiðréttu vandamálið eða fáðu nýtt afrit af forritinu.
InvalidParameter=Ógild færibreyta var afhent á skipanalínunni:%n%n%1
SetupAlreadyRunning=Uppsetning er nú þegar í keyrslu.
WindowsVersionNotSupported=Þetta forrit styður ekki útgáfuna af Windows sem tölvan þín keyrir.
WindowsServicePackRequired=Þetta forrit krefst %1 þjónustupakka %2 eða síðari.
NotOnThisPlatform=Þetta forrit mun ekki keyra á %1.
OnlyOnThisPlatform=Þetta forrit verður að keyra á %1.
OnlyOnTheseArchitectures=Þetta forrit er einungis hægt að setja upp á útgáfur af Windows hannaðar fyrir eftirfarandi gjörvahaganir:%n%n%1
WinVersionTooLowError=Þetta forrit krefst %1-útgáfu %2 eða síðari.
WinVersionTooHighError=Þetta forrit er ekki hægt að setja upp á %1-útgáfu %2 eða síðari.
AdminPrivilegesRequired=Þú verður að vera innskráð(ur) sem stjórnandi við uppsetningu þessa forrits.
PowerUserPrivilegesRequired=Þú verður að vera innskráð(ur) sem stjórnandi eða sem meðlimur Power Users-hópsins meðan þú setur upp þetta forrit.
SetupAppRunningError=Uppsetning hefur greint að %1 er í keyrslu.%n%nVinsamlega lokaðu öllum tilvikum þess núna, smelltu síðan á Í lagi til að halda áfram eða Hætta við til að hætta.
UninstallAppRunningError=Niðurtaka hefur greint að %1 er í keyrslu.%n%nVinsamlega lokaðu öllum tilvikum þess núna, smelltu síðan á Í lagi til að halda áfram eða Hætta við til að hætta.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Veldu uppsetningarham
PrivilegesRequiredOverrideInstruction=Veldu uppsetningarham
PrivilegesRequiredOverrideText1=%1 er hægt að setja upp fyrir alla notendur (krefst stjórnandaréttinda) eða fyrir þig einungis.
PrivilegesRequiredOverrideText2=%1 er hægt að setja upp fyrir þig einungis eða fyrir alla notendur (krefst stjórnandaréttinda).
PrivilegesRequiredOverrideAllUsers=Setja upp fyrir &alla notendur
PrivilegesRequiredOverrideAllUsersRecommended=Setja upp fyrir &alla notendur (ráðlagt)
PrivilegesRequiredOverrideCurrentUser=Setja upp fyrir &mig einungis
PrivilegesRequiredOverrideCurrentUserRecommended=Setja upp fyrir &mig einungis (ráðlagt)

; *** Misc. errors
ErrorCreatingDir=Uppsetningu var ófært um að skapa skráasafnið „%1“
ErrorTooManyFilesInDir=Ófært um að skapa skrá í skráasafninu „%1“ vegna þess að það inniheldur of margar skrár

; *** Setup common messages
ExitSetupTitle=Hætta í Uppsetningu
ExitSetupMessage=Uppsetningu er ekki lokið. Ef þú hættir núna mun forritið ekki vera uppsett.%n%nÞú getur keyrt Uppsetningu aftur síðar til að ljúka uppsetningunni.%n%nHætta í Uppsetningu?
AboutSetupMenuItem=&Um Uppsetningu…
AboutSetupTitle=Um Uppsetningu
AboutSetupMessage=%1 %2%n%3%n%n%1-heimasíða:%n%4
AboutSetupNote=
TranslatorNote=Stefán Örvar Sigmundsson (stefan.orvar.sigmundsson@proton.me)

; *** Buttons
ButtonBack=< &Fyrri
ButtonNext=&Næst >
ButtonInstall=&Setja upp
ButtonOK=Í lagi
ButtonCancel=Hætta við
ButtonYes=&Já
ButtonYesToAll=Já við &öllu
ButtonNo=&Nei
ButtonNoToAll=N&ei við öllu
ButtonFinish=&Ljúka
ButtonBrowse=&Vafra…
ButtonWizardBrowse=&Vafra…
ButtonNewFolder=&Skapa nýja möppu

; *** "Select Language" dialog messages
SelectLanguageTitle=Veldu tungumál Uppsetningar
SelectLanguageLabel=Veldu tungumálið sem nota á við uppsetninguna.

; *** Common wizard text
ClickNext=Smelltu á Næst til að halda áfram eða Hætta við til að hætta í Uppsetningu.
BeveledLabel=
BrowseDialogTitle=Vafra eftir möppu
BrowseDialogLabel=Veldu möppu í listanum fyrir neðan, smelltu síðan á Í lagi.
NewFolderName=Ný mappa

; *** "Welcome" wizard page
WelcomeLabel1=Velkomin(n) í [name]-uppsetningaraðstoðarann
WelcomeLabel2=Þetta mun setja upp [name/ver] á tölvuna þína.%n%nÞað er ráðlagt að þú lokir öllum öðrum hugbúnaði áður en haldið er áfram.

; *** "Password" wizard page
WizardPassword=Aðgangsorð
PasswordLabel1=Þessi uppsetning er aðgangsorðsvarin.
PasswordLabel3=Vinsamlega veittu aðgangsorðið, smelltu síðan á Næst til að halda áfram. Aðgangsorð eru hástafanæm.
PasswordEditLabel=&Aðgangsorð:
IncorrectPassword=Aðgangsorðið sem þú slóst inn er ekki rétt. Vinsamlega reyndu aftur.

; *** "License Agreement" wizard page
WizardLicense=Leyfissamningur
LicenseLabel=Vinsamlega lestu hinar eftirfarandi mikilvægu upplýsingar áður en haldið er áfram.
LicenseLabel3=Vinsamlega lestu eftirfarandi leyfissamning. Þú verður að samþykkja skilmála samningsins áður en haldið er áfram með uppsetninguna.
LicenseAccepted=Ég &samþykki samninginn
LicenseNotAccepted=Ég samþykki &ekki samninginn

; *** "Information" wizard pages
WizardInfoBefore=Upplýsingar
InfoBeforeLabel=Vinsamlega lestu hinar eftirfarandi mikilvægu upplýsingar áður en haldið er áfram.
InfoBeforeClickLabel=Þegar þú ert tilbúin(n) til að halda áfram með Uppsetningu, smelltu á Næst.
WizardInfoAfter=Upplýsingar
InfoAfterLabel=Vinsamlega lestu hinar eftirfarandi mikilvægu upplýsingar áður en haldið er áfram.
InfoAfterClickLabel=Þegar þú ert tilbúin(n) til að halda áfram með Uppsetningu, smelltu á Næst.

; *** "User Information" wizard page
WizardUserInfo=Notandaupplýsingar
UserInfoDesc=Vinsamlega sláðu inn upplýsingarnar þínar.
UserInfoName=N&otandanafn:
UserInfoOrg=&Stofnun:
UserInfoSerial=&Raðnúmer:
UserInfoNameRequired=Þú verður að slá inn nafn.

; *** "Select Destination Location" wizard page
WizardSelectDir=Veldu áfangastaðsetningu
SelectDirDesc=Hvar ætti [name] að vera uppsett?
SelectDirLabel3=Uppsetning mun setja upp [name] í hina eftirfarandi möppu.
SelectDirBrowseLabel=Til að halda áfram, smelltu á Næst. Ef þú vilt velja aðra möppu, smelltu á Vafra.
DiskSpaceGBLabel=Að minnsta kosti [gb] GB af lausu diskplássi er krafist.
DiskSpaceMBLabel=Að minnsta kosti [mb] MB af lausu diskplássi er krafist.
CannotInstallToNetworkDrive=Uppsetning getur ekki sett upp á netdrif.
CannotInstallToUNCPath=Uppsetning getur ekki sett upp á UNC-slóð.
InvalidPath=Þú verður að slá inn fulla slóð með drifstaf; til dæmis:%n%nC:\App%n%neða UNC-slóð í sniðinu:%n%n\\server\share
InvalidDrive=Drifið eða UNC-deilingin sem þú valdir er ekki til eða er ekki aðgengileg. Vinsamlega veldu annað.
DiskSpaceWarningTitle=Ekki nóg diskpláss
DiskSpaceWarning=Uppsetning krefst að minnsta kosti %1 KB af lausu plássi til að setja upp en hið valda drif hefur einungis %2 KB tiltæk.%n%nVilt þú halda áfram hvort sem er?
DirNameTooLong=Möppunafnið eða slóðin er of löng.
InvalidDirName=Möppunafnið er ekki gilt.
BadDirName32=Möppunöfn geta ekki innihaldið nein af hinum eftirfarandi rittáknum:%n%n%1
DirExistsTitle=Mappa er til
DirExists=Mappan:%n%n%1%n%ner nú þegar til. Vilt þú setja upp í þá möppu hvort sem er?
DirDoesntExistTitle=Mappa er ekki til
DirDoesntExist=Mappan:%n%n%1%n%ner ekki til. Vilt þú að mappan sé sköpuð?

; *** "Select Components" wizard page
WizardSelectComponents=Veldu íhluti
SelectComponentsDesc=Hvaða íhluti ætti að setja upp?
SelectComponentsLabel2=Veldu íhlutina sem þú vilt setja upp; hreinsaðu íhlutina sem þú vilt ekki setja upp. Smelltu á Næst þegar þú ert tilbúin(n) til að halda áfram.
FullInstallation=Full uppsetning
CompactInstallation=Samanþjöppuð uppsetning
CustomInstallation=Sérsniðin uppsetning
NoUninstallWarningTitle=Íhlutir eru til
NoUninstallWarning=Uppsetning hefur greint það að eftirfarandi íhlutir séu nú þegar uppsettir á tölvunni þinni:%n%n%1%n%nAð afvelja þessa íhluti mun ekki taka þá niður.%n%nVilt þú halda áfram hvort sem er?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Núverandi val krefst að minnsta kosti [gb] GB af diskplássi.
ComponentsDiskSpaceMBLabel=Núverandi val krefst að minnsta kosti [mb] MB af diskplássi.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Veldu aukaleg verk
SelectTasksDesc=Hvaða aukalegu verk ættu að vera framkvæmd?
SelectTasksLabel2=Veldu hin aukalegu verk sem þú vilt að Uppsetning framkvæmi meðan [name] er sett upp, smelltu síðan á Næst.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Veldu Upphafsvalmyndarmöppu
SelectStartMenuFolderDesc=Hvert ætti Uppsetning að setja flýtileiðir forritsins?
SelectStartMenuFolderLabel3=Uppsetning mun skapa flýtileiðir forritsins í hinni eftirfarandi Upphafsvalmyndarmöppu.
SelectStartMenuFolderBrowseLabel=Til að halda áfram, smelltu á Næst. Ef þú vilt velja aðra möppu, smelltu á Vafra.
MustEnterGroupName=Þú verður að slá inn möppunafn.
GroupNameTooLong=Möppunafnið eða slóðin er of löng.
InvalidGroupName=Möppunafnið er ekki gilt.
BadGroupName=Möppunafnið getur ekki innihaldið neitt af hinum eftirfarandi rittáknum:%n%n%1
NoProgramGroupCheck2=&Ekki skapa Upphafsvalmyndarmöppu

; *** "Ready to Install" wizard page
WizardReady=Tilbúin til að setja upp
ReadyLabel1=Uppsetning er núna tilbúin til að hefja uppsetningu [name] á tölvuna þína.
ReadyLabel2a=Smelltu á Setja upp til að halda áfram uppsetningunni eða smelltu á Fyrri ef þú vilt endurskoða eða breyta einhverjum stillingum.
ReadyLabel2b=Smelltu á Setja upp til að halda áfram uppsetningunni.
ReadyMemoUserInfo=Notandaupplýsingar:
ReadyMemoDir=Áfangastaðsetning:
ReadyMemoType=Uppsetningartegund:
ReadyMemoComponents=Valdir íhlutir:
ReadyMemoGroup=Upphafsvalmyndarmappa:
ReadyMemoTasks=Aukaleg verk:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Niðurhlaðandi skrám…
ButtonStopDownload=&Stöðva niðurhleðslu
StopDownload=Ert þú viss um að þú viljir stöðva niðurhleðsluna?
ErrorDownloadAborted=Niðurhleðslu hætt
ErrorDownloadFailed=Niðurhleðsla mistókst: %1 %2
ErrorDownloadSizeFailed=Mistókst að sækja stærð: %1 %2
ErrorProgress=Ógild framvinda: %1 af %2
ErrorFileSize=Ógild skráarstærð: bjóst við %1, fékk %2

; *** TExtractionWizardPage wizard page and ExtractArchive
ExtractingLabel=Dragandi út skrár…
ButtonStopExtraction=&Stöðva útdrátt
StopExtraction=Ert þú viss um að þú viljir stöðva útdrátt?
ErrorExtractionAborted=Útdrætti hætt
ErrorExtractionFailed=Útdráttur mistókst: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=Aðgangsorðið er rangt
ArchiveIsCorrupted=Safnskráin er spillt
ArchiveUnsupportedFormat=Safnskráarsniðið er ekki stutt

; *** "Preparing to Install" wizard page
WizardPreparing=Undirbúandi uppsetningu
PreparingDesc=Uppsetning er að undirbúa uppsetningu [name] á tölvuna þína.
PreviousInstallNotCompleted=Uppsetningu/Fjarlægingu fyrra forrits var ekki lokið. Þú þarft að endurræsa tölvuna þína til að ljúka þeirri uppsetningu.%n%nEftir endurræsingu tölvunnar þinnar, keyrðu Uppsetningu aftur til að ljúka uppsetningu [name].
CannotContinue=Uppsetning getur ekki haldið áfram. Vinsamlega smelltu á Hætta við til að hætta.
ApplicationsFound=Eftirfarandi hugbúnaður er að nota skrár sem þurfa að vera uppfærðar af Uppsetningu. Það er ráðlagt að þú leyfir Uppsetningu sjálfvirkt að loka þessum hugbúnaði.
ApplicationsFound2=Eftirfarandi hugbúnaður er að nota skrár sem þurfa að vera uppfærðar af Uppsetningu. Það er ráðlagt að þú leyfir Uppsetningu sjálfvirkt að loka þessum hugbúnaði. Eftir að uppsetningunni lýkur mun Uppsetning reyna að endurræsa hugbúnaðinn.
CloseApplications=&Sjálfvirkt loka hugbúnaðinum
DontCloseApplications=&Ekki loka hugbúnaðinum
ErrorCloseApplications=Uppsetningu var ófært um að sjálfvirkt loka öllum hugbúnaði. Það er ráðlagt að þú lokir öllum hugbúnaði sem er að nota skrár sem þurfa að vera uppfærðar af Uppsetningu áður en haldið er áfram.
PrepareToInstallNeedsRestart=Uppsetning þarf að endurræsa tölvuna þína. Eftir að hafa endurræst tölvuna þína, keyrðu Uppsetningu aftur til að ljúka uppsetningu [name].%n%nVilt þú endurræsa núna?

; *** "Installing" wizard page
WizardInstalling=Setjandi upp
InstallingLabel=Vinsamlega bíddu meðan Uppsetning setur upp [name] á tölvuna þína.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Ljúkandi [name]-uppsetningaraðstoðaranum
FinishedLabelNoIcons=Uppsetning hefur lokið uppsetningu [name] á tölvuna þína.
FinishedLabel=Uppsetning hefur lokið uppsetningu [name] á tölvuna þína. Hugbúnaðurinn getur verið ræstur með því að velja hinar uppsettu flýtileiðir.
ClickFinish=Smelltu á Ljúka til að hætta í Uppsetningu.
FinishedRestartLabel=Til að ljúka uppsetningu [name] þarf Uppsetning að endurræsa tölvuna þína. Vilt þú endurræsa núna?
FinishedRestartMessage=Til að ljúka uppsetningu [name] þarf Uppsetning að endurræsa tölvuna þína.%n%nVilt þú endurræsa núna?
ShowReadmeCheck=Já, ég vil skoða README-skrána
YesRadio=&Já, endurræsa tölvuna núna
NoRadio=&Nei, ég mun endurræsa tölvuna síðar
RunEntryExec=Keyra %1
RunEntryShellExec=Skoða %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Uppsetning þarfnast næsta disks
SelectDiskLabel2=Vinsamlega settu inn disk %1 og smelltu á Í lagi.%n%nEf skrárnar á þessum disk er hægt að finna í annarri möppu en þeirri sem birt er fyrir neðan, sláðu inn réttu slóðina eða smelltu á Vafra.
PathLabel=&Slóð:
FileNotInDir2=Skrána „%1“ var ekki hægt að staðsetja í „%2“. Vinsamlega settu inn rétta diskinn eða veldu aðra möppu.
SelectDirectoryLabel=Vinsamlega tilgreindu staðsetningu næsta disks.

; *** Installation phase messages
SetupAborted=Uppsetningu var ekki lokið.%n%nVinsamlega leiðréttu vandamálið og keyrðu Uppsetningu aftur.
AbortRetryIgnoreSelectAction=Veldu aðgerð
AbortRetryIgnoreRetry=&Reyna aftur
AbortRetryIgnoreIgnore=&Hunsa villuna og halda áfram
AbortRetryIgnoreCancel=Hætta við uppsetningu
RetryCancelSelectAction=Veldu aðgerð
RetryCancelRetry=&Reyna aftur
RetryCancelCancel=Hætta við

; *** Installation status messages
StatusClosingApplications=Lokandi hugbúnaði…
StatusCreateDirs=Skapandi skráasöfn…
StatusExtractFiles=Dragandi út skrár…
StatusDownloadFiles=Niðurhlaðandi skrám…
StatusCreateIcons=Skapandi flýtileiðir…
StatusCreateIniEntries=Skapandi INI-færslur…
StatusCreateRegistryEntries=Skapandi kerfisskrárfærslur…
StatusRegisterFiles=Skrásetjandi skrár…
StatusSavingUninstall=Vistandi niðurtökuupplýsingar…
StatusRunProgram=Ljúkandi uppsetningu…
StatusRestartingApplications=Endurræsandi hugbúnað…
StatusRollback=Rúllandi aftur breytingum…

; *** Misc. errors
ErrorInternal2=Innri villa: %1
ErrorFunctionFailedNoCode=%1 mistókst
ErrorFunctionFailed=%1 mistókst; kóði %2
ErrorFunctionFailedWithMessage=%1 mistókst; kóði %2.%n%3
ErrorExecutingProgram=Ófært um að keyra skrá:%n%1

; *** Registry errors
ErrorRegOpenKey=Villa við opnun kerfisskrárlykils:%n%1\%2
ErrorRegCreateKey=Villa við sköpun kerfisskrárlykils:%n%1\%2
ErrorRegWriteKey=Villa við ritun í kerfisskrárlykil:%n%1\%2

; *** INI errors
ErrorIniEntry=Villa við sköpun INI-færslu í skránni „%1“.

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Sleppa þessari skrá (ekki ráðlagt)
FileAbortRetryIgnoreIgnoreNotRecommended=&Hunsa villuna og halda áfram (ekki ráðlagt)
SourceIsCorrupted=Upprunaskráin er spillt
SourceDoesntExist=Upprunaskráin „%1“ er ekki til
SourceVerificationFailed=Staðfesting upprunaskrárinnar mistókst: %1
VerificationSignatureDoesntExist=Undirskriftarskráin „%1“ er ekki til
VerificationSignatureInvalid=Undirskriftarskráin „%1“ er ógild
VerificationKeyNotFound=Undirskriftarskráin „%1“ notar óþekktan lykil
VerificationFileNameIncorrect=Nafn skrárinnar er rangt
VerificationFileTagIncorrect=Merki skrárinnar er rangt
VerificationFileSizeIncorrect=Stærð skrárinnar er röng
VerificationFileHashIncorrect=Tætigildi skrárinnar er rangt
ExistingFileReadOnly2=Hina gildandi skrá var ekki hægt að yfirrita því hún er merkt sem skrifvarin.
ExistingFileReadOnlyRetry=&Fjarlægja skrifvarnareigindið og reyna aftur
ExistingFileReadOnlyKeepExisting=&Halda hinni gildandi skrá
ErrorReadingExistingDest=Villa kom upp meðan reynt var að lesa gildandi skrána:
FileExistsSelectAction=Veldu aðgerð
FileExists2=Skráin er nú þegar til.
FileExistsOverwriteExisting=&Yfirrita hina gildandi skrá
FileExistsKeepExisting=&Halda hinni gildandi skrá
FileExistsOverwriteOrKeepAll=&Gera þetta við næstu árekstra
ExistingFileNewerSelectAction=Veldu aðgerð
ExistingFileNewer2=Hin gildandi skrá er nýrri en sú sem Uppsetning er að reyna að setja upp.
ExistingFileNewerOverwriteExisting=&Yfirrita hina gildandi skrá
ExistingFileNewerKeepExisting=&Halda hinni gildandi skrá (ráðlagt)
ExistingFileNewerOverwriteOrKeepAll=&Gera þetta við næstu árekstra
ErrorChangingAttr=Villa kom upp meðan reynt var að breyta eigindum gildandi skráarinnar:
ErrorCreatingTemp=Villa kom upp meðan reynt var að skapa skrá í áfangaskráasafninu:
ErrorReadingSource=Villa kom upp meðan reynt var að lesa upprunaskrána:
ErrorCopying=Villa kom upp meðan reynt var að afrita skrá:
ErrorDownloading=Villa kom upp meðan reynt var að niðurhlaða skrá:
ErrorExtracting=Villa kom upp meðan reynt var að draga út safnskrá:
ErrorReplacingExistingFile=Villa kom upp meðan reynt var að yfirrita gildandi skrána:
ErrorRestartReplace=RestartReplace mistókst:
ErrorRenamingTemp=Villa kom upp meðan reynt var að endurnefna skrá í áfangaskráasafninu:
ErrorRegisterServer=Ófært um að skrá DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 mistókst með skilakóðann %1
ErrorRegisterTypeLib=Ófært um að skrá tegundasafnið: %1

; *** Uninstall display name markings
UninstallDisplayNameMark=%1 (%2)
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bita
UninstallDisplayNameMark64Bit=64-bita
UninstallDisplayNameMarkAllUsers=Allir notendur
UninstallDisplayNameMarkCurrentUser=Núverandi notandi

; *** Post-installation errors
ErrorOpeningReadme=Villa kom upp meðan reynt var að opna README-skrána.
ErrorRestartingComputer=Uppsetningu tókst ekki að endurræsa tölvuna. Vinsamlega gerðu þetta handvirkt.

; *** Uninstaller messages
UninstallNotFound=Skráin „%1“ er ekki til. Getur ekki tekið niður.
UninstallOpenError=Skrána „%1“ var ekki hægt að opna. Getur ekki tekið niður
UninstallUnsupportedVer=Niðurtökuatburðaskráin „%1“ er í sniði sem er ekki þekkt af þessari útgáfu af Niðurtöku. Getur ekki tekið niður
UninstallUnknownEntry=Óþekkt færsla (%1) var fundin í niðurtökuatburðaskránni
ConfirmUninstall=Ert þú viss um að þú viljir algjörlega fjarlægja %1 og alla íhluti þess?
UninstallOnlyOnWin64=Þessa uppsetningu er einungis hægt að taka niður á 64-bita Windows.
OnlyAdminCanUninstall=Þessi uppsetning getur einungis verið tekin niður af notanda með stjórnandaréttindi.
UninstallStatusLabel=Vinsamlega bíddu meðan %1 er fjarlægt úr tölvunni þinni.
UninstalledAll=%1 var giftusamlega fjarlægt af tölvunni þinni.
UninstalledMost=%1-niðurtöku lokið.%n%nSuma liði var ekki hægt að fjarlægja. Þá er hægt að fjarlægja handvirkt.
UninstalledAndNeedsRestart=Til að ljúka niðurtöku %1 þarf að endurræsa tölvuna þína.%n%nVilt þú endurræsa núna?
UninstallDataCorrupted=Skráin „%1“ er spillt. Getur ekki tekið niður

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Fjarlægja deilda skrá?
ConfirmDeleteSharedFile2=Kerfið gefur til kynna að hin eftirfarandi deilda skrá sé ekki lengur í notkun hjá neinu forriti. Vilt þú að Niðurtaka fjarlægi þessa deildu skrá?%n%nEf einhver forrit eru enn að nota þessa skrá og hún er fjarlægð kann að vera að þau forrit muni ekki virka almennilega. Ef þú ert óviss, veldu Nei. Að skilja skrána eftir á kerfinu þínu mun ekki valda skaða.

SharedFileNameLabel=Skráarnafn:
SharedFileLocationLabel=Staðsetning:
WizardUninstalling=Niðurtökustaða
StatusUninstalling=Takandi niður %1…

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Setjandi upp %1.
ShutdownBlockReasonUninstallingApp=Takandi niður %1.

[CustomMessages]

NameAndVersion=%1 útgáfa %2
AdditionalIcons=Aukalegar flýtileiðir:
CreateDesktopIcon=Skapa &skjáborðsflýtileið
CreateQuickLaunchIcon=Skapa Skyndi&ræsiflýtileið
ProgramOnTheWeb=%1 á vefnum
UninstallProgram=Niðurtaka %1
LaunchProgram=Ræsa %1
AssocFileExtension=&Tengja %1 við %2-skráarendinguna
AssocingFileExtension=Tengjandi %1 við %2-skráarendinguna…
AutoStartProgramGroupDescription=Ræsing:
AutoStartProgram=Sjálfvirkt ræsa %1
AddonHostProgramNotFound=%1 var ekki fundið í möppunni sem þú valdir.%n%nVilt þú halda áfram hvort sem er?