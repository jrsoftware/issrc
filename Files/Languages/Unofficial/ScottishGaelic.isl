; *** Inno Setup version 6.0.0+ Scottish Gaelic messages ***
;
; Translation by GunChleoc <fios@foramnagaidhlig.net>
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and
; understand the '[LangOptions] section' topic in the help file.
LanguageName=G<00E0>idhlig
LanguageID=$0491
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
SetupAppTitle=Stàladh
SetupWindowTitle=A’ stàladh %1
UninstallAppTitle=Dì-stàladh
UninstallAppFullTitle=A’ dì-stàladh %1

; *** Misc. common
InformationTitle=Fiosrachadh
ConfirmTitle=Dearbhadh
ErrorTitle=Mearachd

; *** SetupLdr messages
SetupLdrStartupMessage=Thèid %1 a stàladh a-nis. A bheil thu airson leantainn air adhart?
LdrCannotCreateTemp=Cha b’ urrainn dhuinn faidhle sealach a chruthachadh. Chaidh sgur dhen stàladh
LdrCannotExecTemp=Cha b’ urrainn dhuinn am faidhle a ruith sa phasgan shealach. Chaidh sgur dhen stàladh

; *** Startup error messages
LastErrorMessage=%1.%n%nMearachd %2: %3
SetupFileMissing=Tha am faidhle %1 a dhìth sa phasgan stàlaidh. Feuch an càraich thu an duilgheadas seo no faigh lethbhreac ùr dhen phrògram.
SetupFileCorrupt=Tha na faidhlichean stàlaidh coirbte. Feuch am faigh thu lethbhreac ùr dhen phrògram.
SetupFileCorruptOrWrongVer=Tha na faidhlichean stàlaidh coirbte no neo-chòrdail ris an tionndadh seo aig an stàladh. Feuch an càraich thu an duilgheadas seo no faigh lethbhreac ùr dhen phrògram.
InvalidParameter=Chaidh paramadair mì-dhligheach a shìneadh air an loidhne-àithne:%n%n%1
SetupAlreadyRunning=Tha an stàladh ’ga ruith mu thràth.
WindowsVersionNotSupported=Cha chuir am prògram seo taic ris an tionndadh aig Windows a tha an coimpiutair agad a’ ruith.
WindowsServicePackRequired=Tha %1 pacaid seirbheise %2 no tionndadh nas ùire dhith a dhìth air a’ phrògram seo.
NotOnThisPlatform=Chan urrainn dhut am prògram seo a ruith fo %1.
OnlyOnThisPlatform=Feumaidh tu am prògram seo a ruith fo %1.
OnlyOnTheseArchitectures=Chan urrainn dhut am prògram seo a ruith ach air tionndaidhean Windows a chuireas taic ri ailtireachdan nam pròiseasar seo:%n%n%1
WinVersionTooLowError=Tha feum air %1 tionndadh %2 no nas ùire airson a’ phrògraim seo.
WinVersionTooHighError=Cha ghabh am prògram seo a stàladh fo %1 tionndadh %2 no nas ùire.
AdminPrivilegesRequired=Feumaidh tu clàradh a-steach mar rianaire gus am prògram seo a stàladh.
PowerUserPrivilegesRequired=Feumaidh tu clàradh a-steach mar rianaire no mar bhall dhen bhuidheann Power Users gus am prògram seo a stàladh.
SetupAppRunningError=Mhothaich an stàladh gu bheil %1 ’ga ruith an-dràsta.%n%nDùin gach ionstans a tha a’ ruith an-dràsta is briog air “Ceart ma-thà” air neo briog air “Sguir dheth” gus an stàladh fhàgail.
UninstallAppRunningError=Mhothaich an dì-stàladh gu bheil %1 ’ga ruith an-dràsta.%n%nDùin gach ionstans a tha a’ ruith an-dràsta is briog air “Ceart ma-thà” air neo briog air “Sguir dheth” gus an dì-stàladh fhàgail.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Suidhich modh an stàlaidh
PrivilegesRequiredOverrideInstruction=Tagh modh an stàlaidh
PrivilegesRequiredOverrideText1=Gabhaidh %1 a stàladh dha na h-uile cleachdaiche (bidh feum air pribhleidean rianaire) no dhut-sa a-mhàin.
PrivilegesRequiredOverrideText2=Gabhaidh %1 a stàladh dhut-sa a-mhàin no dha na h-uile cleachdaiche (bidh feum air pribhleidean rianaire).
PrivilegesRequiredOverrideAllUsers=Stàlaich dh&a na h-uile cleachdaiche
PrivilegesRequiredOverrideAllUsersRecommended=Stàlaich dh&a na h-uile cleachdaiche (mholamaid seo)
PrivilegesRequiredOverrideCurrentUser=Stàlaich dho&mh-sa a-mhàin
PrivilegesRequiredOverrideCurrentUserRecommended=Stàlaich dho&mh-sa a-mhàin (mholamaid seo)

; *** Misc. errors
ErrorCreatingDir=Cha b’ urrainn dhan stàladh am pasgan "%1" a chruthachadh
ErrorTooManyFilesInDir=Tha faidhle ann nach b’ urrainn dhan stàladh cruthachadh sa phasgan “%1” on a tha cus fhaidhlichean ann

; *** Setup common messages
ExitSetupTitle=Fàg an stàladh
ExitSetupMessage=Chan eil an stàladh coileanta fhathast. Ma sguireas tu dheth an-dràsta, cha dèid am prògram a stàladh.%n%n’S urrainn dhut an stàladh a dhèanamh a-rithist uaireigin eile gus a choileanadh.%n%nA bheil thu airson an stàladh fhàgail?
AboutSetupMenuItem=&Mun stàladh …
AboutSetupTitle=Mun stàladh
AboutSetupMessage=%1 Tionndadh %2%n%3%n%n%1 Duilleag-lìn:%n%4
AboutSetupNote=
TranslatorNote=An t-eadar-theangachadh le GunChleoc (fios@foramnagaidhlig.net)

; *** Buttons
ButtonBack=< Air ai&s
ButtonNext=Air adha&rt >
ButtonInstall=&Stàlaich
ButtonOK=Ceart ma-thà
ButtonCancel=Sguir dheth
ButtonYes=&Tha
ButtonYesToAll=Th&a dhan a h-uile
ButtonNo=&Chan eil
ButtonNoToAll=Cha&n eil dhan a h-uile
ButtonFinish=&Crìochnaich
ButtonBrowse=Rùrai&ch …
ButtonWizardBrowse=&Rùraich …
ButtonNewFolder=&Cruthaich pasgan ùr

; *** "Select Language" dialog messages
SelectLanguageTitle=Tagh cànan an stàlaidh
SelectLanguageLabel=Tagh an cànan a chleachdas an t-inneal-stàlaidh seo.

; *** Common wizard text
ClickNext=Briog air “Air adhart” gus leantainn air adhart no air “Sguir dheth” gus fàgail an-seo.
BeveledLabel=
BrowseDialogTitle=Lorg pasgan
BrowseDialogLabel=Tagh pasgan is briog air “Ceart ma-thà” an uairsin.
NewFolderName=Pasgan ùr

; *** "Welcome" wizard page
WelcomeLabel1=Fàilte dhan draoidh stàlaidh aig [name]
WelcomeLabel2=Stàlaichidh an draoidh seo [name/ver] air a’ choimpiutair agad a-nis.%n%nBu chòir dhut crìoch a chur air a h-uile aplacaid eile mus lean thu air adhart leis an stàladh.

; *** "Password" wizard page
WizardPassword=Facal-faire
PasswordLabel1=Tha an stàladh seo dìonta le facal-faire.
PasswordLabel3=Cuir a-steach am facal-faire is briog air “Air adhart” an uairsin. Thoir an aire air litrichean mòra is beaga.
PasswordEditLabel=&Facal-faire:
IncorrectPassword=Chan eil am facal-faire a chuir thu ann mar bu chòir. Am feuch thu ris a-rithist?

; *** "License Agreement" wizard page
WizardLicense=Aonta ceadachais
LicenseLabel=An leugh thu am fiosrachadh cudromach seo mus lean thu air adhart?
LicenseLabel3=Feuch an leugh thu an t-aonta ceadachais seo. Feumaidh tu gabhail ri teirmichean an aonta mus fhaod thu leantainn air adhart.
LicenseAccepted=&Gabhaidh mi ris an aonta
LicenseNotAccepted=&Diùltaidh mi an t-aonta

; *** "Information" wizard pages
WizardInfoBefore=Fiosrachadh
InfoBeforeLabel=An leugh thu am fiosrachadh cudromach seo mus lean thu air adhart?
InfoBeforeClickLabel=Nuair a bhios tu deiseil gus leantainn air adhart, briog air “Air adhart.”
WizardInfoAfter=Fiosrachadh
InfoAfterLabel=An leugh thu am fiosrachadh cudromach seo mus lean thu air adhart?
InfoAfterClickLabel=Nuair a bhios tu deiseil gus leantainn air adhart, briog air “Air adhart.”

; *** "User Information" wizard page
WizardUserInfo=Fiosrachadh a’ chleachdaiche
UserInfoDesc=An cuir thu a-steach an dàta agad?
UserInfoName=&Ainm:
UserInfoOrg=&Eagrachas:
UserInfoSerial=Àireamh &shreathach:
UserInfoNameRequired=Feumaidh tu ainm a chur a-steach.

; *** "Select Destination Location" wizard page
WizardSelectDir=Tagh am pasgan-amais
SelectDirDesc=Càite an dèid [name] a stàladh?
SelectDirLabel3=Thèid [name] a stàladh sa phasgan seo.
SelectDirBrowseLabel=Briog air “Air adhart” gus leantainn air adhart. Briog air “Rùraich” ma tha thu airson pasgan eile a thaghadh.
DiskSpaceMBLabel=Bidh feum air co-dhiù [mb] MB de rùm sàbhalaidh saor.
CannotInstallToNetworkDrive=Cha ghabh seo stàladh air draibh lìonraidh.
CannotInstallToUNCPath=Cha ghabh seo stàladh air slighe UNC.
InvalidPath=Feumaidh tu slighe iomlan le litir draibh a thoirt seachad; m.e.:%n%nC:\Ball-eisimpleir%n%nno slighe UNC leis a' chruth:%n%n\\Frithealaiche\Co-roinneadh
InvalidDrive=Chan eil an draibh no an t-slighe UNC a thug thu seachad ann no chan urrainn dhuinn inntrigeadh. Feuch an tagh thu pasgan eile.
DiskSpaceWarningTitle=Chan eil rùm saor gu leòr ann
DiskSpaceWarning=Cha feum air co-dhiù %1 KB de rùm saor airson an stàlaidh, ach chan eil ach %2 KB ri làimh air an draibh a thagh thu.%n%nA bheil thu airson leantainn air adhart co-dhiù?
DirNameTooLong=Tha ainm a’ phasgain/na slighe ro fhada.
InvalidDirName=Chan eil ainm a’ phasgain dligheach.
BadDirName32=Chan fhaod na caractaran seo a bhith ann an ainm pasgain:%n%n%1
DirExistsTitle=Tha am pasgan ann mu thràth
DirExists=Tha am pasgan:%n%n%1%n%nann mu thràth. A bheil thu airson a stàladh sa phasgan seo co-dhiù?
DirDoesntExistTitle=Chan eil am pasgan ann
DirDoesntExist=Chan eil am pasgan:%n%n%1%n%nann. A bheil thu airson a chruthachadh?

; *** "Select Components" wizard page
WizardSelectComponents=Tagh co-phàirtean
SelectComponentsDesc=Dè na co-phàirtean a thèid a stàladh?
SelectComponentsLabel2=Tagh na co-phàirtean a tha thu airson stàladh. Briog air “Air adhart” nuair a bhios tu ullamh.
FullInstallation=Stàladh slàn
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Stàladh beag
CustomInstallation=Stàladh gnàthaichte
NoUninstallWarningTitle=Tha co-phàirtean ann
NoUninstallWarning=Mhothaich an stàladh gun deach na co-phàirtean seo a stàladh air a’ choimpiutair agad roimhe:%n%n%1%n%nCha dèid na co-phàirtean seo nach do thagh thu tuilleadh a thoirt air falbh on choimpiutair agad.%n%nA bheil thu airson leantainn air adhart co-dhiù?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Thèid co-dhiù [mb] MB de rùm a chleachdadh airson na thagh thu.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Saothraichean a bharrachd
SelectTasksDesc=Dè na saothraichean a bharrachd a thèid a ruith?
SelectTasksLabel2=Tagh na saothraichean a bharrachd a tha thu airson ruith leis an stàladh aig [name] is briog air “Air adhart” an uairsin.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Tagh pasgan ann an “Tòisich”
SelectStartMenuFolderDesc=Càite an cruthaich an stàladh na ceanglaichean dhan phrògram?
SelectStartMenuFolderLabel3=Cruthaichidh an stàladh na ceanglaichean dhan phrògram sa phasgan seo ann an “Tòisich.”
SelectStartMenuFolderBrowseLabel=Briog air “Air adhart” gus leantainn air adhart. Briog air “Rùraich” ma tha thu airson pasgan eile a thaghadh.
MustEnterGroupName=Feumaidh tu ainm pasgain a chur a-steach.
GroupNameTooLong=Tha ainm a’ phasgain/na slighe ro fhada.
InvalidGroupName=Chan eil ainm a’ phasgain dligheach.
BadGroupName=Chan fhaod na caractaran seo a bhith ann an ainm pasgain:%n%n%1
NoProgramGroupCheck2=&Na cruthaich pasgan sam bith ann an “Tòisich.”

; *** "Ready to Install" wizard page
WizardReady=Deiseil airson an stàlaidh
ReadyLabel1=Tha an draoidh stàlaidh deiseil gus [name] a stàladh air a’ choimpiutair agad.
ReadyLabel2a=Briog air “Stàlaich” gus tòiseachadh air an stàladh no air “Air ais” gus sùil a thoirt air na roghainnean no gus an atharrachadh.
ReadyLabel2b=Briog air “Stàlaich” gus tòiseachadh air an stàladh.
ReadyMemoUserInfo=Fiosrachadh a’ chleachdaiche:
ReadyMemoDir=Pasgan-amais:
ReadyMemoType=Seòrsa an stàlaidh:
ReadyMemoComponents=Co-phàirtean air an taghadh:
ReadyMemoGroup=Pasgan ann an “Tòisich”:
ReadyMemoTasks=Saothraichean a bharrachd:

; *** "Preparing to Install" wizard page
WizardPreparing=Ag ullachadh an stàlaidh
PreparingDesc=Tha an stàladh aig [name] air a’ choimpiutair seo ’ga ullachadh.
PreviousInstallNotCompleted=Chaidh prògram eile a stàladh/a dhì-stàladh roimhe ’s cha deach sin a choileanadh. Feumaidh tu an coimpiutair ath-thòiseachadh gus crìoch a chur air an stàladh/dì-stàladh sin.%n%nAn dèidh dhut an coimpiutair agad ath-thòiseachadh, tòisich an stàladh a-rithist gus [name] a stàladh.
CannotContinue=Chan urrainn dhan stàladh leantainn air adhart. Feuch am briog thu air “Sguir dheth” gus fàgail an-seo.
ApplicationsFound=Tha na h-aplacaidean seo a’ cleachdadh faidhlichean a dh’fheumas an stàladh ùrachadh. Mholamaid gun ceadaich thu gun dùin an stàladh na h-aplacaidean sin gu fèin-obrachail.
ApplicationsFound2=Tha na h-aplacaidean seo a’ cleachdadh faidhlichean a dh’fheumas an stàladh ùrachadh. Mholamaid gun ceadaich thu gun dùin an stàladh na h-aplacaidean sin gu fèin-obrachail. Nuair a bhios an stàladh deiseil, feuchaidh sinn ris na h-aplacaidean sin ath-thòiseachadh.
CloseApplications=&Dùin na h-aplacaidean gu fèin-obrachail
DontCloseApplications=&Na dùin na h-aplacaidean
ErrorCloseApplications=Cha deach leis an stàladh a h-uile aplacaid a dhùnadh gu fèin-obrachail. Mus lean thu air adhart, mholamaid gun dùin thu a h-uile aplacaid a chleachdas faidhlichean a dh’fheumas an stàladh ùrachadh.

; *** "Installing" wizard page
WizardInstalling=’Ga stàladh
InstallingLabel=Fuirich ort fhad ’s a tha [name] ’ga stàladh air a’ choimpiutair agad.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=A’ crìochnachadh an draoidh stàlaidh aig [name]
FinishedLabelNoIcons=Tha sinn deiseil a’ stàladh [name] air a’ choimpiutair agad.
FinishedLabel=Tha sinn deiseil a’ stàladh [name] air a’ choimpiutair agad. ’S urrainn dhut am prògram a thòiseachadh a-nis leis na ceanglaichean dhan phrògram a chaidh a stàladh.
ClickFinish=Briog air “Crìochnaich” gus crìoch a chur air an stàladh.
FinishedRestartLabel=Feumaidh sinn an coimpiutair ath-thòiseachadh gus an stàladh aig [name] a choileanadh. An dèan sinn seo dhut an-dràsta?
FinishedRestartMessage=Feumaidh sinn an coimpiutair ath-thòiseachadh gus an stàladh aig [name] a choileanadh.%n%nAn dèan sinn seo dhut an-dràsta?
ShowReadmeCheck=Tha mi airson am faidhle LEUGHMI a shealltainn
YesRadio=&Nì, ath-thòisichibh an coimpiutair dhomh an-dràsta
NoRadio=&Cha dèan, ath-thòisichidh mi fhìn an coimpiutair uaireigin eile
; used for example as 'Run MyProg.exe'
RunEntryExec=Cuir %1 gu dol
; used for example as 'View Readme.txt'
RunEntryShellExec=Seall %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Tha an t-ath-chlàr a dhìth aig an stàladh
SelectDiskLabel2=Cuir a-steach clàr %1 is briog air “Ceart ma-thà.”%n%nMur eil na faidhlichean on chlàir-shùbailte seo sa phasgan a tha ’ga shealltainn dhut, cuir a-steach an t-slighe cheart no briog air “Rùraich.”
PathLabel=&Slighe:
FileNotInDir2=Chan eil am faidhle “%1” an-seo: “%2.” Feuch an atharraich thu am pasgan no an cuir thu a-steach clàr-sùbailte eile.
SelectDirectoryLabel=Sònraich càite an dèid an t-ath-chlàr a chur a-steach.

; *** Installation phase messages
SetupAborted=Cha b’ urrainn dhuinn an stàladh a choileanadh.%n%nFeuch an càraich thu an duilgheadas is tòisich air an stàladh a-rithist.
AbortRetryIgnoreSelectAction=Tagh gnìomh
AbortRetryIgnoreRetry=Feuch ris a-ri&thist
AbortRetryIgnoreIgnore=Le&ig seachad a’ mhearachd is lean air adhart
AbortRetryIgnoreCancel=Sguir dhen stàladh

; *** Installation status messages
StatusClosingApplications=A’ dùnadh aplacaidean …
StatusCreateDirs=A’ cruthachadh pasganan …
StatusExtractFiles=A’ dì-dhùmhlachadh faidhlichean …
StatusCreateIcons=A’ cruthachadh ceanglaichean …
StatusCreateIniEntries=A’ cruthachadh innteartan INI …
StatusCreateRegistryEntries=A’ cruthachadh innteartan na clàr-lainn …
StatusRegisterFiles=A’ clàradh faidhlichean …
StatusSavingUninstall=A’ sàbhaladh fiosrachadh dì-stàlaidh …
StatusRunProgram=A’ crìochnachadh an stàlaidh …
StatusRestartingApplications=Ag ath-thòiseachadh nan aplacaidean …
StatusRollback=A’ neo-dhèanamh nan atharraichean …

; *** Misc. errors
ErrorInternal2=Mearachd inntearnail: %1
ErrorFunctionFailedNoCode=Dh’fhàillig le %1
ErrorFunctionFailed=Dh’fhàillig le %1; còd %2
ErrorFunctionFailedWithMessage=Dh’fhàillig le %1; còd %2.%n%3
ErrorExecutingProgram=Cha ghabh am faidhle a ruith:%n%1

; *** Registry errors
ErrorRegOpenKey=Cha b’ urrainn dhuinn iuchair na clàr-lainn fhosgladh:%n%1\%2
ErrorRegCreateKey=Cha b’ urrainn dhuinn iuchair na clàr-lainn a chruthachadh:%n%1\%2
ErrorRegWriteKey=Mearachd le sgrìobhadh iuchair na clàr-lainn:%n%1\%2

; *** INI errors
ErrorIniEntry=Mearachd le cruthachadh innteart INI san fhaidhle “%1.”

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Geàrr leum thar an fhaidhle seo (cha mholamaid seo)
FileAbortRetryIgnoreIgnoreNotRecommended=Le&ig seachad a’ mhearachd is lean air adhart (cha mholamaid seo)
SourceIsCorrupted=Tha am faidhle tùsail coirbte
SourceDoesntExist=Chan eil am faidhle tùsail “%1” ann
ExistingFileReadOnly2=Cha b’ urrainn dhuinn am faidhle ùr a chur an àite an t-seann-fhir on a tha comharra ri leughadh a-mhàin ris.
ExistingFileReadOnlyRetry=Thoi&r air falbh an comharra gu bheil e ri leughadh a-mhàin ’s feuch ris a-rithist
ExistingFileReadOnlyKeepExisting=&Cùm am faidhle a tha ann mu thràth
ErrorReadingExistingDest=Mearachd leughaidh san fhaidhle:
FileExists=Tha am faidhle seo ann mu thràth.%n%nA bheil thu airson sgrìobhadh thairis air?
ExistingFileNewer=Tha am faidhle a tha ann mu thràth nas ùire na am faidhle a tha thu airson stàladh. Mholamaid gun cùm thu am faidhle a tha ann mu thràth.%n%n A bheil thu airson am faidhle a chumail a tha ann mu thràth?
ErrorChangingAttr=Thachair mearachd le atharrachadh nan gleusan aig an fhaidhle a tha ann mu thràth:
ErrorCreatingTemp=Thachair mearachd a’ feuchainn ri faidhle a chruthachadh sa phasgan-amais:
ErrorReadingSource=Thachair mearachd a’ feuchainn ris am faidhle tùsail a leughadh:
ErrorCopying=Thachair mearachd a’ feuchainn ri lethbhreac a dhèanamh de dh’fhaidhle:
ErrorReplacingExistingFile=Thachair mearachd le feuchainn ri cur an àite an fhaidhle a tha ann:
ErrorRestartReplace=Dh’fhàillig le ath-thòiseachadh/cur ’na àite:
ErrorRenamingTemp=Thachair mearachd a’ feuchainn ri ainm ùr a thoirt air faidhle sa phasgan-amais:
ErrorRegisterServer=Cha ghabh an DLL/OCX a chlàradh: %1
ErrorRegSvr32Failed=Dh’fhàillig RegSvr32 le còd fàgail %1
ErrorRegisterTypeLib=Cha ghabh leabhar-lann nan seòrsa a chlàradh: %1

; *** Uninstall display name markings
UninstallDisplayNameMark=%1 (%2)
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-biod
UninstallDisplayNameMark64Bit=64-biod
UninstallDisplayNameMarkAllUsers=Na h-uile cleachdaiche
UninstallDisplayNameMarkCurrentUser=An cleachdaiche làithreach

; *** Post-installation errors
ErrorOpeningReadme=Mearachd le fosgladh an fhaidhle LEUGHMI.
ErrorRestartingComputer=Cha deach leis an stàladh an coimpiutair agad ath-thòiseachadh. An dèan thu an t-ath-thòiseachadh a làimh?

; *** Uninstaller messages
UninstallNotFound=Chan eil am faidhle “%1” ann. Dh’fhàillig le dì-stàladh na h-aplacaid.
UninstallOpenError=Cha b’ urrainn dhuinn am faidhle “%1” fhosgladh. Dh’fhàillig le dì-stàladh na h-aplacaid
UninstallUnsupportedVer=Cha b’ urrainn dhuinn mothachadh dè am fòrmat a th’ air an fhaidhle dì-stàlaidh “%1.” Dh’fhàillig le dì-stàladh na h-aplacaid
UninstallUnknownEntry=Tha innteart neo-aithnichte (%1) san loga dì-stàlaidh
ConfirmUninstall=A bheil thu cinnteach bu bheil thu airson %1 is a h-uile co-phàirt aige a thoirt air falbh?
UninstallOnlyOnWin64=Chan urrainn dhuinn an stàladh seo a thoirt air falbh ach fo thionndaidhean Windows 64-biod.
OnlyAdminCanUninstall=Chan fhaod ach cleachdaiche le pribhleidean rianaire an stàladh seo a thoirt air falbh.
UninstallStatusLabel=Fuirich ort fhad ’s a tha %1 ’ga dhì-stàladh on choimpiutair agad.
UninstalledAll=Chaidh %1 a thoirt air falbh on choimpiutair agad.
UninstalledMost=Tha an dì-stàladh aig %1 deiseil.%n%nTha co-phàirtean ann nach b’ urrainn dhuinn toirt air falbh. ’S urrainn dhut fhèin an sguabadh às a làimh.
UninstalledAndNeedsRestart=Feumaidh sinn an coimpiutair agad ath-thòiseachadh gus an dì-stàladh aig %1 a choileanadh.%n%nAn dèan sinn seo dhut an-dràsta?
UninstallDataCorrupted=Tha am faidhle “%1” coirbte. Dh’fhàillig le dì-stàladh na h-aplacaid.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=A bheil thu airson am faidhle co-roinnte a sguabadh às?
ConfirmDeleteSharedFile2=Tha an siostam ag innse nach dèid am faidhle co-roinnte seo a chleachdadh le prògram sam bith eile. A bheil thu airson ’s gun sguab sinn às dha?%nMa tha prògraman eile ann a chleachdas am faidhle seo fhathast is ma thèid a thoirt air falbh, dh’fhaoidte nach obraich na prògraman ud mar bu chòir tuilleadh. Ma tha thu mì-chinnteach, tagh “Chan eil” gus am faidhle fhàgail san t-siostam. Cha dèan e cron dhan t-siostam agad ma chumas tu am faidhle seo air.
SharedFileNameLabel=Ainm an fhaidhle:
SharedFileLocationLabel=Pasgan:
WizardUninstalling=Staid an dì-stàlaidh
StatusUninstalling=A’ dì-stàladh %1 ...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=A’ stàladh %1.
ShutdownBlockReasonUninstallingApp=A’ dì-stàladh %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 tionndadh %2
AdditionalIcons=Ìomhaigheagan a bharrachd:
CreateDesktopIcon=Cruthaich ìomhaigheag air an &deasg
CreateQuickLaunchIcon=Cruthaich ìomhaigheag &grad-thòiseachaidh
ProgramOnTheWeb=%1 air an eadar-lìon
UninstallProgram=Dì-stàlaich %1
LaunchProgram=Cuir %1 gu dol
AssocFileExtension=&Clàraich %1 leis an leudachan fhaidhle %2
AssocingFileExtension=A’ clàradh %1 leis an leudachan fhaidhle %2 ...
AutoStartProgramGroupDescription=Tòiseachadh:
AutoStartProgram=Tòisich %1 gu fèin-obrachail
AddonHostProgramNotFound=Cha deach %1 a lorg sa phasgan a thagh thu.%n%nA bheil thu airson leantainn air adhart co-dhiù?
