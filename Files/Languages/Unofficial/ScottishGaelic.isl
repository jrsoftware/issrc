; *** Inno Setup version 5.5.3+ Scottish Gaelic messages ***
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
SetupAppTitle=St�ladh
SetupWindowTitle=St�ladh - %1
UninstallAppTitle=D�-st�ladh
UninstallAppFullTitle=�Ga dh�-st�ladh %1

; *** Misc. common
InformationTitle=Fiosrachadh
ConfirmTitle=Dearbhaich
ErrorTitle=Mearachd

; *** SetupLdr messages
SetupLdrStartupMessage=Th�id %1 a st�ladh a-nis. A bheil thu airson leantainn air adhart?
LdrCannotCreateTemp=Cha b� urrainn dhuinn faidhle sealach a chruthachadh. Chaidh sgur dhen st�ladh
LdrCannotExecTemp=Cha b� urrainn dhuinn am faidhle a ruith sa phasgan shealach. Chaidh sgur dhen st�ladh

; *** Startup error messages
LastErrorMessage=%1.%n%nMearachd %2: %3
SetupFileMissing=Tha am faidhle %1 a dh�th sa phasgan st�laidh. Feuch an c�raich thu an duilgheadas seo no faigh lethbhreac �r dhen phr�gram.
SetupFileCorrupt=Tha na faidhlichean st�laidh coirbte. Feuch am faigh thu lethbhreac �r dhen phr�gram.
SetupFileCorruptOrWrongVer=Tha na faidhlichean st�laidh coirbte no neo-ch�rdail ris an tionndadh seo aig an st�ladh. Feuch an c�raich thu an duilgheadas seo no faigh lethbhreac �r dhen phr�gram.
InvalidParameter=Chaidh paramadair m�-dhligheach a sh�neadh air an loidhne-�ithne:%n%n%1
SetupAlreadyRunning=Tha an st�ladh �ga ruith mu thr�th.
WindowsVersionNotSupported=Cha chuir am pr�gram seo taic ris an tionndadh aig Windows a tha an coimpiutair agad a� ruith.
WindowsServicePackRequired=Tha %1 pacaid seirbheise %2 no tionndadh nas �ire dhith a dh�th air a� phr�gram seo.
NotOnThisPlatform=Chan urrainn dhut am pr�gram seo a ruith fo %1.
OnlyOnThisPlatform=Feumaidh tu am pr�gram seo a ruith fo %1.
OnlyOnTheseArchitectures=Chan urrainn dhut am pr�gram seo a ruith ach air tionndaidhean Windows a chuireas taic ri ailtireachdan nam pr�iseasar seo:%n%n%1
MissingWOW64APIs=Chan eil na foincseanan a tha a feumach airson st�ladh 64-biod a dh�anamh aig an tionndadh aig Windows a tha thu a� ruith. Feuch an st�laich thu a� phacaid sheirbheise %1 gus an duilgheadas seo a ch�radh.
WinVersionTooLowError=Tha %1 tionndadh %2 no nas �ire a dh�th airson a� phr�graim seo.
WinVersionTooHighError=Cha ghabh am pr�gram seo a st�ladh fo %1 tionndadh %2 no nas �ire.
AdminPrivilegesRequired=Feumaidh tu cl�radh a-steach mar rianaire gus am pr�gram seo a st�ladh.
PowerUserPrivilegesRequired=Feumaidh tu cl�radh a-steach mar rianaire no mar bhall dhen bhuidheann Power Users gus am pr�gram seo a st�ladh.
SetupAppRunningError=Mhothaich an st�ladh gu bheil %1 �ga ruith an-dr�sta.%n%nD�in gach ionstans a tha a� ruith an-dr�sta is briog air �Ceart ma-th�� air neo briog air �Sguir dheth� gus an st�ladh fh�gail.
UninstallAppRunningError=Mhothaich an d�-st�ladh gu bheil %1 �ga ruith an-dr�sta.%n%nD�in gach ionstans a tha a� ruith an-dr�sta is briog air �Ceart ma-th�� air neo briog air �Sguir dheth� gus an d�-st�ladh fh�gail.

; *** Misc. errors
ErrorCreatingDir=Cha b� urrainn dhan st�ladh am pasgan "%1" a chruthachadh
ErrorTooManyFilesInDir=Tha faidhle ann nach b� urrainn dhan st�ladh cruthachadh sa phasgan �%1� on a tha cus fhaidhlichean ann

; *** Setup common messages
ExitSetupTitle=F�g an st�ladh
ExitSetupMessage=Chan eil an st�ladh coileanta fhathast. Ma sguireas tu dheth an-dr�sta, cha d�id am pr�gram a st�ladh.%n%n�S urrainn dhut an st�ladh a dh�anamh a-rithist uaireigin eile gus a choileanadh.%n%nA bheil thu airson an st�ladh fh�gail?
AboutSetupMenuItem=&Mun st�ladh �
AboutSetupTitle=Mun st�ladh
AboutSetupMessage=%1 Tionndadh %2%n%3%n%n%1 Duilleag-l�n:%n%4
AboutSetupNote=
TranslatorNote=An t-eadar-theangachadh le GunChleoc (fios@foramnagaidhlig.net)

; *** Buttons
ButtonBack=< Air ai&s
ButtonNext=Air adha&rt >
ButtonInstall=&St�laich
ButtonOK=Ceart ma-th�
ButtonCancel=Sguir dheth
ButtonYes=&Tha
ButtonYesToAll=Th&a dhan a h-uile
ButtonNo=&Chan eil
ButtonNoToAll=Cha&n eil dhan a h-uile
ButtonFinish=&Cr�ochnaich
ButtonBrowse=R�rai&ch �
ButtonWizardBrowse=&R�raich �
ButtonNewFolder=&Cruthaich pasgan �r

; *** "Select Language" dialog messages
SelectLanguageTitle=Tagh c�nan an st�laidh
SelectLanguageLabel=Tagh an c�nan a chleachdas an t-inneal-st�laidh

; *** Common wizard text
ClickNext=Briog air �Air adhart� gus leantainn air adhart no air �Sguir dheth� gus f�gail an-seo.
BeveledLabel=
BrowseDialogTitle=Lorg pasgan
BrowseDialogLabel=Tagh pasgan is briog air �Ceart ma-th�� an uairsin.
NewFolderName=Pasgan �r

; *** "Welcome" wizard page
WelcomeLabel1=F�ilte dhan draoidh st�laidh aig [name]
WelcomeLabel2=St�laichidh an draoidh seo [name/ver] air a� choimpiutair agad a-nis.%n%nBu ch�ir dhut cr�och a chur air a h-uile aplacaid eile mus lean thu air adhart leis an st�ladh.

; *** "Password" wizard page
WizardPassword=Facal-faire
PasswordLabel1=Tha an st�ladh seo d�onta le facal-faire.
PasswordLabel3=Cuir a-steach am facal-faire is briog air �Air adhart� an uairsin. Thoir an aire air litrichean m�ra is beaga.
PasswordEditLabel=&Facal-faire:
IncorrectPassword=Chan eil am facal-faire a chuir thu ann mar bu ch�ir. Am feuch thu ris a-rithist?

; *** "License Agreement" wizard page
WizardLicense=Aonta ceadachais
LicenseLabel=An leugh thu am fiosrachadh cudromach seo mus lean thu air adhart?
LicenseLabel3=Feuch an leugh thu an t-aonta ceadachais seo. Feumaidh tu gabhail ri teirmichean an aonta mus fhaod thu leantainn air adhart.
LicenseAccepted=&Gabhaidh mi ris an aonta
LicenseNotAccepted=&Di�ltaidh mi an t-aonta

; *** "Information" wizard pages
WizardInfoBefore=Fiosrachadh
InfoBeforeLabel=An leugh thu am fiosrachadh cudromach seo mus lean thu air adhart?
InfoBeforeClickLabel=Nuair a bhios tu deiseil gus leantainn air adhart, briog air �Air adhart.�
WizardInfoAfter=Fiosrachadh
InfoAfterLabel=An leugh thu am fiosrachadh cudromach seo mus lean thu air adhart?
InfoAfterClickLabel=Nuair a bhios tu deiseil gus leantainn air adhart, briog air �Air adhart.�

; *** "User Information" wizard page
WizardUserInfo=Fiosrachadh a� chleachdaiche
UserInfoDesc=An cuir thu a-steach an d�ta agad?
UserInfoName=&Ainm:
UserInfoOrg=&Buidheann:
UserInfoSerial=�ireamh &shreathach:
UserInfoNameRequired=Feumaidh tu ainm a chur a-steach.

; *** "Select Destination Location" wizard page
WizardSelectDir=Tagh am pasgan-amais
SelectDirDesc=C�ite an d�id [name] a st�ladh?
SelectDirLabel3=Th�id [name] a st�ladh sa phasgan seo.
SelectDirBrowseLabel=Briog air �Air adhart� gus leantainn air adhart. Briog air �R�raich� ma tha thu airson pasgan eile a thaghadh.
DiskSpaceMBLabel=Bidh feum air co-dhi� [mb] MB de rum s�bhalaidh saor.
CannotInstallToNetworkDrive=Cha ghabh seo st�ladh air draibh l�onraidh.
CannotInstallToUNCPath=Cha ghabh seo st�ladh air slighe UNC.
InvalidPath=Feumaidh tu slighe iomlan le litir draibh a thoirt seachad; m.e.:%n%nC:\Ball-eisimpleir%n%nno slighe UNC leis a' chruth:%n%n\\Frithealaiche\Co-roinneadh
InvalidDrive=Chan eil an draibh no an t-slighe UNC a thug thu seachad ann no chan urrainn dhuinn inntrigeadh. Feuch an tagh thu pasgan eile.
DiskSpaceWarningTitle=Chan eil rum saor gu le�r ann
DiskSpaceWarning=Cha feum air co-dhi� %1 KB de rum saor airson an st�laidh, ach chan eil ach %2 KB ri l�imh air an draibh a thagh thu.%n%nA bheil thu airson leantainn air adhart co-dhi�?
DirNameTooLong=Tha ainm a� phasgain/na slighe ro fhada.
InvalidDirName=Chan eil ainm a� phasgain dligheach.
BadDirName32=Chan fhaod na caractaran seo a bhith ann an ainm pasgain:%n%n%1
DirExistsTitle=Tha am pasgan ann mu thr�th
DirExists=Tha am pasgan:%n%n%1%n%nann mu thr�th. A bheil thu airson a st�ladh sa phasgan seo co-dhi�?
DirDoesntExistTitle=Chan eil am pasgan ann
DirDoesntExist=Chan eil am pasgan:%n%n%1%n%nann. A bheil thu airson a chruthachadh?

; *** "Select Components" wizard page
WizardSelectComponents=Tagh co-ph�irtean
SelectComponentsDesc=D� na co-ph�irtean a th�id a st�ladh?
SelectComponentsLabel2=Tagh na co-ph�irtean a tha thu airson st�ladh. Briog air �Air adhart� nuair a bhios tu ullamh.
FullInstallation=St�ladh sl�n
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=St�ladh beag
CustomInstallation=St�ladh gn�thaichte
NoUninstallWarningTitle=Tha co-ph�irtean ann
NoUninstallWarning=Mhothaich an st�ladh gun deach na co-ph�irtean seo a st�ladh air a� choimpiutair agad roimhe:%n%n%1%n%nCha d�id na co-ph�irtean seo nach do thagh thu tuilleadh a thoirt air falbh on choimpiutair agad.%n%nA bheil thu airson leantainn air adhart co-dhi�?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Th�id co-dhi� [mb] MB de rum a chleachdadh airson na thagh thu.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Saothraichean a bharrachd
SelectTasksDesc=D� na saothraichean a bharrachd a th�id a ruith?
SelectTasksLabel2=Tagh na saothraichean a bharrachd a tha thu airson ruith leis an st�ladh aig [name] is briog air �Air adhart� an uairsin.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Tagh pasgan sa chl�r-taice t�iseachaidh
SelectStartMenuFolderDesc=C�ite an cruthaich an st�ladh na ceanglaichean dhan phr�gram?
SelectStartMenuFolderLabel3=Cruthaichidh an st�ladh na ceanglaichean dhan phr�gram sa phasgan seo ann an �T�isich.�
SelectStartMenuFolderBrowseLabel=Briog air �Air adhart� gus leantainn air adhart. Briog air �R�raich� ma tha thu airson pasgan eile a thaghadh.
MustEnterGroupName=Feumaidh tu ainm pasgain a chur a-steach.
GroupNameTooLong=Tha ainm a� phasgain/na slighe ro fhada.
InvalidGroupName=Chan eil ainm a� phasgain dligheach.
BadGroupName=Chan fhaod na caractaran seo a bhith ann an ainm pasgain:%n%n%1
NoProgramGroupCheck2=&Na cruthaich pasgan sam bith ann an �T�isich.�

; *** "Ready to Install" wizard page
WizardReady=Deiseil airson an st�laidh
ReadyLabel1=Tha an draoidh st�laidh deiseil gus [name] a st�ladh air a� choimpiutair agad.
ReadyLabel2a=Briog air �St�laich� gus t�iseachadh air an st�ladh no air �Air ais� gus s�il a thoirt air na roghainnean no gus an atharrachadh.
ReadyLabel2b=Briog air �St�laich� gus t�iseachadh air an st�ladh.
ReadyMemoUserInfo=Fiosrachadh a� chleachdaiche:
ReadyMemoDir=Pasgan-amais:
ReadyMemoType=Se�rsa an st�laidh:
ReadyMemoComponents=Co-ph�irtean air an taghadh:
ReadyMemoGroup=Pasgan ann an �T�isich�:
ReadyMemoTasks=Saothraichean a bharrachd:

; *** "Preparing to Install" wizard page
WizardPreparing=Ag ullachadh an st�laidh
PreparingDesc=Tha an st�ladh aig [name] air a� choimpiutair seo �ga ullachadh.
PreviousInstallNotCompleted=Chaidh pr�gram eile a st�ladh/a dh�-st�ladh roimhe �s cha deach sin a choileanadh. Feumaidh tu an coimpiutair ath-th�iseachadh gus cr�och a chur air an st�ladh/d�-st�ladh sin.%n%nAn d�idh dhut an coimpiutair agad ath-th�iseachadh, t�isich an st�ladh a-rithist gus [name] a st�ladh.
CannotContinue=Chan urrainn dhan st�ladh leantainn air adhart. Feuch am briog thu air �Sguir dheth� gus f�gail an-seo.
ApplicationsFound=Tha na h-aplacaidean seo a� cleachdadh faidhlichean a dh�fheumas an st�ladh �rachadh. Mholamaid gun ceadaich thu gun d�in an st�ladh na h-aplacaidean sin gu f�in-obrachail.
ApplicationsFound2=Tha na h-aplacaidean seo a� cleachdadh faidhlichean a dh�fheumas an st�ladh �rachadh. Mholamaid gun ceadaich thu gun d�in an st�ladh na h-aplacaidean sin gu f�in-obrachail. Nuair a bhios an st�ladh coileanta, feuchaidh sinn ris na h-aplacaidean sin ath-th�iseachadh.
CloseApplications=&D�in na h-aplacaidean gu f�in-obrachail
DontCloseApplications=&Na d�in na h-aplacaidean
ErrorCloseApplications=Cha deach leis an st�ladh a h-uile aplacaid a dh�nadh gu f�in-obrachail. Mus lean thu air adhart, mholamaid gun d�in thu a h-uile aplacaid a chleachdas faidhlichean a dh�fheumas an st�ladh �rachadh.

; *** "Installing" wizard page
WizardInstalling=�Ga st�ladh
InstallingLabel=Fuirich ort fhad �s a tha [name] �ga st�ladh air a� choimpiutair agad.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=A� cr�ochnachadh an draoidh st�laidh aig [name]
FinishedLabelNoIcons=Tha sinn deiseil a� st�ladh [name] air a� choimpiutair agad.
FinishedLabel=Tha sinn deiseil a� st�ladh [name] air a� choimpiutair agad. �S urrainn dhut am pr�gram a th�iseachadh a-nis leis na ceanglaichean dhan phr�gram a chaidh a st�ladh.
ClickFinish=Briog air �Cr�ochnaich� gus cr�och a chur air an st�ladh.
FinishedRestartLabel=Feumaidh sinn an coimpiutair ath-th�iseachadh gus an st�ladh aig [name] a choileanadh. An d�an sinn seo dhut an-dr�sta?
FinishedRestartMessage=Feumaidh sinn an coimpiutair ath-th�iseachadh gus an st�ladh aig [name] a choileanadh.%n%nAn d�an sinn seo dhut an-dr�sta?
ShowReadmeCheck=Tha mi airson am faidhle LEUGHMI a shealltainn
YesRadio=&N�, ath-th�isichibh an coimpiutair dhomh an-dr�sta
NoRadio=&Cha d�an, ath-th�isichidh mi fh�n an coimpiutair uaireigin eile
; used for example as 'Run MyProg.exe'
RunEntryExec=Cuir %1 gu dol
; used for example as 'View Readme.txt'
RunEntryShellExec=Seall %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Tha an t-ath-chl�r a dh�th aig an st�ladh
SelectDiskLabel2=Cuir a-steach cl�r %1 is briog air �Ceart ma-th�.�%n%nMur eil na faidhlichean on chl�ir-sh�bailte seo sa phasgan a tha �ga shealltainn dhut, cuir a-steach an t-slighe cheart no briog air �R�raich.�
PathLabel=&Slighe:
FileNotInDir2=Chan eil am faidhle �%1� an-seo: �%2.� Feuch an atharraich thu am pasgan no an cuir thu a-steach cl�r-s�bailte eile.
SelectDirectoryLabel=S�nraich c�ite an d�id an t-ath-chl�r a chur a-steach.

; *** Installation phase messages
SetupAborted=Cha b� urrainn dhuinn an st�ladh a choileanadh.%n%nFeuch an c�raich thu an duilgheadas is t�isich air an st�ladh a-rithist.
EntryAbortRetryIgnore=Briog air �Ath-dh�an� gus feuchainn ris a-rithist, air �Leig seachad� gus leantainn air adhart co-dhi� no air �Sguir dheth� gus sgur dhen st�ladh.

; *** Installation status messages
StatusClosingApplications=A� d�nadh aplacaidean �
StatusCreateDirs=A� cruthachadh pasganan �
StatusExtractFiles=A� d�-dh�mhlachadh faidhlichean �
StatusCreateIcons=A� cruthachadh ceanglaichean �
StatusCreateIniEntries=A� cruthachadh innteartan INI �
StatusCreateRegistryEntries=A� cruthachadh innteartan na cl�r-lainn �
StatusRegisterFiles=A� cl�radh faidhlichean �
StatusSavingUninstall=A� s�bhaladh fiosrachadh d�-st�laidh �
StatusRunProgram=A� cr�ochnachadh an st�laidh �
StatusRestartingApplications=Ag ath-th�iseachadh nan aplacaidean �
StatusRollback=A� neo-dh�anamh nan atharraichean �

; *** Misc. errors
ErrorInternal2=Mearachd inntearnail: %1
ErrorFunctionFailedNoCode=Dh�fh�illig le %1
ErrorFunctionFailed=Dh�fh�illig le %1; c�d %2
ErrorFunctionFailedWithMessage=Dh�fh�illig le %1; c�d %2.%n%3
ErrorExecutingProgram=Cha ghabh am faidhle a ruith:%n%1

; *** Registry errors
ErrorRegOpenKey=Cha b� urrainn dhuinn iuchair na cl�r-lainn fhosgladh:%n%1\%2
ErrorRegCreateKey=Cha b� urrainn dhuinn iuchair na cl�r-lainn a chruthachadh:%n%1\%2
ErrorRegWriteKey=Mearachd le sgr�obhadh iuchair na cl�r-lainn:%n%1\%2

; *** INI errors
ErrorIniEntry=Mearachd le cruthachadh innteart INI san fhaidhle �%1.�

; *** File copying errors
FileAbortRetryIgnore=Briog air �Ath-dh�an� gus feuchainn ris a-rithist, air �Leig seachad� gus leum thairis air an fhaidhle seo (cha mholamaid seo) no air �Sguir dheth� gus sgur dhen st�ladh.
FileAbortRetryIgnore2=Briog air �Ath-dh�an� gus feuchainn ris a-rithist, air �Leig seachad� gus leantainn air adhart co-dhi� (cha mholamaid seo) no air �Sguir dheth� gus sgur dhen st�ladh.
SourceIsCorrupted=Tha am faidhle t�sail coirbte
SourceDoesntExist=Chan eil am faidhle t�sail �%1� ann
ExistingFileReadOnly=Tha d�on sgr�obhaidh air an fhaidhle a tha ann.%n%nBriog air �Ath-dh�an� gus an d�on sgr�obhaidh a thoirt air falbh, air �Leig seachad� gus leum thairis air an fhaidhle no air �Sguir dheth� gus sgur dhen st�ladh.
ErrorReadingExistingDest=Mearachd leughaidh san fhaidhle:
FileExists=Tha am faidhle seo ann mu thr�th.%n%nA bheil thu airson sgr�obhadh thairis air?
ExistingFileNewer=Tha am faidhle a tha ann mu thr�th nas �ire na am faidhle a tha thu airson st�ladh. Mholamaid gun c�m thu am faidhle a tha ann mu thr�th.%n%n A bheil thu airson am faidhle a chumail a tha ann mu thr�th?
ErrorChangingAttr=Thachair mearachd le atharrachadh nan gleusan aig an fhaidhle a tha ann mu thr�th:
ErrorCreatingTemp=Thachair mearachd a� feuchainn ri faidhle a chruthachadh sa phasgan-amais:
ErrorReadingSource=Thachair mearachd a� feuchainn ris am faidhle t�sail a leughadh:
ErrorCopying=Thachair mearachd a� feuchainn ri lethbhreac a dh�anamh de dh�fhaidhle:
ErrorReplacingExistingFile=Thachair mearachd le feuchainn ri cur an �ite an fhaidhle a tha ann:
ErrorRestartReplace=Dh�fh�illig le ath-th�iseachadh/cur �na �ite:
ErrorRenamingTemp=Thachair mearachd a� feuchainn ri ainm �r a thoirt air faidhle sa phasgan-amais:
ErrorRegisterServer=Cha ghabh an DLL/OCX a chl�radh: %1
ErrorRegSvr32Failed=Dh�fh�illig RegSvr32 le c�d f�gail %1
ErrorRegisterTypeLib=Cha ghabh leabharlann nan se�rsa a chl�radh: %1

; *** Post-installation errors
ErrorOpeningReadme=Mearachd le fosgladh an fhaidhle LEUGHMI.
ErrorRestartingComputer=Cha deach leis an st�ladh an coimpiutair agad ath-th�iseachadh. An d�an thu an t-ath-th�iseachadh a l�imh?

; *** Uninstaller messages
UninstallNotFound=Chan eil am faidhle �%1� ann. Dh�fh�illig le d�-st�ladh na h-aplacaid.
UninstallOpenError=Cha b� urrainn dhuinn am faidhle �%1� fhosgladh. Dh�fh�illig le d�-st�ladh na h-aplacaid
UninstallUnsupportedVer=Cha b� urrainn dhuinn mothachadh do fh�rmat an fhaidhle d�-st�laidh �%1.� Dh�fh�illig le d�-st�ladh na h-aplacaid
UninstallUnknownEntry=Tha innteart neo-aithnichte (%1) san loga d�-st�laidh
ConfirmUninstall=A bheil thu cinnteach bu bheil thu airson %1 is a h-uile co-ph�irt aige a thoirt air falbh?
UninstallOnlyOnWin64=Chan urrainn dhuinn an st�ladh seo a thoirt air falbh ach fo thionndaidhean Windows 64-biod.
OnlyAdminCanUninstall=Chan fhaod ach cleachdaiche le pribhleidean rianaire an st�ladh seo a thoirt air falbh.
UninstallStatusLabel=Fuirich ort fhad �s a tha %1 �ga dh�-st�ladh on choimpiutair agad.
UninstalledAll=Chaidh %1 a thoirt air falbh on choimpiutair agad gu soirbheachail.
UninstalledMost=Tha an d�-st�ladh aig %1 deiseil.%n%nTha co-ph�irtean ann nach b� urrainn dhuinn toirt air falbh. �S urrainn dhut fh�in an sguabadh �s a l�imh.
UninstalledAndNeedsRestart=Feumaidh sinn an coimpiutair agad ath-th�iseachadh gus an d�-st�ladh aig %1 a choileanadh.%n%nAn d�an sinn seo dhut an-dr�sta?
UninstallDataCorrupted=Tha am faidhle �%1� coirbte. Dh�fh�illig le d�-st�ladh na h-aplacaid.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=A bheil thu airson am faidhle co-roinnte a sguabadh �s?
ConfirmDeleteSharedFile2=Tha an siostam ag innse nach d�id am faidhle co-roinnte seo a chleachdadh le pr�gram sam bith eile. A bheil thu airson �s gun sguab sinn �s dha?%nMa tha pr�graman eile ann fhathast a chleachdas am faidhle seo is ma th�id a thoirt air falbh, dh�fhaoidte nach obraich na pr�graman ud mar bu ch�ir tuilleadh. Ma tha thu m�-chinnteach, tagh �Chan eil� gus am faidhle fh�gail san t-siostam. Cha d�an e cron dhan t-siostam agad ma chumas tu am faidhle seo air.
SharedFileNameLabel=Ainm an fhaidhle:
SharedFileLocationLabel=Pasgan:
WizardUninstalling=Staid an d�-st�laidh
StatusUninstalling=A� d�-st�ladh %1 ...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=A� st�ladh %1.
ShutdownBlockReasonUninstallingApp=A� d�-st�ladh %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 tionndadh %2
AdditionalIcons=�omhaigheagan a bharrachd:
CreateDesktopIcon=Cruthaich �omhaigheag air an &deasg
CreateQuickLaunchIcon=Cruthaich �omhaigheag &grad-th�iseachaidh
ProgramOnTheWeb=%1 air an eadar-l�on
UninstallProgram=D�-st�laich %1
LaunchProgram=T�isich %1
AssocFileExtension=&Cl�raich %1 leis an leudachan fhaidhle %2
AssocingFileExtension=A� cl�radh %1 leis an leudachan fhaidhle %2 ...
AutoStartProgramGroupDescription=T�iseachadh:
AutoStartProgram=T�isich %1 gu f�in-obrachail
AddonHostProgramNotFound=Cha deach %1 a lorg sa phasgan a thagh thu.%n%nA bheil thu airson leantainn air adhart co-dhi�?
