; *** Inno Setup version 5.1.11+ Valencian messages ***
;
;Translated by Vicente Adam (email: vjatv@yahoo.es)

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Valenci<00E0>
LanguageID=33280
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
SetupAppTitle=Instalacio
SetupWindowTitle=Instalacio - %1
UninstallAppTitle=Eliminar
UninstallAppFullTitle=Eliminar - %1

; *** Misc. common
InformationTitle=Informacio
ConfirmTitle=Confirmacio
ErrorTitle=Erro

; *** SetupLdr messages
SetupLdrStartupMessage=Este programa instalara la traduccio de %1. ¿Vols continuar?
LdrCannotCreateTemp=No s'ha pogut crear un ficher temporal. Instalacio cancelada
LdrCannotExecTemp=No s'ha pogut eixecutar l'archiu en la carpeta temporal. Instalacio cancelada

; *** Startup error messages
LastErrorMessage=%1.%n%nErro %2: %3
SetupFileMissing=L'archiu %1 no es troba en la carpeta d'instalacio. Per favor, corregir el problema i obtindre una nova copia del programa.
SetupFileCorrupt=Els archius d'instalacio estan danyats. Per favor, obtinga una nova copia del programa.
SetupFileCorruptOrWrongVer=Els archius d'instalacio estan danyats, o son incompatibles en esta versio del programa. Per favor, corregir el problema o obtindre una nova copia del programa.
NotOnThisPlatform=Este programa no funcionara en un sistema %1.
OnlyOnThisPlatform=Este programa nomes pot ser eixecutat en un sistema %1.
OnlyOnTheseArchitectures=Este programa nomes pot ser instalat en versions de Windows dissenyades per a les següents arquitectures de processador:%n%n%1
MissingWOW64APIs=Esta versio de Windows no conte la funcionalitat necessaria per a realisar una instalacio de 64 bits. Per tal de corregir este problema, per favor, instaleu el Service Pack %1.
WinVersionTooLowError=Este programa requerix %1 versio %2 o posterior.
WinVersionTooHighError=Este programa no pot ser instalat en %1 versio %2 o posterior.
AdminPrivilegesRequired=Deu de tingre privilegis d'administrador per poder instalar este programa.
PowerUserPrivilegesRequired=Deu ser l'administrador o equivalent per a poder instalar este programa.
SetupAppRunningError=El programa d'instalacio ha detectat que %1 s'esta eixecutant actualment.%n%nPer favor, tanqueu el programa i apreteu Acceptar per a continuar o Cancelar per a eixir.
UninstallAppRunningError=El programa de desinstalacio ha detectat que %1 s'està eixecutant en este moment.%n%nPer favor, tanqueu el programa i apreteu Acceptar per a continuar o Cancelar per a eixir.

; *** Misc. errors
ErrorCreatingDir=El programa d'instalacio no ha pogut crear la carpeta "%1"
ErrorTooManyFilesInDir=No s'ha pogut crear un archiu en la carpeta "%1" perque conte massa archius

; *** Setup common messages
ExitSetupTitle=Eixir
ExitSetupMessage=L'instalacio no s'ha completat. Si eixiu ara, el programa no sera instalat.%n%nPer a completar-la podreu tornar a eixecutar el programa d'instalacio quan vullgau.%n%n¿Vols Eixir?
AboutSetupMenuItem=&Al voltant de l'instalacio...
AboutSetupTitle=Al voltant de l'instalacio
AboutSetupMessage=%1 versio %2%n%3%n%nPlana uep de %1:%n%4
AboutSetupNote=
TranslatorNote=Valencian translation by vjatv (vjatv@yahoo.es)

; *** Buttons
ButtonBack=< &Arrere
ButtonNext=&Següent >
ButtonInstall=&Instalar
ButtonOK=Acceptar
ButtonCancel=Cancelar
ButtonYes=&Si
ButtonYesToAll=Si a &tot
ButtonNo=&No
ButtonNoToAll=N&o a tot
ButtonFinish=&Finalisar
ButtonBrowse=&Explorar...
ButtonWizardBrowse=E&xaminar...
ButtonNewFolder=Crear &nova carpeta

; *** "Select Language" dialog messages
SelectLanguageTitle=Triar l'idioma de l'instalacio
SelectLanguageLabel=Triar l'idioma que desija utilisar durant l'instalacio:

; *** Common wizard text
ClickNext=Faça clic en Següent per a continuar o en Cancelar per a abandonar l'instalacio.
BeveledLabel=
BrowseDialogTitle=Buscar Carpeta
BrowseDialogLabel=Trie una carpeta de la següent llista, i despres faça clic en Acceptar.
NewFolderName=Nova Carpeta

; *** "Welcome" wizard page
WelcomeLabel1=Benvingut a l'instalacio de [name]
WelcomeLabel2=Este programa instalara [name/ver] en el seu ordenador.%n%nEs recomana que abans de continuar tanqueu tots els atres programes oberts, per tal d'evitar conflictes durant el proces d'instalacio.

; *** "Password" wizard page
WizardPassword=Contrasenya
PasswordLabel1=Esta instalacio esta protegida en una contrasenya.
PasswordLabel3=Indiqueu la contrasenya i apreteu Següent per a continuar. Esta contrasenya distinguix entre mayuscules i minuscules.
PasswordEditLabel=&Contrasenya:
IncorrectPassword=La contrasenya introduïda no es correcta. Torneu-ho a intentar.

; *** "License Agreement" wizard page
WizardLicense=Acord de Llicencia
LicenseLabel=Per favor llegiu la següent informacio abans de continuar.
LicenseLabel3=Per favor, llixca el següent Acort de Llicencia. Utilise la barra de desplaçament o aprete la tecla Avpág per a vore el resto de la llicencia.
LicenseAccepted=&Acceptar l'acort
LicenseNotAccepted=&No accepte l'acort

; *** "Information" wizard pages
WizardInfoBefore=Informacio
InfoBeforeLabel=Per favor, llixca l'informacio següent abans de continuar.
InfoBeforeClickLabel=Quan estiga preparat per a continuar, apreteu el boto Següent
WizardInfoAfter=Informacio
InfoAfterLabel=Per favor, llixca l'informacio següent abans de continuar.
InfoAfterClickLabel=Quan estiga preparat per a continuar, apreteu  el boto Següent

; *** "User Information" wizard page
WizardUserInfo=Informacio sobre l'usuari
UserInfoDesc=Per favor, proporcionar la vostra informacio.
UserInfoName=&Nom de l'usuari:
UserInfoOrg=&Organisacio
UserInfoSerial=&Numero de serie:
UserInfoNameRequired=Deu proporcionar el seu nom

; *** "Select Destination Location" wizard page
WizardSelectDir=Triar la Carpeta de Desti
SelectDirDesc=¿On s'instalara [name]?
SelectDirLabel3=El programa d'instalacio instalara [name] en la carpeta següent.
SelectDirBrowseLabel=Per a continuar, apreteu Següent. Si desigeu triar una atra capeta, apreteu Examinar.
DiskSpaceMBLabel=Este programa necessita un minim de [mb] MB d'espai en el disc.
ToUNCPathname=No se pot instalar el programa en un directori UNC. Si esta tratant d'instalar-lo en red, haureu d'assignar una lletra (D:, E:, etc...) al disc de desti.
InvalidPath=Deu proporcionar una ruta completa en la lletra d'unitat, per eixemple:%n%nC:\APP
InvalidDrive=L'unitat que ha triat no existix, per favor trieu-ne un atra.
DiskSpaceWarningTitle=No hi ha prou espai al disc
DiskSpaceWarning=El programa d'instalacio necessita com a minim %1 KB d'espai lliure, pero l'unitat triada nomes te %2 KB disponibles.%n%n¿Desija continuar de totes formes?
DirNameTooLong=El nom de la carpeta o de la ruta es massa llarga.
InvalidDirName=El nom de la carpeta no es valida.
BadDirName32=El nom de una carpeta no pot incloure cap dels caracters següents:%n%n%1
DirExistsTitle=La carpeta ya existix
DirExists=La carpeta:%n%n%1%n%nya existix. ¿Vols instalar igualment el programa en esta carpeta?
DirDoesntExistTitle=La Carpeta No existix
DirDoesntExist=La carpeta:%n%n%1%n%nno existix. ¿Vols crear la carpeta?

; *** "Select Program Group" wizard page
WizardSelectComponents=Triar Components
SelectComponentsDesc=Quins components vols instalar?
SelectComponentsLabel2=Triar els components que vols instalar; eliminar els components que no voleu instalar. Apretar Següent per a continuar.
FullInstallation=Instalacio completa

; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalacio compacta
CustomInstallation=Instalacio personalisada
NoUninstallWarningTitle=Els components Existixen
NoUninstallWarning=El programa d'instalacio ha detectat que els components següents ya es troben en el seu ordenador:%n%n%1%n%nAl desactivarlos no s'instalaran.%n%n¿Vols continuar de totes les maneres?
ComponentSize1=%1 Kb
ComponentSize2=%1 Mb
ComponentsDiskSpaceMBLabel=Esta seleccio requerix un minim de [mb] Mb d'espai en el disc.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Triar tarees adicionals
SelectTasksDesc=Quines tarees adicionals deuran d'eixecutar-se?
SelectTasksLabel2=Triar les tarees adicionals que vols que siguen eixecutades mentres s'instala [name], i despres apreteu el boto Següent.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Triar la carpeta del Menu d'Inici
SelectStartMenuFolderDesc=¿On deuran colocar-se els icons d'acces directe al programa?
SelectStartMenuFolderLabel3=El programa d'instalacio creara l'acces directe al programa en la següent carpeta del menu d'Inici.
SelectStartMenuFolderBrowseLabel=Per a continuar, apreteu Següent. Si desija triar un atra carpeta, apreteu en Examinar.
MustEnterGroupName=Deu proporcionar un nom de carpeta.
GroupNameTooLong=El nom de la carpeta o de la ruta es massa llarga.
InvalidGroupName=El nom de la carpeta no es valida.
BadGroupName=El nom de la carpeta no pot contindre cap dels caracters següents:%n%n%1
NoProgramGroupCheck2=&No crear una carpeta al Menu Inici

; *** "Ready to Install" wizard page
WizardReady=Preparat per a instalar
ReadyLabel1=El programa d'instalacio esta preparat per a iniciar l'instalacio de [name] al vostre ordenador.
ReadyLabel2a=Apretar Instalar per a continuar en l'instalacio, o Arrere si vols revisar o modificar les opcions d'instalacio.
ReadyLabel2b=Apretar Instalar per a continuar en l'instalacio.
ReadyMemoUserInfo=Informacio de l'usuari:
ReadyMemoDir=Carpeta de desti:
ReadyMemoType=Tipo d'instalacio:
ReadyMemoComponents=Components seleccionats:
ReadyMemoGroup=Carpeta del Menu Inici:
ReadyMemoTasks=Tarees adicionals:

; *** "Preparing to Install" wizard page
WizardPreparing=Preparant l'instalacio
PreparingDesc=Preparant l'instalacio de [name] al vostre ordenador.
PreviousInstallNotCompleted=L'instalacio o desinstalacio anterior no s'ha completat. Deura reiniciar l'ordenador per a finalisar l'instalacio.%n%nDespres de reiniciar l'ordenador, eixecuteu este programa de nou per completar l'instalacio de [name].
CannotContinue=L'instalacio no pot continuar. Per favor, apreteu en el boto Cancelar per a eixir.

; *** "Installing" wizard page
WizardInstalling=Instalant
InstallingLabel=Per favor, espereu mentres s'instala [name] al vostre ordenador.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Completant l'instalacio de [name]
FinishedLabelNoIcons=El programa ha finalisat l'instalacio de [name] al vostre ordenador.
FinishedLabel=El programa ha finalisat l'instalacio de [name] al vostre ordenador. L'aplicacio pot eixecutar-se triant l'icon creat.
ClickFinish=Apretar Finalisar per a eixir de l'instalacio.
FinishedRestartLabel=Per a completar l'instalacio de [name] deu reiniciar l'ordenador. ¿Vols fer-ho ara?
FinishedRestartMessage=Per a completar l'instalacio de [name] deu reiniciar l'ordenador. ¿Vols fer-ho ara?
ShowReadmeCheck=Si, desige vore l'archiu LLIXCAM.TXT
YesRadio=&Si, desige reiniciar l'ordenador ara
NoRadio=&No, reiniciare l'ordenador mes tart
; used for example as 'Run MyProg.exe'
RunEntryExec=Eixecutar %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Visualisar %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=El programa d'instalacio necessita el disc següent
SelectDiskLabel2=Per favor, introduir el disc %1 i apreteu Acceptar.%n%nSi els archius es localisen en una carpeta diferent de l'indicada baix, proporcione la ruta correcta o be apreteu en Examinar.
PathLabel=&Ruta:
FileNotInDir2=L'archiu "%1" no s'ha pogut trobar en "%2". Per favor, introduixca el disc correcte o trie un atra carpeta.
SelectDirectoryLabel=Per favor, indique on es troba el disc següent.

; *** Installation phase messages
SetupAborted=L'instalacio no s'ha completat.%n%n%Per favor, resolga el problema i eixecute de nou el programa d'instalacio.
EntryAbortRetryIgnore=Apretar sobre Reintentar per a intentar-ho de nou, Ignorar per a continuar igualment, o Abandonar per abandonar l'instalacio.

; *** Installation status messages
StatusCreateDirs=Creant carpetes...
StatusExtractFiles=Copiant archius...
StatusCreateIcons=Creant enllaços del programa...
StatusCreateIniEntries=Creant entrades INI...
StatusCreateRegistryEntries=Creant entrades de registre...
StatusRegisterFiles=Registrant archius...
StatusSavingUninstall=Guardant informacio de desinstalacio...
StatusRunProgram=Finalisant l'instalacio...
StatusRollback=Desfent els canvits...

; *** Misc. errors
ErrorInternal2=Erro intern: %1
ErrorFunctionFailedNoCode=%1 ha fallat
ErrorFunctionFailed=%1 ha fallat; codic %2
ErrorFunctionFailedWithMessage=%1 ha fallat; codic %2.%n%3
ErrorExecutingProgram=No es pot eixecutar l'archiu:%n%1

; *** Registry errors
ErrorRegOpenKey=Erro obrint la clau de registre:%n%1\%2
ErrorRegCreateKey=Erro creant la clau de registre:%n%1\%2
ErrorRegWriteKey=Erro escriguent en la clau de registre:%n%1\%2

; *** INI errors
ErrorIniEntry=Erro creant entrada en l'archiu INI "%1".

; *** File copying errors
FileAbortRetryIgnore=Apretar Reintentar per a intentar-ho de nou, Ignorar per a botar-se este archiu (no recomanat), o Cancelar per abandonar l'instalacio.
FileAbortRetryIgnore2=Apretar Reintentar per a intentar-ho de nou, Ignorar per a continuar igualment (no recomanat), o Cancelar per abandonar l'instalacio.
SourceIsCorrupted=L'archiu d'orige esta danyat
SourceDoesntExist=L'archiu d'orige "%1" no existix
ExistingFileReadOnly=L'archiu es de nomes lectura.%n%nApretar Reintentar per a llevar-li l'atribut de nomes lectura i tornar-ho a intentar, Ignorar per a botar-se'l (no recomanat), o Cancelar per a abandonar l'instalacio.
ErrorReadingExistingDest=S'ha produït un erro al tratar de llegir l'archiu:
FileExists=L'archiu ya existix.%n%n¿Vols sobreescriure-lo?
ExistingFileNewer=L'archiu existent es mes nou que el que s'intenta instalar. Es recomana mantindre l'archiu existent.%n%n¿Vols mantindre-lo?
ErrorChangingAttr=Ha hagut un erro al tratar de canviar els atributs de l'archiu:
ErrorCreatingTemp=Ha hagut un erro al tratar de crear un archiu en la carpeta de desti:
ErrorReadingSource=Ha hagut un erro al tratar de llegir l'archiu d'orige:
ErrorCopying=Ha hagut un erro al tratar de copiar l'archiu:
ErrorReplacingExistingFile=Ha hagut un erro al tratar de reemplaçar l'archiu existent:
ErrorRestartReplace=Ha fallat el reintent de reemplaçar:
ErrorRenamingTemp=Ha hagut un erro al tratar de reanomenar un archiu en la carpeta de desti:
ErrorRegisterServer=No s'ha pogut registrar el DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 fallo en el codic d'eixida %1
ErrorRegisterTypeLib=No s'ha pogut registrar la biblioteca de tipo: %1

; *** Post-installation errors
ErrorOpeningReadme=Ha hagut un erro al tratar d'obrir l'archiu LLIXGAM.TXT.
ErrorRestartingComputer=El programa d'instalacio no ha pogut reiniciar l'ordenador. Per favor, feu-ho manualment.

; *** Uninstaller messages
UninstallNotFound=L'archiu "%1" no existix. No es pot desinstalar.
UninstallOpenError=L'archiu "%1" no s'ha pogut obrir. No es pot desinstalar.
UninstallUnsupportedVer=L'archiu de desinstalacio "%1" esta en un format no reconegut per esta versio del desinstalador. No es pot desinstalar
UninstallUnknownEntry=S'ha trobat una entrada desconeguda (%1) en l'archiu de desinstalacio.
ConfirmUninstall=¿Esta segur de voler eliminar completament %1 i tots els seus components?
UninstallOnlyOnWin64=Este programa nomes pot ser desinstalat en Windows de 64 bits.
OnlyAdminCanUninstall=Este programa nomes pot ser desinstalat per un usuari en privilegis d'administrador.
UninstallStatusLabel=Per favor, espereu mentres s'elimina %1 del vostre ordenador.
UninstalledAll=%1 s'ha desinstalat correctament del vostre ordenador.
UninstalledMost=Desinstalacio de %1 completada.%n%nAlguns elements no s'han pogut eliminar. Poden ser eliminats manualment.
UninstalledAndNeedsRestart=Per completar l'instalacio de %1, deu reiniciar el vostre ordenador.%n%n¿Vols fer-ho ara?
UninstallDataCorrupted=L'archiu "%1" esta danyat. No es pot desinstalar.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=¿Eliminar Archius compartits?
ConfirmDeleteSharedFile2=El sistema indica que l'archiu compartit següent ya no es utilisat per atres programes. ¿Vols que la desinstalacio elimine este archiu?%n%nSi atres programes encara estan utilisant-lo i es eliminat, podria no funcionar correctament. Si no n'esteu segur, trieu No. Deixar l'archiu al sistema no fara cap mal.
SharedFileNameLabel=Nom de l'archiu:
SharedFileLocationLabel=Ubicacio:
WizardUninstalling=Estat de la desinstalacio
StatusUninstalling=Desinstalant %1...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versio %2
AdditionalIcons=Icons adicionals:
CreateDesktopIcon=Crear un icon en l'&Escritori
CreateQuickLaunchIcon=Crear un icon en la &Barra de tarees
ProgramOnTheWeb=%1 a Internet
UninstallProgram=Desinstalar %1
LaunchProgram=Obrir %1
AssocFileExtension=&Associar %1 en l'extensio d'archiu %2
AssocingFileExtension=Associant %1 en l'extensio d'archiu %2...
