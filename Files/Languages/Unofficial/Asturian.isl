; Translation made with Translator 1.32 (http://www2.arnes.si/~sopjsimo/translator.html)
; $Translator:NL=%n:TB=%t
;
; *** Inno Setup version 4.0.x Asturian messages ***
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/is3rdparty.php
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; $Id: asturian-4.0.6.isl,v 0.1 2003/09/17 22:14:19 jl Exp $

[LangOptions]
LanguageName=Asturianu
LanguageID=$0409
; Si el lenguaje al cual está traduciendo requiere un tipo de letra o
; tamaño, quite el comentario de alguna de las siguientes entradas y cámbielas según el caso.
;DialogFontName=MS Shell Dlg
;DialogFontSize=8
;DialogFontStandardHeight=13
;TitleFontName=Arial
;TitleFontSize=29
;WelcomeFontName=Verdana
;WelcomeFontSize=12
;CopyrightFontName=Arial
;CopyrightFontSize=8

CopyrightFontName=Arial
CopyrightFontSize=8
[Messages]
; *** Application titles
SetupAppTitle=Instalar
SetupWindowTitle=Instalar - %1
UninstallAppTitle=Desinstalar
UninstallAppFullTitle=Desinstalar - %1

; *** Misc. common
InformationTitle=Información
ConfirmTitle=Confirmar
ErrorTitle=Fallu

; *** SetupLdr messages
SetupLdrStartupMessage=Instalaráse %1. ¿Quier seguir?
LdrCannotCreateTemp=Nun pudo crease l'archivu temporal. Instalación parada
LdrCannotExecTemp=Nun s'executó l'archivu nel directoriu temporal. Instalación parada

; *** Startup error messages
LastErrorMessage=%1.%n%nFallu %2: %3
SetupFileMissing=Nun s'alcuentra l'archivu %1 de la carpeta d'instalación. Por favor, corrixa'l problema o garre otra copia del programa.
SetupFileCorrupt=Los archivos d'instalación tan frayaos. Por favor, garre otra copia del programa.
SetupFileCorruptOrWrongVer=Los archivos d'instalación tan frayaos, o son incompatibles cola su versión d'instalación. Por favor, arregle'l fallu o garre otra copia del programa.
NotOnThisPlatform=Esti programa nun va furrular en %1.
OnlyOnThisPlatform=Esti programa tien que executase en %1.
WinVersionTooLowError=Esti programa necesita %1 versión %2 o superior.
WinVersionTooHighError=Esti programa nun furrula en %1 versión %2 o superior.
AdminPrivilegesRequired=Tien qu'entamar la sesión comu alministraor cuando instale esti programa.
PowerUserPrivilegesRequired=Tien que tar conectau comu Alministrador o ser miembru del grupu d'Usuarios Avanzaos pa instalar esti programa.
SetupAppRunningError=La instalación detectó que %1 ta executandose agora.%n%nPor favor, ciérrelu, llueu calque Bien pa seguir, o Cancelar para colar.
UninstallAppRunningError=La desinstalación detectó que %1 ta executandose agora.%n%nPor favor, ciérrelu, llueu calque Bien pa seguir, o Cancelar para colar.

; *** Misc. errors
ErrorCreatingDir=Nun se pue facer la carpeta "%1"
ErrorTooManyFilesInDir=Nun se puede facer un archivu ena carpeta "%1" porque tien demasiaos archivos.

; *** Setup common messages
ExitSetupTitle=Zarrar la Instalación
ExitSetupMessage=La instalación nun ta completa. Si la cierra agora, el programa nun s'instalará.%n%nPuede executar el programa d'instalación otra vegá pa completala.%n%n¿Zarrar la Instalación?
AboutSetupMenuItem=&Acerca d'Instalar...
AboutSetupTitle=Acerca d'Instalar
AboutSetupMessage=%1 versión %2%n%3%n%n%1 páxina web:%n%4
AboutSetupNote=

; *** Buttons
ButtonBack=< &Tornar
ButtonNext=&Seguir >
ButtonInstall=&Instalar
ButtonOK=Bien
ButtonCancel=Cancelar
ButtonYes=&Sí
ButtonYesToAll=Sí a &Tou
ButtonNo=&Non
ButtonNoToAll=N&on a Tou
ButtonFinish=&Finar
ButtonBrowse=&Examinar...

; *** "Select Language" dialog messages
SelectLanguageTitle=Escueya l'idioma del instalaor
SelectLanguageLabel=Escueya l'idioma pala instalación:

; *** Common wizard text
ClickNext=Calque Seguir pa continuar, Cancelar pa colar.
BeveledLabel=

; *** "Welcome" wizard page
WelcomeLabel1=Bienveniu al programa d'instalación de [name].
WelcomeLabel2=Esti programa instalará [name/ver] nel su sistema.%n%nRecomiendase enantes de seguir, zarrar toles demás aplicaciones que ten furrulando pa evitar conflictos durante la instalación.

; *** "Password" wizard page
WizardPassword=Contraseña
PasswordLabel1=Esta instalación ta protexida.
PasswordLabel3=Por favor enxerte la su contraseña, calque'n Seguir para continuar. Les contraseñes diferencien les mayúscules de les minúscules.
PasswordEditLabel=&Contraseña:
IncorrectPassword=La contraseña nun val. Por favor, inténtelu otra vegá.

; *** "License Agreement" wizard page
WizardLicense=Contratu de Llicencia
LicenseLabel=Por favor, llea la siguiente información enantes de seguir.
LicenseLabel3=Por favor, llea detenidamente el siguiente contratu de llicencia. Tien que aceptar los términos d'esti contratu enantes de seguir cola instalación.
LicenseAccepted=A&ceptu'l contratu
LicenseNotAccepted=&Nun aceptu'll contratu

; *** "Information" wizard pages
WizardInfoBefore=Información
InfoBeforeLabel=Por favor, llea la siguiente información enantes de seguir.
InfoBeforeClickLabel=Cuandu te llistu pa continuar cola instalación, calque Seguir.
WizardInfoAfter=Información
InfoAfterLabel=Por favor, llea la siguiente información enantes de seguir.
InfoAfterClickLabel=Cuando te llistu pa continuar, calque Seguir.

; *** "User Information" wizard page
WizardUserInfo=Información d'usuariu
UserInfoDesc=Por favor, introduzca la su información.
UserInfoName=Nome d'&Usuariu:
UserInfoOrg=&Organización/Empresa:
UserInfoSerial=Númberu de &Serie:
UserInfoNameRequired=Tien qu'enxertar un nome.

; *** "Select Destination Directory" wizard page
WizardSelectDir=Seleción de la Carpeta Destinu
SelectDirDesc=¿Au tien qu'instalase [name]?
SelectDirLabel=Escueya la carpeta ena que quier instalar [name] y calque Seguir.
DiskSpaceMBLabel=Necesita polo menos [mb] MB d'espaciu llibre en discu.
ToUNCPathname=Nun puede instalase nun directoriu UNC. Si quier instalar en rede, tien que mapear una unidá de la rede.
InvalidPath=Tien qu'enxertar una ruta completa cola lletra d'unidad; por exemplu:%n%nC:\APP%n%no una ruta UNC de la siguiente mena:%n%n\\servior\compartiu
InvalidDrive=La unidá o ruta UNC que elixió nun existe o nun ta accesible. Por favor, escueya otra.
DiskSpaceWarningTitle=Nun hay espaciu suficiente'n discu
DiskSpaceWarning=Necesita polo menos %1 KB de espaciu llibre pala instalación, pero la unidá que elixió namás tiene %2 KB llibres.%n%n¿Quier seguir?
BadDirName32=El nome de la carpeta nun puede incluir dalgun de los siguientes carauteres:%n%n%1
DirExistsTitle=La Carpeta ya Existe
DirExists=La carpeta:%n%n%1%n%nya existe. ¿Quier instalar n'esta carpeta entá?
DirDoesntExistTitle=La Carpeta Nun Existe
DirDoesntExist=La carpeta:%n%n%1%n%n nun existe. ¿Quier facela?

; *** "Select Components" wizard page
WizardSelectComponents=Seleción de Componentes
SelectComponentsDesc=¿Qué componentes tienen qu'instalase?
SelectComponentsLabel2=Escueya los componentes a instalar; desmarque los componentes que nun quier instalar. Calque Seguir cuandu tea llistu.
FullInstallation=Instalación Completa
; Si es posible no traduzca 'Compacta' a 'Minima' (Me refiero a 'Minima' en su lenguaje)
CompactInstallation=Instalación Compacta
CustomInstallation=Instalación Personalizada
NoUninstallWarningTitle=Componentes Existentes
NoUninstallWarning=La Instalación detectó que los siguientes componentes tan instalaos ena computaora:%n%n%1%n%nDesmarcando estos componentes, nun s'instalarán.%n%n¿Desea continuar de todos modos?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=La selección fecha requier polo menos [mb] MB d'espaciu llibre'n discu.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Selección de Tarees Adicionales
SelectTasksDesc=¿Qué tarees adicionales tienen que realizase?
SelectTasksLabel2=Seleccione les tarees adicionales que quiera facer durante la instalación de [name] y calque Seguir.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Selección de la carpeta del Menú d'entamu
SelectStartMenuFolderDesc=¿Au tiene que pones los iconos de programa?
SelectStartMenuFolderLabel=Escueya la carpeta del Menú d'entamu au quier que'l programa de instalación faiga los iconos de programa y calque Seguir.
NoIconsCheck=&Nun facer nengún iconu
MustEnterGroupName=Tien qu'enxertar un nome de carpeta.
BadGroupName=El nome de la carpeta nun puede tener nengún de los siguientes caracteres:%n%n%1
NoProgramGroupCheck2=&Nun facer grupu nel Menú d'entamu

; *** "Ready to Install" wizard page
WizardReady=Llistu pa Instalar
ReadyLabel1=Agora el programa ta llistu pa entamar la instalación de [name].
ReadyLabel2a=Calque Instalar pa seguir cola instalación, o Tornar si quier revisar o camudar dalguna configuración.
ReadyLabel2b=Calque Instalar para seguir cola instalación.
ReadyMemoUserInfo=Información d'usuariu:
ReadyMemoDir=Carpeta de Destinu:
ReadyMemoType=Tipu d'Instalación:
ReadyMemoComponents=Componentes Seleccionaos:
ReadyMemoGroup=Carpeta del Menú d'entamu:
ReadyMemoTasks=Tarees Adicionales:

; *** "Preparing to Install" wizard page
WizardPreparing=Preparándose pa Instalar
PreparingDesc=El programa ta preparandose pa instalar [name].
PreviousInstallNotCompleted=La instalación/desinstalación previa d'otru programa nun finó. Tien que reiniciar l'equipu pa finar la instalación.%n%nEncuantes reinicie la computaora, execute'l programa de nueu pa completar la instalación de [name].
CannotContinue=El programa nun puede continuar. Por favor, calque Cancelar pa colar.

; *** "Installing" wizard page
WizardInstalling=Instalando
InstallingLabel=Por favor, espere mientres s'instala [name] en su computaora.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Finando la instalación de [name]
FinishedLabelNoIcons=El programa finó la instalación de [name].
FinishedLabel=El programa finó la instalación de [name]. Puede executar la aplicación calcando'l iconu instalau.
ClickFinish=Calque Finar pa zarrar la Instalación.
FinishedRestartLabel=Pa finar la instalación de [name], tien que reiniciar la su computaora. ¿Quier reiniciar agora?
FinishedRestartMessage=Pa finar la instalación de [name], tien que reiniciar la su computaora.%n%n¿Quier reiniciar agora?
ShowReadmeCheck=Sí, quiero ver l'archivu LLÉAME.
YesRadio=&Sí, quiero reiniciar la computaora agora
NoRadio=&Non, yo reiniciaré la computaora más alantre
; used for example as 'Run MyProg.exe'
RunEntryExec=Executar %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Ver %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=La Instalación Necesita el Siguiente Discu
SelectDirectory=Escueya Carpeta
SelectDiskLabel2=Por favor, enxerte'l Discu %1 y calque Bien.%n%nSi los archivos pueden alcontrase nuna carpeta diferente a la d'abaxo, plumie la ruta correuta o calque Examinar.
PathLabel=&Ruta:
FileNotInDir2=L'archivu "%1" nun puede localizase en "%2". Por favor, enxerte'l discu correutu o escueya otra carpeta.
SelectDirectoryLabel=Por favor, especifique la llocalización del siguiente discu.

; *** Installation phase messages
SetupAborted=La instalación nun pudo completase.%n%nPor favor, arregle'l fallu y execute Instalar otra vegá.
EntryAbortRetryIgnore=Calque Reintentar pa intentalu de nueu, Omitir para continuar y que seya lo que dios quiera, o Anular pa finar la instalación.

; *** Installation status messages
StatusCreateDirs=Faciendo carpetas...
StatusExtractFiles=Copiando archivos...
StatusCreateIcons=Faciendo iconos del programa...
StatusCreateIniEntries=Faciendo entraes nel INI...
StatusCreateRegistryEntries=Faciendo entraes de rexistru...
StatusRegisterFiles=Rexistrando archivos...
StatusSavingUninstall=Guardando información pa desinstalar...
StatusRunProgram=Finando la instalación...
StatusRollback=Desfaciendo cambios...

; *** Misc. errors
ErrorInternal2=Fallu Internu: %1
ErrorFunctionFailedNoCode=%1 falló
ErrorFunctionFailed=%1 falló; códigu %2
ErrorFunctionFailedWithMessage=%1 falló; códigu %2.%n%3
ErrorExecutingProgram=Nun puede executase l'archivu:%n%1

; *** Registry errors
ErrorRegOpenKey=Fallu abriendo clave de rexistru:%n%1\%2
ErrorRegCreateKey=Fallu faciendo clave de rexistru:%n%1\%2
ErrorRegWriteKey=Fallu plumiando nuna clave de rexistru:%n%1\%2

; *** INI errors
ErrorIniEntry=Fallu faciendo entrada nel archivu INI "%1".
; *** File copying errors
FileAbortRetryIgnore=Calque Reintentar pa intentalu otra vegá, Omitir pa omitir esti archivu (nun recomendau), o Anular pa cancelar la instalación.
FileAbortRetryIgnore2=Calque Reintentar pa intentalu otra vegá, Ignorar pa seguir de cualisquier mena (nun recomendau), o Anular pa finar la instalación.
SourceIsCorrupted=L'archivu d'orixen ta dañau
SourceDoesntExist=L'archivu d'orixen "%1" nun existe
ExistingFileReadOnly=L'archivu existente ta marcau comu namas-llectura.%n%nCalque Reintentar pa quitar l'atributu namas-llectura y intentalu otra vegá, Ignorar pa omitir esti archivu, o Anular pa finar la instalación.
ErrorReadingExistingDest=Hebo un fallu al lleer l'archivu existente:
FileExists=L'archivu ya existe.%n%n¿Quier machacalu?
ExistingFileNewer=L'archivu existente ye más modernu que'l que ta instalando. Sería meyor dexar l'archivu existente.%n%n¿Quier dexar l'archivu existente?
ErrorChangingAttr=Hebo un fallu al camudar los atributos del archivu:
ErrorCreatingTemp=Hebo un fallu al facer un archivu ena carpeta de destinu:
ErrorReadingSource=Hebo un fallu al lleer l'archivu d'orixen:
ErrorCopying=Hebo un fallu al copiar l'archivu:
ErrorReplacingExistingFile=Hebo un fallu al machacar l'archivu:
ErrorRestartReplace=Nun pudo machacase:
ErrorRenamingTemp=Hebo un fallu al renomar un archivu ena carpeta de destin:
ErrorRegisterServer=Nun pudo rexistrar el DLL/OCX: %1
ErrorRegisterServerMissingExport=Nun s'alcuentra DllRegisterServer export
ErrorRegisterTypeLib=Nun pudo rexistrar la llibrería de tipu: %1

; *** Post-installation errors
ErrorOpeningReadme=Hebo un fallu tratando d'abrir l'archivu LLÉAME.
ErrorRestartingComputer=El programa d'Instalación nun puede reiniciar la computaora. Por favor, fáigalo manualmente.

; *** Uninstaller messages
UninstallNotFound=L'archivu "%1" nun existe. Nun puede desinstalase.
UninstallOpenError=Archivu "%1" nun pudo abrise. Nun puede desinstalase.

; *** Uninstallation phase messages
UninstallUnsupportedVer=L'archivu de rexistru pa desinstalar "%1" ta nun formatu nun reconociu por esta versión de desinstalar. Nun puede desinstalase
UninstallUnknownEntry=Alcontrose una entrada desconocia (%1) nel rexistru pa desinstalar
ConfirmUninstall=¿Ta seguru que quier esborrar %1 y tolos sus componentes?
OnlyAdminCanUninstall=Esti programa namás pue desinstalalu un usuariu con privilexios d'alministraor.
UninstallStatusLabel=Por favor, espere demientres %1 quitase de su computaora.
UninstalledAll=%1 quitose de su computaora.
UninstalledMost=La desinstalación de %1 finó.%n%nDalgunes coses nun pudieron esborrase. Pue usté quitales manualmente.
UninstalledAndNeedsRestart=Pa finar la desinstalación de %1, la tien que reiniciar la computaora.%n%n¿Quier reiniciala agora?
UninstallDataCorrupted=L'archivu "%1" ta frayau. Nun puede desinstalase.
ConfirmDeleteSharedFileTitle=¿Quitar Archivos Compartios?
ConfirmDeleteSharedFile2=El sistema diz que esti archivu compartiu nun lo usa nengún otru programa. ¿Quier quitar esti archivu compartiu?%n%nSi otros programes usen esti archivu y lo quita, podien dexar de furrular bien. Si nun ta seguru, escueya Non. Si nun quita l'archivu nu fairá nengún dañu.
SharedFileNameLabel=Nome d'archivu:
SharedFileLocationLabel=Llocalización:
WizardUninstalling=Estau de la Desinstalación
StatusUninstalling=Desinstalando %1...
