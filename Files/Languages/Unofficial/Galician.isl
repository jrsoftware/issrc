; *** Inno Setup version 5.1.11+ Galician messages ***
;
; Translated by Jos� Antonio Cidre Bardel�s (Medulio) - (medulio@ciberirmandade.org)
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/is3rdparty.php
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Galego
LanguageID=$0456
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
SetupAppTitle=Instalaci�n
SetupWindowTitle=Instalaci�n de %1
UninstallAppTitle=Desinstalaci�n
UninstallAppFullTitle=Desinstalaci�n de %1

; *** Misc. common
InformationTitle=Informaci�n
ConfirmTitle=Confirmaci�n
ErrorTitle=Erro

; *** SetupLdr messages
SetupLdrStartupMessage=Vaise instalar %1. Queres continuar?
LdrCannotCreateTemp=Non se puido crear un arquivo temporal. Instalaci�n cancelada
LdrCannotExecTemp=Non se puido executa-lo arquivo no directorio temporal. Instalaci�n cancelada

; *** Startup error messages
LastErrorMessage=%1.%n%nErro %2: %3
SetupFileMissing=Non se atopou o arquivo %1 no directorio da instalaci�n. Por favor, corrixe o problema ou consegue unha nova copia do programa.
SetupFileCorrupt=Os arquivos de instalaci�n est�n danados. Por favor, consegue unha nova copia do programa.
SetupFileCorruptOrWrongVer=Os arquivos de instalaci�n est�n danados ou sonche incompatibles con esta versi�n do Instalador. Por favor, corrixe o problema ou consegue unha nova copia do programa.
NotOnThisPlatform=Este programa non funcionar� baixo %1.
OnlyOnThisPlatform=Este programa s� funcionar� baixo %1.
OnlyOnTheseArchitectures=Este programa s� se pode instalar en versi�ns do Windows dese�adas para as seguintes arquitecturas de procesadores:%n%n%1
MissingWOW64APIs=A versi�n do Windows que est�s a utilizar non incl�e as funcionalidades requeridas polo Instalador para efectuar unha instalaci�n de 64 bits. Para corrixir este problema, por favor, instala o Service Pack %1.
WinVersionTooLowError=Este programa require %1 versi�n %2 ou posterior.
WinVersionTooHighError=Este programa non se pode instalar baixo %1 versi�n %2 ou posterior.
AdminPrivilegesRequired=T�s que entrar no sistema coma administrador para poder instalar este programa.
PowerUserPrivilegesRequired=T�s que entrar no sistema coma administrador ou ben coma membro do grupo de usuarios con privilexios para poder instalar este programa.
SetupAppRunningError=O Instalador detectou que %1 est� execut�ndose nestes intres.%n%nPor favor, pecha o programa e logo preme Aceptar para continuar ou Cancelar para sair.
UninstallAppRunningError=O Instalador detectou que %1 est� execut�ndose nestes intres.%n%nPor favor, pecha o programa e logo preme Aceptar para continuar ou Cancelar para sair.

; *** Misc. errors
ErrorCreatingDir=O Instalador non puido crea-lo directorio "%1"
ErrorTooManyFilesInDir=Non se puido crear un arquivo no directorio "%1" xa que cont�n arquivos de m�is

; *** Setup common messages
ExitSetupTitle=Sair do Instalador
ExitSetupMessage=A instalaci�n non se completou. Se saes agora o programa non ser� instalado.%n%nPodes volver executa-lo programa de Instalaci�n noutro intre para completa-la mesma.%n%nQueres sair do Instalador?
AboutSetupMenuItem=&Verbo do Instalador...
AboutSetupTitle=Verbo do Instalador
AboutSetupMessage=%1 versi�n %2%n%3%n%nP�xina web de %1:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Volver
ButtonNext=&Seguinte >
ButtonInstall=&Instalar
ButtonOK=Aceptar
ButtonCancel=Cancelar
ButtonYes=&Si
ButtonYesToAll=Confirmar &Todo
ButtonNo=&Non
ButtonNoToAll=N&on a Todo
ButtonFinish=&Rematar
ButtonBrowse=&Explorar...
ButtonWizardBrowse=Explo&rar...
ButtonNewFolder=&Crear Novo Cartafol

; *** "Select Language" dialog messages
SelectLanguageTitle=Escolle-lo Idioma de Instalaci�n
SelectLanguageLabel=Selecciona o Idioma a usar durante a Instalaci�n:

; *** Common wizard text
ClickNext=Preme Seguinte para continuar, ou Cancelar para sair do Instalador.
BeveledLabel=
BrowseDialogTitle=Explorar Cartafol
BrowseDialogLabel=Selecciona un cartafol da seguinte lista e pulsa Aceptar.
NewFolderName=Novo Cartafol

; *** "Welcome" wizard page
WelcomeLabel1=Benvido � asistente de instalaci�n de [name]
WelcomeLabel2=Vaise instalar [name/ver] no teu ordenador.%n%n� recomendable que peches t�dalas outras aplicaci�ns que se estean a executar denantes de inicia-la Instalaci�n.

; *** "Password" wizard page
WizardPassword=Contrasinal
PasswordLabel1=Esta Instalaci�n est� protexida por contrasinal.
PasswordLabel3=Por favor, introduce o contrasinal e logo preme Seguinte para continuar. Os contrasinais son sensibles �s mai�sculas/min�sculas.
PasswordEditLabel=&Contrasinal:
IncorrectPassword=O contrasinal introducido � incorrecto. Por favor, proba outra vez.

; *** "License Agreement" wizard page
WizardLicense=Conformidade coa Licencia de Uso
LicenseLabel=Por favor, le a seguinte informaci�n de importancia antes de continuar.
LicenseLabel3=Por favor, lee a seguinte Conformidade coa Licencia de Uso. T�s que acepta-los termos deste acordo antes de continuar coa Instalaci�n.
LicenseAccepted=&Acepto os termos do acordo
LicenseNotAccepted=&Non acepto os termos do acordo

; *** "Information" wizard pages
WizardInfoBefore=Informaci�n
InfoBeforeLabel=Por favor, le a seguinte informaci�n de importancia antes de continuar.
InfoBeforeClickLabel=Cando est�s preparado para continuar coa Instalaci�n, preme Seguinte.
WizardInfoAfter=Informaci�n
InfoAfterLabel=Por favor, le a seguinte informaci�n de importancia antes de continuar.
InfoAfterClickLabel=Cando est�s preparado para continuar coa Instalaci�n, preme Seguinte.

; *** "User Information" wizard page
WizardUserInfo=Informaci�n do Usuario
UserInfoDesc=Por favor, introduce a t�a informaci�n.
UserInfoName=Nome de &Usuario:
UserInfoOrg=&Organizaci�n:
UserInfoSerial=N�mero de &Serie:
UserInfoNameRequired=T�s que introducir un nome.

; *** "Select Destination Location" wizard page
WizardSelectDir=Selecciona o Directorio de Desti�o
SelectDirDesc=Onde queres instalar [name]?
SelectDirLabel3=O Instalador vai instalar [name] no seguinte cartafol.
SelectDirBrowseLabel=Para continuar, preme Seguinte. Se queres seleccionar outro cartafol, preme Explorar.
DiskSpaceMBLabel=O programa require polo menos [mb] MB de espazo ceibe no disco.
ToUNCPathname=O Instalador non pode instala-lo programa nun nome de directorio UNC. Se est�s tentando face-la instalaci�n nunha rede, ter�s que asignar unha letra (D:, E:, F:, etc) � disco de desti�o.
InvalidPath=T�s que introducir un enderezo completo con letra da unidade de disco; por exemplo:%n%nC:\Aplicaci�n%n%nou ben unha ruta UNC do seguinte xeito:%n%n\\servidor\compartido
InvalidDrive=O disco ou enderezo UNC de rede que seleccionaches non � accesible. Por favor, selecciona outro.
DiskSpaceWarningTitle=Non hai Espazo dabondo no Disco
DiskSpaceWarning=O Instalador require polo menos %1 KB de espazo ceibe para a instalaci�n, pero o disco que escolleches s� ten %2 KB dispo�ibles.%n%nQueres continuar de t�dolos xeitos?
DirNameTooLong=O nome do cartafol ou da ruta e longo de m�is.
InvalidDirName=O nome de cartafol non � v�lido.
BadDirName32=Os nomes de directorio non poden incluir calquera dos caracteis seguintes:%n%n%1
DirExistsTitle=O Directorio Xa Existe
DirExists=O directorio:%n%n%1%n%nxa existe. Queres instala-lo programa neste directorio de t�dolos xeitos?
DirDoesntExistTitle=O Directorio Non Existe
DirDoesntExist=O directorio:%n%n%1%n%nnon existe. Queres crealo?

; *** "Select Components" wizard page
WizardSelectComponents=Elixir Compo�entes
SelectComponentsDesc=C�les dos compo�entes queres instalar?
SelectComponentsLabel2=Selecciona �nicamente os compo�entes que queres instalar. Preme seguinte para continuar.
FullInstallation=Instalaci�n Completa
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalaci�n Compacta
CustomInstallation=Instalaci�n Persoalizada
NoUninstallWarningTitle=Os Compo�entes Xa Existen
NoUninstallWarning=O Instalador detectou que os seguintes compo�entes xa est�n instalados no teu ordenador:%n%n%1%n%nDeseleciona-los compo�entes non os vai desinstalar.%n%nQueres continuar de t�dolos xeitos?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=A selecci�n que fixeches require polo menos [mb] MB de espazo ceibe no disco.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Seleccionar Procesos Adicionais
SelectTasksDesc=Que procesos adicionais deben efectuarse?
SelectTasksLabel2=Selecciona os procesos adicionais que queres que o Instalador leve a cabo durante a instalaci�n de [name], logo preme Seguinte.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Seleccionar Cartafol do Men� de Inicio
SelectStartMenuFolderDesc=Onde queres que o Instalador xere as ligaz�ns do programa?
SelectStartMenuFolderLabel3=O Instalador vai crea-las ligaz�ns �s programas no seguinte cartafol do Men� de Inicio.
SelectStartMenuFolderBrowseLabel=Para continuar, preme Seguinte. Se queres seleccionar outro cartafol, preme Explorar.
MustEnterGroupName=T�s que introducir un nome de cartafol.
GroupNameTooLong=O nome do cartafol ou da ruta e longo de m�is.
InvalidGroupName=O nome de cartafol non � v�lido.
BadGroupName=O nome de cartafol non pode incluir ning�n dos seguintes caracteis:%n%n%1
NoProgramGroupCheck2=&Non crear cartafol no Men� de Inicio

; *** "Ready to Install" wizard page
WizardReady=Preparado para Instalar
ReadyLabel1=O Instalador est� preparado para comezar instalar [name] no teu ordenador.
ReadyLabel2a=Preme Instalar para continuar coa instalaci�n ou preme Volver se queres revisar ou troca-las opci�ns escollidas.
ReadyLabel2b=Preme Instalar para continuar coa instalaci�n.
ReadyMemoUserInfo=Informaci�n de Usuario:
ReadyMemoDir=Directorio de desti�o:
ReadyMemoType=Tipo de Instalaci�n:
ReadyMemoComponents=Compe�entes seleccionados:
ReadyMemoGroup=Cartafol do Men� Inicio:
ReadyMemoTasks=Procesos adicionais:

; *** "Preparing to Install" wizard page
WizardPreparing=Preparando a Instalaci�n
PreparingDesc=O Instalador est� a prepara-la instalaci�n de [name] no teu ordenador.
PreviousInstallNotCompleted=A instalaci�n/desinstalaci�n dun programa anterior non foi completada. Ter�s que reinicia-lo teu ordenador para completar este instalaci�n.%n%nLogo de ter reiniciado o teu ordenador, executa o Instalador outra vez para completa-la instalaci�n de [name].
CannotContinue=O Instalador non pode continuar. Por favor, preme Cancelar para sair.

; *** "Installing" wizard page
WizardInstalling=Instalando
InstallingLabel=Por favor, agarda mentres o Instalador instala [name] no teu ordenador.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Rematando o Asistente de Instalaci�n de [name]
FinishedLabelNoIcons=O Instalador rematou de instalar [name] no teu ordenador.
FinishedLabel=O Instalador rematou de instalar [name] no teu ordenador. Podes executa-la aplicaci�n seleccionado as iconas instaladas.
ClickFinish=Preme Rematar para sair do Instalador.
FinishedRestartLabel=Para completa-la instalaci�n de [name] o Instalador t�n que reinicia-lo teu ordenador. Queres reinicialo agora?
FinishedRestartMessage=Para completa-la instalaci�n de [name] o Instalador t�n que reinicia-lo teu ordenador. %n%nQueres reinicialo agora?
ShowReadmeCheck=Si, quero ve-lo arquivo LEME agora
YesRadio=&Si, quero reinicia-lo ordenador agora
NoRadio=&Non, reiniciarei o ordenador logo
; used for example as 'Run MyProg.exe'
RunEntryExec=Executar %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Ver %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=O Instalador Precisa o Disco Seguinte
SelectDiskLabel2=Por favor, inserta o Disco %1 e preme Aceptar.%n%nSe os arquivos deste disco se poden atopar noutro cartafol diferente � amosado a continuaci�n, introduce a ruta correcta ou preme Explorar.
PathLabel=&Ruta:
FileNotInDir2=Non se puido atopa-lo arquivo "%1" en "%2". Por favor, inserta o disco correcto ou escolle outro cartafol.
SelectDirectoryLabel=Por favor, indica onde se atopa o disco seguinte.

; *** Installation phase messages
SetupAborted=A Instalaci�n non foi completada.%n%nPor favor, corrixe o problema e executa o Instalador de novo.
EntryAbortRetryIgnore=Preme Reintentar para volta-lo tentar, Ignorar para continuar de t�dolos xeitos, ou Abortar para cancela-la instalaci�n.

; *** Installation status messages
StatusCreateDirs=Creando directorios...
StatusExtractFiles=Extraendo arquivos...
StatusCreateIcons=Creando iconas do programa...
StatusCreateIniEntries=Creando entradas de arquivo INI...
StatusCreateRegistryEntries=Creando entradas de rexistro...
StatusRegisterFiles=Rexistrando arquivos...
StatusSavingUninstall=Gardando informaci�n de desinstalaci�n...
StatusRunProgram=Rematando instalaci�n...
StatusRollback=Desfacendo trocos...

; *** Misc. errors
ErrorInternal2=Erro Interno: %1
ErrorFunctionFailedNoCode=%1 fallou
ErrorFunctionFailed=%1 fallou; c�digo %2
ErrorFunctionFailedWithMessage=%1 fallou; c�digo %2.%n%3
ErrorExecutingProgram=Non se pode executa-lo arquivo:%n%1

; *** Registry errors
ErrorRegOpenKey=Erro � abri-la chave de rexistro:%n%1\%2
ErrorRegCreateKey=Erro � crea-la chave de rexistro:%n%1\%2
ErrorRegWriteKey=Erro � escribi-la chave de rexistro:%n%1\%2

; *** INI errors
ErrorIniEntry=Erro � crea-la entrada INI no arquivo "%1".

; *** File copying errors
FileAbortRetryIgnore=Preme Reintentar para tentalo de novo, Ignorar para saltar este arquivo (non recomendado), ou Abortar para cancela-la instalaci�n.
FileAbortRetryIgnore2=Preme Reintentar para tentalo de novo, Ignorar para continuar de t�dolos xeitos (non recomendado), ou Abortar para cancela-la instalaci�n.
SourceIsCorrupted=O arquivo de orixe est� corrupto
SourceDoesntExist=O arquivo de orixe "%1" non existe
ExistingFileReadOnly=O arquivo existente � s� de lectura.%n%nPreme Reintentar para tira-lo atributo de s� lectura e tentalo de novo, Ignorar para saltar este arquivo, ou Abortar para cancela-la instalaci�n.
ErrorReadingExistingDest=Houbo un erro � tentar le-lo arquivo:
FileExists=O arquivo xa existe.%n%nQueres sobreescribilo?
ExistingFileNewer=O arquivo existente � m�is novo que o que est�s tentando instalar. Recom�ndase que conserves o arquivo existente.%n%nQueres mante-lo arquivo existente?
ErrorChangingAttr=Houbo un erro o tentar de muda-los atributos do arquivo:
ErrorCreatingTemp=Houbo un erro o tentar crea-lo arquivo no directorio de desti�o:
ErrorReadingSource=Houbo un erro o tentar le-lo arquivo de orixe:
ErrorCopying=Houbo un erro o tentar copia-lo arquivo:
ErrorReplacingExistingFile=Houbo un erro o tentar de reemplaza-lo arquivo:
ErrorRestartReplace=Reemplazar fallou:
ErrorRenamingTemp=Houbo un erro o tentar renomea-lo arquivo no directorio de desti�o:
ErrorRegisterServer=Non se puido rexistra-lo DLL/OCX: %1
ErrorRegSvr32Failed=O RegSvr32 fallou indicando este c�digo de sa�da: %1
ErrorRegisterTypeLib=Non se puido rexistra-la librar�a de tipo: %1

; *** Post-installation errors
ErrorOpeningReadme=Houbo un erro o tentar abri-lo arquivo LEME.
ErrorRestartingComputer=O Instalador non puido reinicia-lo ordenador. Por favor, faino manualmente.

; *** Uninstaller messages
UninstallNotFound=O arquivo "%1" non existe. Non se pode desinstalar.
UninstallOpenError=O arquivo "%1" non se puido abrir. Non se pode desinstalar
UninstallUnsupportedVer=O arquivo de desinstalaci�n "%1" est� nun formato non reco�ecible por esta versi�n no desinstalador. Non se pode desinstalar
UninstallUnknownEntry=Atopouse unha entrada desco�ecida (%1) no arquivo de desinstalaci�n
ConfirmUninstall=Seguro que queres desinstalar completamente %1 e t�dolos seus compo�entes?
UninstallOnlyOnWin64=Esta instalaci�n s� pode desinstalarse nun Windows de 64 bits.
OnlyAdminCanUninstall=Esta instalaci�n s� a pode desinstalar un usuario con privilexios de administrador.
UninstallStatusLabel=Por favor, agarda mentres se elimina %1 do teu ordenador.
UninstalledAll=%1 foi correctamente desinstalado do teu ordenador.
UninstalledMost=Desinstalaci�n de %1 completa.%n%nAlg�ns elementos non puideron ser eliminados. Ter�s que borralos manualmente.
UninstalledAndNeedsRestart=Para completa-la desinstalaci�n de %1, ter�s que reinicia-lo teu ordenador.%n%nQueres reinicia-lo agora?
UninstallDataCorrupted=O arquivo "%1" est� corrupto. Non se pode desinstalar

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Queres elimina-lo arquivo compartido?
ConfirmDeleteSharedFile2=O sistema indica que o seguinte arquivo compartido xa non se usa por parte de ning�n programa. Queres que o desinstalador elimine este arquivo compartido?%n%nSe alg�n programa necesita este arquivo e o eliminas, eses programas poden non funcionar correctamente. Se non est�s seguro escolle Non. Deixa-lo arquivo no teu sistema non causar� problemas.
SharedFileNameLabel=Nome do arquivo:
SharedFileLocationLabel=Localizaci�n:
WizardUninstalling=Estado da Desinstalaci�n
StatusUninstalling=Desinstalando %1...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versi�n %2
AdditionalIcons=Iconas adicionais:
CreateDesktopIcon=Crear unha icona no &escritorio
CreateQuickLaunchIcon=Crear unha icona no Inicio &R�pido
ProgramOnTheWeb=%1 na Web
UninstallProgram=Desinstalar %1
LaunchProgram=Iniciar %1
AssocFileExtension=&Asociar %1 coa extensi�n de arquivo %2
AssocingFileExtension=Asociando %1 coa extensi�n de arquivo %2...
