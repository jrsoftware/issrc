; *** Inno Setup version 6.5.0+ Galician messages ***

; Maintained by Alexandre Espinosa Menor (aemenor@gmail.com)
; Galician.isl version 1.0.0
; based on Spanish.isl version 1.7.1 (20250625)
;
[LangOptions]
; The following three entries are very important. Be sure to read and
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Galego
LanguageID=$0456
; LanguageCodePage should always be set if possible, even if this file is Unicode
; For English it's set to zero anyway because English only uses ASCII characters
LanguageCodePage=1252
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
SetupAppTitle=Instalar
SetupWindowTitle=Instalar - %1
UninstallAppTitle=Desinstalar
UninstallAppFullTitle=Desinstalar - %1

; *** Misc. common
InformationTitle=Información
ConfirmTitle=Confirmar
ErrorTitle=Erro

; *** SetupLdr messages
SetupLdrStartupMessage=Este programa instalará %1. Desexa continuar?
LdrCannotCreateTemp=Non se puido crear o arquivo temporal. Instalación interrompida
LdrCannotExecTemp=Non se puido executar o arquivo no cartafol temporal. Instalación interrompida
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nErro %2: %3
SetupFileMissing=O arquivo %1 non se atopa no cartafol de instalación. Por favor, solucione este problema ou consiga unha copia nova do programa.
SetupFileCorrupt=Os arquivos de instalación están danados. Por favor, consiga unha copia nova do programa.
SetupFileCorruptOrWrongVer=Os arquivos de instalación están danados ou son incompatibles con esta versión do programa de instalación. Por favor, solucione este problema ou consiga unha copia nova do programa.
InvalidParameter=Proporcionouse un parámetro non válido na liña de ordes:%n%n%1
SetupAlreadyRunning=O programa de instalación aínda se está a executar.
WindowsVersionNotSupported=Este programa non é compatible coa versión de Windows do seu equipo.
WindowsServicePackRequired=Este programa require %1 Service Pack %2 ou posterior.
NotOnThisPlatform=Este programa non se executará en %1.
OnlyOnThisPlatform=Este programa debe executarse en %1.
OnlyOnTheseArchitectures=Este programa só pode instalarse en versións de Windows deseñadas para as seguintes arquitecturas de procesadores:%n%n%1
WinVersionTooLowError=Este programa require %1 versión %2 ou posterior.
WinVersionTooHighError=Este programa non pode instalarse en %1 versión %2 ou posterior.
AdminPrivilegesRequired=Debe iniciar a sesión como administrador para instalar este programa.
PowerUserPrivilegesRequired=Debe iniciar a sesión como administrador ou como membro do grupo de Usuarios Avanzados para instalar este programa.
SetupAppRunningError=O programa de instalación detectou que %1 se está a executar.%n%nPor favor, pecheo agora, despois faga clic en Aceptar para continuar ou en Cancelar para saír.
UninstallAppRunningError=O desinstalador detectou que %1 se está a executar.%n%nPor favor, pecheo agora, despois faga clic en Aceptar para continuar ou en Cancelar para saír.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Selección do Modo de Instalación
PrivilegesRequiredOverrideInstruction=Seleccione o modo de instalación
PrivilegesRequiredOverrideText1=%1 pode ser instalado para todos os usuarios (require privilexios administrativos), ou só para vostede.
PrivilegesRequiredOverrideText2=%1 pode ser instalado só para vostede, ou para todos os usuarios (require privilexios administrativos).
PrivilegesRequiredOverrideAllUsers=Instalar para &todos os usuarios
PrivilegesRequiredOverrideAllUsersRecommended=Instalar para &todos os usuarios (recomendado)
PrivilegesRequiredOverrideCurrentUser=Instalar para &min soamente
PrivilegesRequiredOverrideCurrentUserRecommended=Instalar para &min soamente (recomendado)

; *** Misc. errors
ErrorCreatingDir=O programa de instalación non puido crear o cartafol "%1"
ErrorTooManyFilesInDir=Non se puido crear un arquivo no cartafol "%1" porque contén demasiados arquivos

; *** Setup common messages
ExitSetupTitle=Saír da Instalación
ExitSetupMessage=A instalación aínda non se completou. Se cancela agora, o programa non se instalará.%n%nPode executar de novo o programa de instalación noutra ocasión para completala.%n%nDesexa saír da instalación?
AboutSetupMenuItem=&Acerca de Instalar...
AboutSetupTitle=Acerca de Instalar
AboutSetupMessage=%1 versión %2%n%3%n%n%1 sitio web:%n%4
AboutSetupNote=
TranslatorNote=Tradución ao galego mantida por Alexandre Espinosa Menor (aemenor@gmail.com)

; *** Buttons
ButtonBack=< &Atrás
ButtonNext=&Seguinte >
ButtonInstall=&Instalar
ButtonOK=Aceptar
ButtonCancel=Cancelar
ButtonYes=&Si
ButtonYesToAll=Si a &Todo
ButtonNo=&Non
ButtonNoToAll=N&on a Todo
ButtonFinish=&Finalizar
ButtonBrowse=&Examinar...
ButtonWizardBrowse=&Examinar...
ButtonNewFolder=&Crear Novo Cartafol

; *** "Select Language" dialog messages
SelectLanguageTitle=Seleccione o Idioma da Instalación
SelectLanguageLabel=Seleccione o idioma a utilizar durante a instalación.

; *** Common wizard text
ClickNext=Faga clic en Seguinte para continuar ou en Cancelar para saír da instalación.
BeveledLabel=
BrowseDialogTitle=Buscar Cartafol
BrowseDialogLabel=Seleccione un cartafol e despois faga clic en Aceptar.
NewFolderName=Novo Cartafol

; *** "Welcome" wizard page
WelcomeLabel1=Benvido ao asistente de instalación de [name]
WelcomeLabel2=Este programa instalará [name/ver] no seu sistema.%n%nRecoméndase pechar todas as demais aplicacións antes de continuar.

; *** "Password" wizard page
WizardPassword=Contrasinal
PasswordLabel1=Esta instalación está protexida por contrasinal.
PasswordLabel3=Por favor, introduza o contrasinal e faga clic en Seguinte para continuar. O contrasinal distingue entre maiúsculas e minúsculas.
PasswordEditLabel=&Contrasinal:
IncorrectPassword=O contrasinal introducido non é correcto. Por favor, inténteo de novo.

; *** "License Agreement" wizard page
WizardLicense=Acordo de Licenza
LicenseLabel=É importante que lea a seguinte información antes de continuar.
LicenseLabel3=Por favor, lea o seguinte acordo de licenza. Debe aceptar as cláusulas deste acordo antes de continuar coa instalación.
LicenseAccepted=A&cepto o acordo
LicenseNotAccepted=&Non acepto o acordo

; *** "Information" wizard pages
WizardInfoBefore=Información
InfoBeforeLabel=É importante que lea a seguinte información antes de continuar.
InfoBeforeClickLabel=Cando estea listo para continuar coa instalación, faga clic en Seguinte.
WizardInfoAfter=Información
InfoAfterLabel=É importante que lea a seguinte información antes de continuar.
InfoAfterClickLabel=Cando estea listo para continuar, faga clic en Seguinte.

; *** "User Information" wizard page
WizardUserInfo=Información do Usuario
UserInfoDesc=Por favor, introduza os seus datos.
UserInfoName=Nome de &Usuario:
UserInfoOrg=&Organización:
UserInfoSerial=Número de &Serie:
UserInfoNameRequired=Debe introducir un nome.

; *** "Select Destination Location" wizard page
WizardSelectDir=Seleccione o Cartafol de Destino
SelectDirDesc=Onde debe instalarse [name]?
SelectDirLabel3=O programa instalará [name] no seguinte cartafol.
SelectDirBrowseLabel=Para continuar, faga clic en Seguinte. Se desexa seleccionar un cartafol diferente, faga clic en Examinar.
DiskSpaceGBLabel=Requírense polo menos [gb] GB de espazo libre no disco.
DiskSpaceMBLabel=Requírense polo menos [mb] MB de espazo libre no disco.
CannotInstallToNetworkDrive=O programa de instalación non pode realizar a instalación nunha unidade de rede.
CannotInstallToUNCPath=O programa de instalación non pode realizar a instalación nunha ruta de acceso UNC.
InvalidPath=Debe introducir unha ruta completa coa letra da unidade; por exemplo:%n%nC:\APP%n%nou unha ruta de acceso UNC da seguinte forma:%n%n\\servidor\compartido
InvalidDrive=A unidade ou ruta de acceso UNC que seleccionou non existe ou non é accesible. Por favor, seleccione outra.
DiskSpaceWarningTitle=Espazo Insuficiente no Disco
DiskSpaceWarning=A instalación require polo menos %1 KB de espazo libre, pero a unidade seleccionada só conta con %2 KB dispoñibles.%n%nDesexa continuar de todas formas?
DirNameTooLong=O nome do cartafol ou a ruta son demasiado longos.
InvalidDirName=O nome do cartafol non é válido.
BadDirName32=Os nomes de cartafoles non poden incluír os seguintes caracteres:%n%n%1
DirExistsTitle=O Cartafol Xa Existe
DirExists=O cartafol:%n%n%1%n%nxa existe. Desexa realizar a instalación nese cartafol de todas formas?
DirDoesntExistTitle=O Cartafol Non Existe
DirDoesntExist=O cartafol:%n%n%1%n%nnon existe. Desexa crear ese cartafol?

; *** "Select Components" wizard page
WizardSelectComponents=Seleccione os Compoñentes
SelectComponentsDesc=Que compoñentes deben instalarse?
SelectComponentsLabel2=Seleccione os compoñentes que desexa instalar e desmarque os compoñentes que non desexa instalar. Faga clic en Seguinte cando estea listo para continuar.
FullInstallation=Instalación Completa
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalación Compacta
CustomInstallation=Instalación Personalizada
NoUninstallWarningTitle=Compoñentes Atopados
NoUninstallWarning=O programa de instalación detectou que os seguintes compoñentes xa están instalados no seu sistema:%n%n%1%n%nDesmarcar estes compoñentes non os desinstalará.%n%nDesexa continuar de todos xeitos?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=A selección actual require polo menos [gb] GB de espazo no disco.
ComponentsDiskSpaceMBLabel=A selección actual require polo menos [mb] MB de espazo no disco.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Seleccione as Tarefas Adicionais
SelectTasksDesc=Que tarefas adicionais deben realizarse?
SelectTasksLabel2=Seleccione as tarefas adicionais que desexa que se realicen durante a instalación de [name] e faga clic en Seguinte.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Seleccione o Cartafol do Menú Inicio
SelectStartMenuFolderDesc=Onde deben colocarse os atallos do programa?
SelectStartMenuFolderLabel3=O programa de instalación creará os atallos do programa no seguinte cartafol do menú Inicio.
SelectStartMenuFolderBrowseLabel=Para continuar, faga clic en Seguinte. Se desexa seleccionar un cartafol diferente, faga clic en Examinar.
MustEnterGroupName=Debe proporcionar un nome de cartafol.
GroupNameTooLong=O nome do cartafol ou a ruta son demasiado longos.
InvalidGroupName=O nome do cartafol non é válido.
BadGroupName=O nome do cartafol non pode incluír ningún dos seguintes caracteres:%n%n%1
NoProgramGroupCheck2=&Non crear un cartafol no menú Inicio

; *** "Ready to Install" wizard page
WizardReady=Listo para Instalar
ReadyLabel1=Agora o programa está listo para iniciar a instalación de [name] no seu sistema.
ReadyLabel2a=Faga clic en Instalar para continuar, ou en Atrás se desexa revisar ou cambiar a configuración.
ReadyLabel2b=Faga clic en Instalar para continuar co proceso.
ReadyMemoUserInfo=Información do usuario:
ReadyMemoDir=Cartafol de destino:
ReadyMemoType=Tipo de instalación:
ReadyMemoComponents=Compoñentes seleccionados:
ReadyMemoGroup=Cartafol do menú Inicio:
ReadyMemoTasks=Tarefas adicionais:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Descargando arquivos...
ButtonStopDownload=&Deter a descarga
StopDownload=Está seguro de que desexa deter a descarga?
ErrorDownloadAborted=Descarga cancelada
ErrorDownloadFailed=Fallou a descarga: %1 %2
ErrorDownloadSizeFailed=Fallou a obtención do tamaño: %1 %2
ErrorProgress=Progreso non válido: %1 de %2
ErrorFileSize=Tamaño de arquivo non válido: agardado %1, atopado %2

; *** TExtractionWizardPage wizard page and ExtractArchive
ExtractingLabel=Extraendo arquivos...
ButtonStopExtraction=&Deter a extracción
StopExtraction=Está seguro de que desexa deter a extracción?
ErrorExtractionAborted=Extracción cancelada
ErrorExtractionFailed=Fallou a extracción: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=O contrasinal é incorrecto
ArchiveIsCorrupted=O arquivo está danado
ArchiveUnsupportedFormat=O formato de arquivo non é compatible

; *** "Preparing to Install" wizard page
WizardPreparing=Preparándose para Instalar
PreparingDesc=O programa de instalación está a prepararse para instalar [name] no seu sistema.
PreviousInstallNotCompleted=A instalación/desinstalación previa dun programa non se completou. Deberá reiniciar o sistema para completar esa instalación.%n%nUnha vez reiniciado o sistema, execute o programa de instalación de novo para completar a instalación de [name].
CannotContinue=O programa de instalación non pode continuar. Por favor, prema Cancelar para saír.
ApplicationsFound=As seguintes aplicacións están a usar arquivos que necesitan ser actualizados polo programa de instalación. Recoméndase que permita ao programa de instalación pechar automaticamente estas aplicacións.
ApplicationsFound2=As seguintes aplicacións están a usar arquivos que necesitan ser actualizados polo programa de instalación. Recoméndase que permita ao programa de instalación pechar automaticamente estas aplicacións. Ao completarse a instalación, o programa de instalación intentará reiniciar as aplicacións.
CloseApplications=&Pechar automaticamente as aplicacións
DontCloseApplications=&Non pechar as aplicacións
ErrorCloseApplications=O programa de instalación non puido pechar de forma automática todas as aplicacións. Recoméndase que, antes de continuar, peche todas as aplicacións que utilicen arquivos que necesiten ser actualizados polo programa de instalación.
PrepareToInstallNeedsRestart=O programa de instalación precisa reiniciar o sistema. Unha vez que se teña reiniciado, execute de novo o programa de instalación para completar a instalación de [name].%n%nDesexa reiniciar o sistema agora?

; *** "Installing" wizard page
WizardInstalling=Instalando
InstallingLabel=Por favor, agarde mentres se instala [name] no seu sistema.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Completando a instalación de [name]
FinishedLabelNoIcons=O programa completou a instalación de [name] no seu sistema.
FinishedLabel=O programa completou a instalación de [name] no seu sistema. Pode executar a aplicación utilizando os atallos creados.
ClickFinish=Faga clic en Finalizar para saír do programa de instalación.
FinishedRestartLabel=Para completar a instalación de [name], o seu sistema debe reiniciarse. Desexa reinicialo agora?
FinishedRestartMessage=Para completar a instalación de [name], o seu sistema debe reiniciarse.%n%nDesexa reinicialo agora?
ShowReadmeCheck=Si, desexo ver o arquivo LÉAME
YesRadio=&Si, desexo reiniciar o sistema agora
NoRadio=&Non, reiniciarei o sistema máis tarde
; used for example as 'Run MyProg.exe'
RunEntryExec=Executar %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Ver %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=O Programa de Instalación Necesita o Seguinte Disco
SelectDiskLabel2=Por favor, insira o Disco %1 e faga clic en Aceptar.%n%nSe os arquivos se poden atopar nun cartafol diferente ao indicado abaixo, introduza a ruta correcta ou faga clic en Examinar.
PathLabel=&Ruta:
FileNotInDir2=O arquivo "%1" non se puido atopar en "%2". Por favor, insira o disco correcto ou seleccione outro cartafol.
SelectDirectoryLabel=Por favor, especifique a localización do seguinte disco.

; *** Installation phase messages
SetupAborted=A instalación non se completou.%n%nPor favor solucione o problema e execute de novo o programa de instalación.
AbortRetryIgnoreSelectAction=Seleccione acción
AbortRetryIgnoreRetry=&Reintentar
AbortRetryIgnoreIgnore=&Ignorar o erro e continuar
AbortRetryIgnoreCancel=Cancelar a instalación
RetryCancelSelectAction=Seleccione acción
RetryCancelRetry=&Reintentar
RetryCancelCancel=Cancelar

; *** Installation status messages
StatusClosingApplications=Pechando aplicacións...
StatusCreateDirs=Creando cartafoles...
StatusExtractFiles=Extraendo arquivos...
StatusDownloadFiles=Descargando arquivos...
StatusCreateIcons=Creando atallos...
StatusCreateIniEntries=Creando entradas INI...
StatusCreateRegistryEntries=Creando entradas do rexistro...
StatusRegisterFiles=Rexistrando arquivos...
StatusSavingUninstall=Gardando información para desinstalar...
StatusRunProgram=Rematando a instalación...
StatusRestartingApplications=Reiniciando aplicacións...
StatusRollback=Desfacendo cambios...

; *** Misc. errors
ErrorInternal2=Erro interno: %1
ErrorFunctionFailedNoCode=%1 fallou
ErrorFunctionFailed=%1 fallou; código %2
ErrorFunctionFailedWithMessage=%1 fallou; código %2.%n%3
ErrorExecutingProgram=Non se puido executar o arquivo:%n%1

; *** Registry errors
ErrorRegOpenKey=Erro ao abrir a chave do rexistro:%n%1\%2
ErrorRegCreateKey=Erro ao crear a chave do rexistro:%n%1\%2
ErrorRegWriteKey=Erro ao escribir a chave do rexistro:%n%1\%2

; *** INI errors
ErrorIniEntry=Erro ao crear a entrada INI no arquivo "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Omitir este arquivo (non recomendado)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignorar o erro e continuar (non recomendado)
SourceIsCorrupted=O arquivo de orixe está danado
SourceDoesntExist=O arquivo de orixe "%1" non existe
SourceVerificationFailed=A verificación do arquivo de orixe fallou: %1
VerificationSignatureDoesntExist=Non existe o arquivo de sinatura "%1"
VerificationSignatureInvalid=O arquivo de sinatura "%1" non é válido
VerificationKeyNotFound=O arquivo de sinatura "%1" utiliza unha chave descoñecida
VerificationFileNameIncorrect=O nome do arquivo é incorrecto
VerificationFileTagIncorrect=A etiqueta do arquivo é incorrecta
VerificationFileSizeIncorrect=O tamaño do arquivo é incorrecto
VerificationFileHashIncorrect=O hash do arquivo é incorrecto
ExistingFileReadOnly2=O arquivo existente non se pode substituír debido a que está marcado como só lectura.
ExistingFileReadOnlyRetry=&Elimine o atributo de só lectura e reintente
ExistingFileReadOnlyKeepExisting=&Manter o arquivo existente
ErrorReadingExistingDest=Ocorreu un erro mentres se intentaba ler o arquivo:
FileExistsSelectAction=Seleccione acción
FileExists2=O arquivo xa existe.
FileExistsOverwriteExisting=&Sobrescribir o arquivo existente
FileExistsKeepExisting=&Manter o arquivo existente
FileExistsOverwriteOrKeepAll=&Facer o mesmo para os seguintes conflitos
ExistingFileNewerSelectAction=Seleccione acción
ExistingFileNewer2=O arquivo existente é máis recente que o que se está a tentar instalar.
ExistingFileNewerOverwriteExisting=&Sobrescribir o arquivo existente
ExistingFileNewerKeepExisting=&Manter o arquivo existente (recomendado)
ExistingFileNewerOverwriteOrKeepAll=&Facer o mesmo para os seguintes conflitos
ErrorChangingAttr=Ocorreu un erro ao tentar cambiar os atributos do arquivo:
ErrorCreatingTemp=Ocorreu un erro ao tentar crear un arquivo no cartafol de destino:
ErrorReadingSource=Ocorreu un erro ao tentar ler o arquivo de orixe:
ErrorCopying=Ocorreu un erro ao tentar copiar o arquivo:
ErrorDownloading=Ocorreu un erro ao tentar descargar un arquivo:
ErrorExtracting=Ocorreu un erro ao tentar extraer un arquivo:
ErrorReplacingExistingFile=Ocorreu un erro ao tentar substituír o arquivo existente:
ErrorRestartReplace=Fallou o reintento de substituír:
ErrorRenamingTemp=Ocorreu un erro ao tentar renomear un arquivo no cartafol de destino:
ErrorRegisterServer=Non se puido rexistrar o DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 fallou co código de saída %1
ErrorRegisterTypeLib=Non se puido rexistrar a librería de tipos: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32 bits
UninstallDisplayNameMark64Bit=64 bits
UninstallDisplayNameMarkAllUsers=Todos os usuarios
UninstallDisplayNameMarkCurrentUser=Usuario actual

; *** Post-installation errors
ErrorOpeningReadme=Ocorreu un erro ao tentar abrir o arquivo LÉAME.
ErrorRestartingComputer=O programa de instalación non puido reiniciar o equipo. Por favor, fágao manualmente.

; *** Uninstaller messages
UninstallNotFound=O arquivo "%1" non existe. Non se puido desinstalar.
UninstallOpenError=O arquivo "%1" non se puido abrir. Non se puido desinstalar
UninstallUnsupportedVer=O arquivo de rexistro para desinstalar "%1" está nun formato non recoñecido por esta versión do desinstalador. Non se puido desinstalar
UninstallUnknownEntry=Atopouse unha entrada descoñecida (%1) no rexistro de desinstalación
ConfirmUninstall=Está seguro de que desexa desinstalar completamente %1 e todos os seus compoñentes?
UninstallOnlyOnWin64=Este programa só pode ser desinstalado en Windows de 64 bits.
OnlyAdminCanUninstall=Este programa só pode ser desinstalado por un usuario con privilexios administrativos.
UninstallStatusLabel=Por favor, agarde mentres %1 se desinstala do seu sistema.
UninstalledAll=%1 desinstalouse correctamente do seu sistema.
UninstalledMost=A desinstalación de %1 completouse.%n%nAlgúns elementos non se puideron eliminar, pero poderá eliminalos manualmente se o desexa.
UninstalledAndNeedsRestart=Para completar a desinstalación de %1, o seu sistema debe reiniciarse.%n%nDesexa reinicialo agora?
UninstallDataCorrupted=O arquivo "%1" está danado. Non se pode desinstalar

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Eliminar arquivo compartido?
ConfirmDeleteSharedFile2=O sistema indica que o seguinte arquivo compartido non é utilizado por ningún outro programa. Desexa eliminar este arquivo compartido?%n%nSe elimina o arquivo e hai programas que o utilizan, eses programas poderían deixar de funcionar correctamente. Se non está seguro, elixa Non. Deixar o arquivo no seu sistema non producirá ningún dano.
SharedFileNameLabel=Arquivo:
SharedFileLocationLabel=Localización:
WizardUninstalling=Estado da Desinstalación
StatusUninstalling=Desinstalando %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instalando %1.
ShutdownBlockReasonUninstallingApp=Desinstalando %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versión %2
AdditionalIcons=Atallos adicionais:
CreateDesktopIcon=Crear un atallo no &escritorio
CreateQuickLaunchIcon=Crear un atallo en &Inicio rápido
ProgramOnTheWeb=%1 na Web
UninstallProgram=Desinstalar %1
LaunchProgram=Executar %1
AssocFileExtension=&Asociar %1 coa extensión de arquivo %2
AssocingFileExtension=Asociando %1 coa extensión de arquivo %2...
AutoStartProgramGroupDescription=Inicio:
AutoStartProgram=Iniciar automaticamente %1
AddonHostProgramNotFound=%1 non se puido localizar no cartafol seleccionado.%n%nDesexa continuar de todas formas?