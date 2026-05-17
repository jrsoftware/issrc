; *** Inno Setup version 6.5.0+ Galician messages ***

; Maintained by MdL (mdl.dixital@gmail.com) and AI
;
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
LdrCannotCreateTemp=Non se puido crear o ficheiro temporal. Instalación interrompida
LdrCannotExecTemp=Non se puido executar o ficheiro na carpeta temporal. Instalación interrompida
HelpTextNote=
; *** Startup error messages
LastErrorMessage=%1.%n%nErro %2: %3
SetupFileMissing=O ficheiro %1 non se atopa na carpeta de instalación. Por favor, solucione este problema ou consiga unha nova copia do programa.
SetupFileCorrupt=Os ficheiros de instalación están danados. Por favor, consiga unha nova copia do programa.
SetupFileCorruptOrWrongVer=Os ficheiros de instalación están danados ou son incompatibles con esta versión do programa de instalación. Por favor, solucione este problema ou consiga unha nova copia do programa.
InvalidParameter=Proporcionouse un parámetro non válido na liña de comandos:%n%n%1
SetupAlreadyRunning=O programa de instalación xa se está executando.
WindowsVersionNotSupported=Este programa non é compatible coa versión de Windows do seu equipo.
WindowsServicePackRequired=Este programa require %1 Service Pack %2 ou posterior.
NotOnThisPlatform=Este programa non se executará en %1.
OnlyOnThisPlatform=Este programa debe executarse en %1.
OnlyOnTheseArchitectures=Este programa só se pode instalar en versións de Windows deseñadas para as seguintes arquitecturas de procesador:%n%n%1
WinVersionTooLowError=Este programa require %1 versión %2 ou posterior.
WinVersionTooHighError=Este programa non se pode instalar en %1 versión %2 ou posterior.
AdminPrivilegesRequired=Debe iniciar sesión como administrador para instalar este programa.
PowerUserPrivilegesRequired=Debe iniciar sesión como administrador ou como membro do grupo de Usuarios Avanzados para instalar este programa.
SetupAppRunningError=O programa de instalación detectou que %1 se está executando.%n%nPor favor, pécheo agora, despois faga clic en Aceptar para continuar ou en Cancelar para saír.
UninstallAppRunningError=O desinstalador detectou que %1 se está executando.%n%nPor favor, pécheo agora, despois faga clic en Aceptar para continuar ou en Cancelar para saír.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Selección do Modo de Instalación
PrivilegesRequiredOverrideInstruction=Seleccione o modo de instalación
PrivilegesRequiredOverrideText1=%1 pode ser instalado para todos os usuarios (require privilexios de administrador), ou só para vostede.
PrivilegesRequiredOverrideText2=%1 pode ser instalado só para vostede, ou para todos os usuarios (require privilexios de administrador).
PrivilegesRequiredOverrideAllUsers=Instalar para &todos os usuarios
PrivilegesRequiredOverrideAllUsersRecommended=Instalar para &todos os usuarios (recomendado)
PrivilegesRequiredOverrideCurrentUser=Instalar só para &min
PrivilegesRequiredOverrideCurrentUserRecommended=Instalar só para &min (recomendado)

; *** Misc. errors
ErrorCreatingDir=O programa de instalación non puido crear a carpeta "%1"
ErrorTooManyFilesInDir=Non se puido crear un ficheiro na carpeta "%1" porque contén demasiados ficheiros

; *** Setup common messages
ExitSetupTitle=Saír da Instalación
ExitSetupMessage=A instalación aínda non se completou. Se cancela agora, o programa non se instalará.%n%nPode executar de novo o programa de instalación noutra ocasión para completala.%n%nDesexa saír da instalación?
AboutSetupMenuItem=&Acerca de Instalar...
AboutSetupTitle=Acerca de Instalar
AboutSetupMessage=%1 versión %2%n%3%n%nSitio web de %1:%n%4
AboutSetupNote=
TranslatorNote=Tradución ao galego baseada no estándar de Inno Setup.

; *** Buttons
ButtonBack=< &Atrás
ButtonNext=&Seguinte >
ButtonInstall=&Instalar
ButtonOK=Aceptar
ButtonCancel=Cancelar
ButtonYes=&Si
ButtonYesToAll=Si a &todo
ButtonNo=&Non
ButtonNoToAll=N&on a todo
ButtonFinish=&Finalizar
ButtonBrowse=&Examinar...
ButtonWizardBrowse=&Examinar...
ButtonNewFolder=&Crear nova carpeta

; *** "Select Language" dialog messages
SelectLanguageTitle=Seleccione o idioma da instalación
SelectLanguageLabel=Seleccione o idioma que se utilizará durante a instalación.

; *** Common wizard text
ClickNext=Faga clic en Seguinte para continuar ou en Cancelar para saír da instalación.
BeveledLabel=
BrowseDialogTitle=Buscar carpeta
BrowseDialogLabel=Seleccione unha carpeta e despois faga clic en Aceptar.
NewFolderName=Nova carpeta

; *** "Welcome" wizard page
WelcomeLabel1=Benvido ao asistente de instalación de [name]
WelcomeLabel2=Este programa instalará [name/ver] no seu sistema.%n%nRecoméndase pechar todas as demais aplicacións antes de continuar.

; *** "Password" wizard page
WizardPassword=Contrasinal
PasswordLabel1=Esta instalación está protexida por contrasinal.
PasswordLabel3=Por favor, introduza o contrasinal e faga clic en Seguinte para continuar. O contrasinal distingue entre maiúsculas e minúsculas.
PasswordEditLabel=&Contrasinal:
IncorrectPassword=O contrasinal introducido non é correcto. Por favor, ténteo de novo.

; *** "License Agreement" wizard page
WizardLicense=Acordo de licenza
LicenseLabel=É importante que lea a seguinte información antes de continuar.
LicenseLabel3=Por favor, lea o seguinte acordo de licenza. Debe aceptar os termos deste acordo antes de continuar coa instalación.
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
WizardUserInfo=Información do usuario
UserInfoDesc=Por favor, introduza os seus datos.
UserInfoName=Nome de &usuario:
UserInfoOrg=&Organización:
UserInfoSerial=Número de &serie:
UserInfoNameRequired=Debe introducir un nome.

; *** "Select Destination Location" wizard page
WizardSelectDir=Seleccione a carpeta de destino
SelectDirDesc=Onde se debe instalar [name]?
SelectDirLabel3=O programa instalará [name] na seguinte carpeta.
SelectDirBrowseLabel=Para continuar, faga clic en Seguinte. Se desexa seleccionar unha carpeta diferente, faga clic en Examinar.
DiskSpaceGBLabel=Requírense polo menos [gb] GB de espazo libre no disco.
DiskSpaceMBLabel=Requírense polo menos [mb] MB de espazo libre no disco.
CannotInstallToNetworkDrive=O programa de instalación non pode realizar a instalación nunha unidade de rede.
CannotInstallToUNCPath=O programa de instalación non pode realizar a instalación nunha ruta de acceso UNC.
InvalidPath=Debe introducir unha ruta completa coa letra da unidade; por exemplo:%n%nC:\APP%n%nou unha ruta de acceso UNC do seguinte xeito:%n%n\\servidor\compartido
InvalidDrive=A unidade ou ruta de acceso UNC que seleccionou non existe ou non está accesible. Por favor, seleccione outra.
DiskSpaceWarningTitle=Espazo insuficiente no disco
DiskSpaceWarning=A instalación require polo menos %1 KB de espazo libre, pero a unidade seleccionada só conta con %2 KB dispoñibles.%n%nDesexa continuar de todos os xeitos?
DirNameTooLong=O nome da carpeta ou a ruta son demasiado longos.
InvalidDirName=O nome da carpeta non é válido.
BadDirName32=Os nomes de carpetas non poden incluír os seguintes caracteres:%n%n%1
DirExistsTitle=A carpeta xa existe
DirExists=A carpeta:%n%n%1%n%nxa existe. Desexa realizar a instalación nesa carpeta de todos os xeitos?
DirDoesntExistTitle=A carpeta non existe
DirDoesntExist=A carpeta:%n%n%1%n%nnon existe. Desexa crear esa carpeta?

; *** "Select Components" wizard page
WizardSelectComponents=Seleccione os compoñentes
SelectComponentsDesc=Que compoñentes se deben instalar?
SelectComponentsLabel2=Seleccione os compoñentes que desexa instalar e desmarque os compoñentes que non queira instalar. Faga clic en Seguinte cando estea listo para continuar.
FullInstallation=Instalación completa
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalación compacta
CustomInstallation=Instalación personalizada
NoUninstallWarningTitle=Compoñentes atopados
NoUninstallWarning=O programa de instalación detectou que os seguintes compoñentes xa están instalados no seu sistema:%n%n%1%n%nDesmarcar estes compoñentes non os desinstalará.%n%nDesexa continuar de todos os xeitos?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=A selección actual require polo menos [gb] GB de espazo no disco.
ComponentsDiskSpaceMBLabel=A selección actual require polo menos [mb] MB de espazo no disco.
; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Seleccione as tarefas adicionais
SelectTasksDesc=Que tarefas adicionais se deben realizar?
SelectTasksLabel2=Seleccione as tarefas adicionais que desexa que se realicen durante a instalación de [name] e faga clic en Seguinte.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Seleccione a carpeta do menú Inicio
SelectStartMenuFolderDesc=Onde se deben colocar os accesos directos do programa?
SelectStartMenuFolderLabel3=O programa de instalación creará os accesos directos do programa na seguinte carpeta do menú Inicio.
SelectStartMenuFolderBrowseLabel=Para continuar, faga clic en Seguinte. Se desexa seleccionar unha carpeta distinta, faga clic en Examinar.
MustEnterGroupName=Debe proporcionar un nome de carpeta.
GroupNameTooLong=O nome da carpeta ou a ruta son demasiado longos.
InvalidGroupName=O nome da carpeta non é válido.
BadGroupName=O nome da carpeta non pode incluír ningún dos seguintes caracteres:%n%n%1
NoProgramGroupCheck2=&Non crear unha carpeta no menú Inicio

; *** "Ready to Install" wizard page
WizardReady=Listo para instalar
ReadyLabel1=Agora o programa está listo para iniciar a instalación de [name] no seu sistema.
ReadyLabel2a=Faga clic en Instalar para continuar, ou en Atrás se desexa revisar ou cambiar a configuración.
ReadyLabel2b=Faga clic en Instalar para continuar co proceso.
ReadyMemoUserInfo=Información do usuario:
ReadyMemoDir=Carpeta de destino:
ReadyMemoType=Tipo de instalación:
ReadyMemoComponents=Compoñentes seleccionados:
ReadyMemoGroup=Carpeta do menú Inicio:
ReadyMemoTasks=Tarefas adicionais:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Descargando ficheiros...
ButtonStopDownload=&Deter a descarga
StopDownload=Está seguro de que desexa deter a descarga?
ErrorDownloadAborted=Descarga cancelada
ErrorDownloadFailed=Fallou a descarga: %1 %2
ErrorDownloadSizeFailed=Fallou a obtención do tamaño: %1 %2
ErrorProgress=Progreso non válido: %1 de %2
ErrorFileSize=Tamaño do ficheiro non válido: agardábase %1, atopouse %2

; *** TExtractionWizardPage wizard page and ExtractArchive
ExtractingLabel=Extraendo ficheiros...
ButtonStopExtraction=&Deter a extracción
StopExtraction=Está seguro de que desexa deter a extracción?
ErrorExtractionAborted=Extracción cancelada
ErrorExtractionFailed=Fallou a extracción: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=O contrasinal é incorrecto
ArchiveIsCorrupted=O arquivo está danado
ArchiveUnsupportedFormat=O formato de arquivo non é compatible

; *** "Preparing to Install" wizard page
WizardPreparing=Preparándose para instalar
PreparingDesc=O programa de instalación estase preparando para instalar [name] no seu sistema.
PreviousInstallNotCompleted=A instalación/desinstalación previa dun programa non se completou. Deberá reiniciar o sistema para completar esa instalación.%n%nUnha vez reiniciado o sistema, execute o programa de instalación de novo para completar a instalación de [name].
CannotContinue=O programa de instalación non pode continuar. Por favor, prema Cancelar para saír.
ApplicationsFound=As seguintes aplicacións están a usar ficheiros que precisan ser actualizados polo programa de instalación. Recoméndase que permita ao programa de instalación pechar automaticamente estas aplicacións.
ApplicationsFound2=As seguintes aplicacións están a usar ficheiros que precisan ser actualizados polo programa de instalación. Recoméndase que permita ao programa de instalación pechar automaticamente estas aplicacións. Ao completarse a instalación, o programa de instalación tentará reiniciar as aplicacións.
CloseApplications=&Pechar automaticamente as aplicacións
DontCloseApplications=&Non pechar as aplicacións
ErrorCloseApplications=O programa de instalación non puido pechar de forma automática todas as aplicacións. Recoméndase que, antes de continuar, peche todas as aplicacións que utilicen ficheiros que precisan ser actualizados polo programa de instalación.
PrepareToInstallNeedsRestart=O programa de instalación precisa reiniciar o sistema. Unha vez que se teña reiniciado execute de novo o programa de instalación para completar a instalación de [name].%n%nDesexa reiniciar o sistema agora?

; *** "Installing" wizard page
WizardInstalling=Instalando
InstallingLabel=Por favor, agarde mentres se instala [name] no seu sistema.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Completando a instalación de [name]
FinishedLabelNoIcons=O programa completou a instalación de [name] no seu sistema.
FinishedLabel=O programa completou a instalación de [name] no seu sistema. Pode executar a aplicación utilizando os accesos directos creados.
ClickFinish=Faga clic en Finalizar para saír do programa de instalación.
FinishedRestartLabel=Para completar a instalación de [name], o seu sistema debe reiniciarse. Desexa reinicialo agora?
FinishedRestartMessage=Para completar a instalación de [name], o seu sistema debe reiniciarse.%n%nDesexa reiniciar agora?
ShowReadmeCheck=Si, desexo ver o ficheiro LÉAME
YesRadio=&Si, desexo reiniciar o sistema agora
NoRadio=&Non, reiniciarei o sistema máis tarde
; used for example as 'Run MyProg.exe'
RunEntryExec=Executar %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Ver %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=O programa de instalación precisa o seguinte disco
SelectDiskLabel2=Por favor, insira o Disco %1 e faga clic en Aceptar.%n%nSe os ficheiros se poden atopar nunha carpeta diferente á indicada abaixo, introduza a ruta correcta ou faga clic en Examinar.
PathLabel=&Ruta:
FileNotInDir2=O ficheiro "%1" non se puido atopar en "%2". Por favor, insira o disco correcto ou seleccione outra carpeta.
SelectDirectoryLabel=Por favor, especifique a localización do seguinte disco.

; *** Installation phase messages
SetupAborted=A instalación non se completou.%n%nPor favor solucione o problema e execute de novo o programa de instalación.
AbortRetryIgnoreSelectAction=Seleccione unha acción
AbortRetryIgnoreRetry=&Reintentar
AbortRetryIgnoreIgnore=&Ignorar o erro e continuar
AbortRetryIgnoreCancel=Cancelar a instalación
RetryCancelSelectAction=Seleccione unha acción
RetryCancelRetry=&Reintentar
RetryCancelCancel=Cancelar

; *** Installation status messages
StatusClosingApplications=Pechando aplicacións...
StatusCreateDirs=Creando carpetas...
StatusExtractFiles=Extraendo ficheiros...
StatusDownloadFiles=Descargando ficheiros...
StatusCreateIcons=Creando accesos directos...
StatusCreateIniEntries=Creando entradas INI...
StatusCreateRegistryEntries=Creando entradas do rexistro...
StatusRegisterFiles=Rexistrando ficheiros...
StatusSavingUninstall=Gardando a información para desinstalar...
StatusRunProgram=Terminando a instalación...
StatusRestartingApplications=Reiniciando aplicacións...
StatusRollback=Desfacendo os cambios...

; *** Misc. errors
ErrorInternal2=Erro interno: %1
ErrorFunctionFailedNoCode=Fallou %1
ErrorFunctionFailed=Fallou %1; código %2
ErrorFunctionFailedWithMessage=Fallou %1; código %2.%n%3
ErrorExecutingProgram=Non se puido executar o ficheiro:%n%1

; *** Registry errors
ErrorRegOpenKey=Erro ao abrir a chave do rexistro:%n%1\%2
ErrorRegCreateKey=Erro ao crear a chave do rexistro:%n%1\%2
ErrorRegWriteKey=Erro ao escribir a chave do rexistro:%n%1\%2

; *** INI errors
ErrorIniEntry=Erro ao crear a entrada INI no ficheiro "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Omitir este ficheiro (non recomendado)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignorar o erro e continuar (non recomendado)
SourceIsCorrupted=O ficheiro de orixe está danado
SourceDoesntExist=O ficheiro de orixe "%1" non existe
SourceVerificationFailed=Fallou a verificación do ficheiro de orixe: %1
VerificationSignatureDoesntExist=Non existe o ficheiro de sinatura "%1"
VerificationSignatureInvalid=O ficheiro de sinatura "%1" non é válido
VerificationKeyNotFound=O ficheiro de sinatura "%1" utiliza unha chave descoñecida
VerificationFileNameIncorrect=O nome do ficheiro é incorrecto
VerificationFileTagIncorrect=A etiqueta do ficheiro é incorrecta
VerificationFileSizeIncorrect=O tamaño do ficheiro é incorrecto
VerificationFileHashIncorrect=O hash do ficheiro é incorrecto
ExistingFileReadOnly2=O ficheiro existente non se pode substituír porque está marcado como de só lectura.
ExistingFileReadOnlyRetry=&Eliminar o atributo de só lectura e reintentar
ExistingFileReadOnlyKeepExisting=&Manter o ficheiro existente
ErrorReadingExistingDest=Ocorreu un erro ao tentar ler o ficheiro:
FileExistsSelectAction=Seleccione unha acción
FileExists2=O ficheiro xa existe.
FileExistsOverwriteExisting=&Substituír o ficheiro existente
FileExistsKeepExisting=&Manter o ficheiro existente
FileExistsOverwriteOrKeepAll=&Facer o mesmo para os seguintes conflitos
ExistingFileNewerSelectAction=Seleccione unha acción
ExistingFileNewer2=O ficheiro existente é máis recente que o que se está a tentar instalar.
ExistingFileNewerOverwriteExisting=&Substituír o ficheiro existente
ExistingFileNewerKeepExisting=&Manter o ficheiro existente (recomendado)
ExistingFileNewerOverwriteOrKeepAll=&Facer o mesmo para os seguintes conflitos
ErrorChangingAttr=Ocorreu un erro ao tentar cambiar os atributos do ficheiro:
ErrorCreatingTemp=Ocorreu un erro ao tentar crear un ficheiro na carpeta de destino:
ErrorReadingSource=Ocorreu un erro ao tentar ler o ficheiro de orixe:
ErrorCopying=Ocorreu un erro ao tentar copiar o ficheiro:
ErrorDownloading=Ocorreu un erro ao tentar descargar un ficheiro:
ErrorExtracting=Ocorreu un erro ao tentar extraer un ficheiro:
ErrorReplacingExistingFile=Ocorreu un erro ao tentar substituír o ficheiro existente:
ErrorRestartReplace=Fallou o intento de substituír ao reiniciar:
ErrorRenamingTemp=Ocorreu un erro ao tentar cambiar o nome dun ficheiro na carpeta de destino:
ErrorRegisterServer=Non se puido rexistrar o DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 fallou co código de saída %1
ErrorRegisterTypeLib=Non se puido rexistrar a biblioteca de tipos: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=Todos os usuarios
UninstallDisplayNameMarkCurrentUser=Usuario actual

; *** Post-installation errors
ErrorOpeningReadme=Ocorreu un erro ao tentar abrir o ficheiro LÉAME.
ErrorRestartingComputer=O programa de instalación non puido reiniciar o equipo. Por favor, fágao manualmente.

; *** Uninstaller messages
UninstallNotFound=O ficheiro "%1" non existe. Non se puido desinstalar.
UninstallOpenError=Non se puido abrir o ficheiro "%1". Non se puido desinstalar.
UninstallUnsupportedVer=O ficheiro de rexistro para desinstalar "%1" está nun formato non recoñecido por esta versión do desinstalador. Non se puido desinstalar.
UninstallUnknownEntry=Atopouse unha entrada descoñecida (%1) no rexistro de desinstalación.
ConfirmUninstall=Está seguro de que desexa desinstalar por completo %1 e todos os seus compoñentes?
UninstallOnlyOnWin64=Este programa só se pode desinstalar en Windows de 64-bits.
OnlyAdminCanUninstall=Este programa só o pode desinstalar un usuario con privilexios de administrador.
UninstallStatusLabel=Por favor, agarde mentres se desinstala %1 do seu sistema.
UninstalledAll=Eliminouse %1 con éxito do seu sistema.
UninstalledMost=Completouse a desinstalación de %1.%n%nAlgúns elementos non se puideron eliminar, pero poderá eliminalos manualmente se o desexa.
UninstalledAndNeedsRestart=Para completar a desinstalación de %1, o seu sistema debe reiniciarse.%n%nDesexa reiniciar agora?
UninstallDataCorrupted=O ficheiro "%1" está danado. Non se pode desinstalar.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Eliminar ficheiro compartido?
ConfirmDeleteSharedFile2=O sistema indica que o seguinte ficheiro compartido non o utiliza ningún outro programa. Desexa eliminar este ficheiro compartido?%n%nSe elimina o ficheiro e hai programas que o utilizan, eses programas poderían deixar de funcionar correctamente. Se non está seguro, escolla Non. Deixar o ficheiro no seu sistema non causará ningún dano.
SharedFileNameLabel=Ficheiro:
SharedFileLocationLabel=Localización:
WizardUninstalling=Estado da desinstalación
StatusUninstalling=Desinstalando %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instalando %1.
ShutdownBlockReasonUninstallingApp=Desinstalando %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versión %2
AdditionalIcons=Accesos directos adicionais:
CreateDesktopIcon=Crear un acceso directo no &escritorio
CreateQuickLaunchIcon=Crear un acceso directo en &Inicio Rápido
ProgramOnTheWeb=%1 na Web
UninstallProgram=Desinstalar %1
LaunchProgram=Executar %1
AssocFileExtension=&Asociar %1 coa extensión de ficheiro %2
AssocingFileExtension=Asociando %1 coa extensión de ficheiro %2...
AutoStartProgramGroupDescription=Inicio:
AutoStartProgram=Iniciar automaticamente %1
AddonHostProgramNotFound=Non se puido localizar %1 na carpeta seleccionada.%n%nDesexa continuar de todos os xeitos?
