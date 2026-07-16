; *** Inno Setup version 6.5.0+ Galician messages ***

;Translated by MdL (email: mdl.dixital@gmail.com)

; To download user-contributed translations of this file, go to:
;   https://jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
LanguageName=Galego
LanguageID=$0456
LanguageCodePage=1252
;DialogFontName=
;DialogFontSize=9
;DialogFontBaseScaleWidth=7
;DialogFontBaseScaleHeight=15
;WelcomeFontName=Segoe UI
;WelcomeFontSize=12

[Messages]

; *** Application titles
SetupAppTitle=Instalar
SetupWindowTitle=Instalar - %1
UninstallAppTitle=Desinstalar
UninstallAppFullTitle=%1 Desinstalar

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
SetupFileMissing=O arquivo %1 non se atopa no cartafol de instalación. Por favor, solucione este problema ou consiga unha nova copia do programa.
SetupFileCorrupt=Os arquivos de instalación están danados. Por favor, consiga unha nova copia do programa.
SetupFileCorruptOrWrongVer=Os arquivos de instalación están danados ou son incompatibles con esta versión do programa de instalación. Por favor, solucione este problema ou consiga unha nova copia do programa.
InvalidParameter=Proporcionouse un parámetro non válido na liña de comandos:%n%n%1
SetupAlreadyRunning=O programa de instalación estase executando.
WindowsVersionNotSupported=Este programa non é compatible coa versión de Windows do seu equipo.
WindowsServicePackRequired=Este programa require %1 Service Pack %2 ou posterior.
NotOnThisPlatform=Este programa non se executará en %1.
OnlyOnThisPlatform=Este programa debe executarse en %1.
OnlyOnTheseArchitectures=Este programa só se pode instalar en versións de Windows deseñadas para as seguintes arquitecturas de procesador:%n%n%1
WinVersionTooLowError=Este programa require %1 versión %2 ou posterior.
WinVersionTooHighError=Este programa non se pode instalar en %1 versión %2 ou posterior.
AdminPrivilegesRequired=Debe iniciar a sesión como administrador para instalar este programa.
PowerUserPrivilegesRequired=Debe iniciar a sesión como administrador ou como membro do grupo de Usuarios Avanzados para instalar este programa.
SetupAppRunningError=O programa de instalación detectou que %1 se está executando.%n%nPor favor, pécheo agora, despois prema en Aceptar para continuar ou en Cancelar para saír.
UninstallAppRunningError=O desinstalador detectou que %1 se está executando.%n%nPor favor, pécheo agora, despois prema en Aceptar para continuar ou en Cancelar para saír.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Seleccione o modo de instalación
PrivilegesRequiredOverrideInstruction=Seleccione o modo de instalación
PrivilegesRequiredOverrideText1=%1 pode ser instalado para todos os usuarios (require privilexios de administrador), ou só para vostede.
PrivilegesRequiredOverrideText2=%1 pode ser instalado só para vostede, ou para todos os usuarios (require privilexios de administrador).
PrivilegesRequiredOverrideAllUsers=Instalar para &todos os usuarios
PrivilegesRequiredOverrideAllUsersRecommended=Instalar para &todos os usuarios (aconséllase)
PrivilegesRequiredOverrideCurrentUser=Instalar só para &min
PrivilegesRequiredOverrideCurrentUserRecommended=Instalar só para &min (aconséllase)

; *** Misc. errors
ErrorCreatingDir=O programa de instalación non puido crear o cartafol "%1"
ErrorTooManyFilesInDir=Non se puido crear un arquivo no cartafol "%1" porque contén demasiados arquivos

; *** Setup common messages
ExitSetupTitle=Saír do instalador
ExitSetupMessage=A instalación aínda non rematou. Se cancela agora, o programa non se instalará.%n%nPode executar de novo o programa de instalación noutra ocasión para completala.%n%nDesexa saír da instalación?
AboutSetupMenuItem=&Acerca de Instalar...
AboutSetupTitle=Acerca de Instalar
AboutSetupMessage=%1 versión %2%n%3%n%n%1 Páxina web:%n%4
AboutSetupNote=
TranslatorNote=

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
ButtonFinish=&Rematar
ButtonBrowse=&Examinar...
ButtonWizardBrowse=&Examinar...
ButtonNewFolder=&Crear novo cartafol

; *** "Select Language" dialog messages
SelectLanguageTitle=Seleccione o idioma do instalador
SelectLanguageLabel=Seleccione o idioma que se utilizará durante a instalación.

; *** Common wizard text
ClickNext=Prema en Seguinte para continuar ou en Cancelar para saír da instalación.
BeveledLabel=
BrowseDialogTitle=Examinar cartafol
BrowseDialogLabel=Seleccione un cartafol na lista de abaixo e despois prema en Aceptar.
NewFolderName=Novo cartafol

; *** "Welcome" wizard page
WelcomeLabel1=Benvido ao asistente de instalación de [name]
WelcomeLabel2=Este programa instalará [name/ver] no seu computador.%n%nAconséllase pechar todas as demais aplicacións antes de continuar.

; *** "Password" wizard page
WizardPassword=Contrasinal
PasswordLabel1=Esta instalación está protexida por contrasinal.
PasswordLabel3=Por favor, introduza o contrasinal e prema en Seguinte para continuar. O contrasinal distingue entre maiúsculas e minúsculas.
PasswordEditLabel=&Contrasinal:
IncorrectPassword=O contrasinal introducido non é correcto. Por favor, ténteo de novo.

; *** "License Agreement" wizard page
WizardLicense=Acordo de licenza
LicenseLabel=É importante que lea a seguinte información antes de continuar.
LicenseLabel3=Por favor, lea o seguinte acordo de licenza. Debe aceptar os termos deste acordo antes de continuar coa instalación.
LicenseAccepted=A&cepto o acordo
LicenseNotAccepted=N&on acepto o acordo

; *** "Information" wizard pages
WizardInfoBefore=Información
InfoBeforeLabel=É importante que lea a seguinte información antes de continuar.
InfoBeforeClickLabel=Cando estea listo para continuar co instalador, prema en Seguinte.
WizardInfoAfter=Información
InfoAfterLabel=É importante que lea a seguinte información antes de continuar.
InfoAfterClickLabel=Cando estea listo para continuar co instalador, prema en Seguinte.

; *** "User Information" wizard page
WizardUserInfo=Información do usuario
UserInfoDesc=Por favor, introduza a súa información.
UserInfoName=&Nome de usuario:
UserInfoOrg=&Organización:
UserInfoSerial=Número de &serie:
UserInfoNameRequired=Debe introducir un nome.

; *** "Select Destination Location" wizard page
WizardSelectDir=Seleccione o cartafol de destino
SelectDirDesc=Onde desexa instalar [name]?
SelectDirLabel3=O instalador instalará [name] no seguinte cartafol.
SelectDirBrowseLabel=Para continuar, prema en Seguinte. Se desexa seleccionar un cartafol diferente, prema en Examinar.
DiskSpaceGBLabel=Requírense polo menos [gb] GB de espazo libre no disco.
DiskSpaceMBLabel=Requírense polo menos [mb] MB de espazo libre no disco.
CannotInstallToNetworkDrive=O instalador non pode realizar a instalación nunha unidade de rede.
CannotInstallToUNCPath=O programa de instalación non pode realizar a instalación nunha ruta de acceso UNC.
InvalidPath=Debe introducir unha ruta completa coa letra da unidade; por exemplo:%n%nC:\APP%n%nou unha ruta UNC co formato:%n%n\\servidor\recurso
InvalidDrive=A unidade ou ruta de acceso UNC que seleccionou non existe ou non está accesible. Por favor, seleccione outra.
DiskSpaceWarningTitle=Espazo insuficiente no disco
DiskSpaceWarning=O instalador require polo menos %1 KB de espazo libre, pero a unidade seleccionada só conta con %2 KB dispoñibles.%n%nDesexa continuar de todos os xeitos?
DirNameTooLong=O nome do cartafol ou a ruta son demasiado longos.
InvalidDirName=O nome do cartafol non é válido.
BadDirName32=Os nomes dos cartafois non poden incluír ningún dos seguintes caracteres:%n%n%1
DirExistsTitle=O cartafol xa existe
DirExists=O cartafol:%n%n%1%n%nxa existe. Desexa realizar a instalación nese cartafol de todos os xeitos?
DirDoesntExistTitle=O cartafol non existe
DirDoesntExist=O cartafol:%n%n%1%n%nnon existe. Desexa que se cree o cartafol?

; *** "Select Components" wizard page
WizardSelectComponents=Seleccione os compoñentes
SelectComponentsDesc=Que compoñentes desexa instalar?
SelectComponentsLabel2=Seleccione os compoñentes que desexa instalar e desmarque os compoñentes que non queira instalar. Prema en Seguinte cando estea listo para continuar.
FullInstallation=Instalación completa
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalación compacta
CustomInstallation=Instalación personalizada
NoUninstallWarningTitle=Compoñentes atopados
NoUninstallWarning=O instalador detectou que os seguintes compoñentes xa están instalados no seu equipo:%n%n%1%n%nDesmarcar estes compoñentes non os desinstalará.%n%nDesexa continuar de todos os xeitos?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=A selección actual require polo menos [gb] GB de espazo libre no disco.
ComponentsDiskSpaceMBLabel=A selección actual require polo menos [mb] MB de espazo libre no disco.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Seleccione as tarefas adicionais
SelectTasksDesc=Que tarefas adicionais desexa facer?
SelectTasksLabel2=Seleccione as tarefas adicionais que desexa facer durante o proceso de instalación de [name] e prema en Seguinte.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Seleccione o cartafol do menú de inicio
SelectStartMenuFolderDesc=Onde desexa que o instalador coloque os accesos directos do programa?
SelectStartMenuFolderLabel3=O instalador creará os accesos directos do programa no seguinte cartafol do menú de inicio.
SelectStartMenuFolderBrowseLabel=Para continuar, prema en Seguinte. Se desexa seleccionar un cartafol diferente, prema en Examinar.
MustEnterGroupName=Poña un nome ao cartafol.
GroupNameTooLong=O nome do cartafol ou a ruta son demasiado longos.
InvalidGroupName=O nome do cartafol non é válido.
BadGroupName=O nome do cartafol non pode ter ningún dos seguintes caracteres:%n%n%1
NoProgramGroupCheck2=&Non crear un cartafol no menú de inicio

; *** "Ready to Install" wizard page
WizardReady=Listo para instalar
ReadyLabel1=Agora o programa está listo para iniciar a instalación de [name] no seu computador.
ReadyLabel2a=Prema en Instalar para continuar, ou en Atrás se desexa revisar ou cambiar a configuración.
ReadyLabel2b=Prema en Instalar para continuar coa instalación.
ReadyMemoUserInfo=Información do usuario:
ReadyMemoDir=Cartafol de destino:
ReadyMemoType=Tipo de instalación:
ReadyMemoComponents=Compoñentes seleccionados:
ReadyMemoGroup=Cartafol do menú de inicio:
ReadyMemoTasks=Tarefas adicionais:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Descargando arquivos...
ButtonStopDownload=&Deter a descarga
StopDownload=Está seguro/a de que desexa deter a descarga?
ErrorDownloadAborted=Descarga cancelada
ErrorDownloadFailed=Fallou a descarga: %1 %2
ErrorDownloadSizeFailed=Fallou a obtención do tamaño: %1 %2
ErrorProgress=Progreso non válido: %1 de %2
ErrorFileSize=Tamaño do arquivo non válido: agardábase %1, atopouse %2

; *** TExtractionWizardPage wizard page and ExtractArchive
ExtractingLabel=Extraendo arquivos...
ButtonStopExtraction=&Deter a extracción
StopExtraction=Está seguro/a de que desexa deter a extracción?
ErrorExtractionAborted=Extracción cancelada
ErrorExtractionFailed=Fallou a extracción: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=O contrasinal é incorrecto
ArchiveIsCorrupted=O arquivo está danado
ArchiveUnsupportedFormat=O formato do arquivo non é compatible

; *** "Preparing to Install" wizard page
WizardPreparing=Preparándose para a instalación
PreparingDesc=O instalador estase preparando para a instalación de [name] no seu computador.
PreviousInstallNotCompleted=A instalación ou desinstalación previa dun programa anterior non se completou. Deberá reiniciar o equipo para completala.%n%nUnha vez reiniciado, execute o instalador de novo para completar a instalación de [name].
CannotContinue=O instalador non pode continuar. Por favor, prema en Cancelar para saír.
ApplicationsFound=As seguintes aplicacións están a usar arquivos que precisan ser actualizados polo instalador. Aconséllase que permita ao instalador pechar automaticamente estas aplicacións.
ApplicationsFound2=As seguintes aplicacións están a usar arquivos que precisan ser actualizados polo instalador. Aconséllase que permita ao instalador pechar automaticamente estas aplicacións. Ao completarse a instalación, o instalador tentará reiniciar as aplicacións.
CloseApplications=&Pechar automaticamente as aplicacións
DontCloseApplications=&Non pechar as aplicacións
ErrorCloseApplications=O instalador non puido pechar de forma automática todas as aplicacións. Aconséllase que, antes de continuar, peche todas as aplicacións que utilicen arquivos que precisan ser actualizados polo instalador.
PrepareToInstallNeedsRestart=O instalador precisa reiniciar o equipo. Unha vez que se teña reiniciado, execute de novo o instalador para completar a instalación de [name].%n%nDesexa reiniciar o equipo agora?


; *** "Installing" wizard page
WizardInstalling=Instalando
InstallingLabel=Por favor, agarde mentres se instala [name] no seu computador.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Completando a instalación de [name]
FinishedLabelNoIcons=O instalador completou a instalación de [name] no seu computador.
FinishedLabel=O instalador completou a instalación de [name] no seu computador. Pode executar a aplicación utilizando os accesos directos creados.
ClickFinish=Prema en Rematar para saír do instalador.
FinishedRestartLabel=Para completar a instalación de [name], o seu computador debe reiniciarse. Desexa reiniciálo agora?
FinishedRestartMessage=Para completar a instalación de [name], o seu computador debe reiniciarse.%n%nDesexa reiniciar agora?
ShowReadmeCheck=Si, desexo ver o arquivo léame (Readme)
YesRadio=&Si, desexo reiniciar o equipo agora
NoRadio=&Non, reiniciarei o equipo máis tarde
; used for example as 'Run MyProg.exe'
RunEntryExec=Executar %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Ver %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=O instalador precisa o seguinte disco
SelectDiskLabel2=Por favor, insira o disco %1 e prema en Aceptar.%n%nSe os arquivos se poden atopar nun cartafol diferente ao que se amosa abaixo, introduza a ruta correcta ou prema en Examinar.
PathLabel=&Ruta:
FileNotInDir2=O arquivo "%1" non se puido atopar en "%2". Por favor, insira o disco correcto ou seleccione outro cartafol.
SelectDirectoryLabel=Por favor, especifique a localización do seguinte disco.

; *** Installation phase messages
SetupAborted=A instalación non se completou.%n%nPor favor, solucione o problema e execute de novo o instalador.
AbortRetryIgnoreSelectAction=Seleccione unha acción
AbortRetryIgnoreRetry=&Tentar de novo
AbortRetryIgnoreIgnore=&Ignorar o erro e continuar
AbortRetryIgnoreCancel=Cancelar a instalación
RetryCancelSelectAction=Seleccione unha acción
RetryCancelRetry=&Tentar de novo
RetryCancelCancel=Cancelar

; *** Installation status messages
StatusClosingApplications=Pechando aplicacións...
StatusCreateDirs=Creando cartafois...
StatusExtractFiles=Extraendo arquivos...
StatusDownloadFiles=Descargando arquivos...
StatusCreateIcons=Creando accesos directos...
StatusCreateIniEntries=Creando entradas INI...
StatusCreateRegistryEntries=Creando entradas do rexistro...
StatusRegisterFiles=Rexistrando arquivos...
StatusSavingUninstall=Gardando a información para desinstalar...
StatusRunProgram=Rematando a instalación...
StatusRestartingApplications=Reiniciando aplicacións...
StatusRollback=Desfacendo os cambios...

; *** Misc. errors
ErrorInternal2=Erro interno: %1
ErrorFunctionFailedNoCode=Fallou %1
ErrorFunctionFailed=Fallou %1; código %2
ErrorFunctionFailedWithMessage=Fallou %1; código %2.%n%3
ErrorExecutingProgram=Non se puido executar o arquivo:%n%1


; *** Registry errors
ErrorRegOpenKey=Erro ao abrir a clave do rexistro:%n%1\%2
ErrorRegCreateKey=Erro ao crear a clave do rexistro:%n%1\%2
ErrorRegWriteKey=Erro ao escribir a clave do rexistro:%n%1\%2


; *** INI errors
ErrorIniEntry=Erro ao escribir na entrada INI no arquivo "%1".


; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Omitir este arquivo (non se aconsella)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignorar o erro e continuar (non se aconsella)
SourceIsCorrupted=O arquivo de orixe está danado
SourceDoesntExist=O arquivo de orixe "%1" non existe
SourceVerificationFailed=Fallou a verificación do arquivo de orixe: %1
VerificationSignatureDoesntExist=Non existe o arquivo de sinatura "%1"
VerificationSignatureInvalid=O arquivo de sinatura "%1" non é válido
VerificationKeyNotFound=O arquivo de sinatura "%1" utiliza unha clave descoñecida
VerificationFileNameIncorrect=O nome do arquivo é incorrecto
VerificationFileTagIncorrect=A etiqueta do arquivo é incorrecta
VerificationFileSizeIncorrect=O tamaño do arquivo é incorrecto
VerificationFileHashIncorrect=O hash do arquivo é incorrecto
ExistingFileReadOnly2=O arquivo existente non se pode substituír porque está marcado como só de lectura.
ExistingFileReadOnlyRetry=&Eliminar o atributo só de lectura e tentar de novo
ExistingFileReadOnlyKeepExisting=&Manter o arquivo existente
ErrorReadingExistingDest=Ocorreu un erro ao tentar ler o arquivo:
FileExistsSelectAction=Seleccione unha acción
FileExists2=O arquivo xa existe.
FileExistsOverwriteExisting=&Substituír o arquivo existente
FileExistsKeepExisting=&Manter o arquivo existente
FileExistsOverwriteOrKeepAll=&Facer o mesmo para os vindeiros conflitos
ExistingFileNewerSelectAction=Seleccione unha acción
ExistingFileNewer2=O arquivo existente é máis recente que o que se está a tentar instalar.
ExistingFileNewerOverwriteExisting=&Substituír o arquivo existente
ExistingFileNewerKeepExisting=&Manter o arquivo existente (aconséllase)
ExistingFileNewerOverwriteOrKeepAll=&Facer o mesmo para os seguintes conflitos
ErrorChangingAttr=Ocorreu un erro ao tentar cambiar os atributos do arquivo:
ErrorCreatingTemp=Ocorreu un erro ao tentar crear un arquivo no cartafol de destino:
ErrorReadingSource=Ocorreu un erro ao tentar ler o arquivo de orixe:
ErrorCopying=Ocorreu un erro ao tentar copiar o arquivo:
ErrorDownloading=Ocorreu un erro ao tentar descargar un arquivo:
ErrorExtracting=Ocorreu un erro ao tentar extraer un arquivo:
ErrorReplacingExistingFile=Ocorreu un erro ao tentar substituír o arquivo existente:
ErrorRestartReplace=Ocorreu un erro ao tentar substituír o arquivo ao reiniciar o equipo:
ErrorRenamingTemp=Ocorreu un erro ao tentar cambiar o nome dun arquivo no cartafol de destino:
ErrorRegisterServer=Non se puido rexistrar o DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 fallou cun código de saída %1
ErrorRegisterTypeLib=Non se puido rexistrar a biblioteca de tipos: %1

; *** Uninstall display name markings
UninstallDisplayNameMark=%1 (%2)
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=Todos os usuarios
UninstallDisplayNameMarkCurrentUser=Usuario actual

; *** Post-installation errors
ErrorOpeningReadme=Ocorreu un erro ao tentar abrir o arquivo léame (README).
ErrorRestartingComputer=O instalador non puido reiniciar o equipo. Por favor, fágao manualmente.

; *** Uninstaller messages
UninstallNotFound=O arquivo "%1" non existe. Non se puido desinstalar.
UninstallOpenError=Non se puido abrir o arquivo "%1". Non se puido desinstalar.
UninstallUnsupportedVer=O arquivo do rexistro para desinstalar "%1" está nun formato non recoñecido por esta versión do desinstalador. Non se puido desinstalar.
UninstallUnknownEntry=Atopouse unha entrada descoñecida (%1) no rexistro de desinstalación.
ConfirmUninstall=Está seguro/a de que desexa desinstalar por completo %1 e todos os seus compoñentes?
UninstallOnlyOnWin64=Este programa só se pode desinstalar en Windows de 64-bits.
OnlyAdminCanUninstall=Este programa só o pode desinstalar un usuario con privilexios de administrador.
UninstallStatusLabel=Por favor, agarde mentres se desinstala %1 do seu computador.
UninstalledAll=Eliminouse %1 con éxito do seu computador.
UninstalledMost=Completouse a desinstalación de %1.%n%nAlgúns elementos non se puideron eliminar, pero poderá eliminalos manualmente se o desexa.
UninstalledAndNeedsRestart=Para completar a desinstalación de %1, o seu computador debe reiniciarse.%n%nDesexa reiniciar agora?
UninstallDataCorrupted=O arquivo "%1" está danado. Non se pode desinstalar.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Eliminar arquivo compartido?
ConfirmDeleteSharedFile2=O sistema indica que o seguinte arquivo compartido non o utiliza ningún outro programa. Desexa eliminar este arquivo compartido?%n%nSe elimina o arquivo e hai programas que o utilizan, eses programas poderían deixar de funcionar correctamente. Se non está seguro/a, escolla Non. Deixar o arquivo no seu sistema non causará ningún dano.
SharedFileNameLabel=Arquivo:
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
CreateQuickLaunchIcon=Crear un acceso directo en &inicio rápido
ProgramOnTheWeb=%1 na Web
UninstallProgram=Desinstalar %1
LaunchProgram=Executar %1
AssocFileExtension=&Asociar %1 coa extensión do arquivo %2
AssocingFileExtension=Asociando %1 coa extensión do arquivo %2...
AutoStartProgramGroupDescription=Inicio:
AutoStartProgram=Iniciar automaticamente %1
AddonHostProgramNotFound=Non se puido localizar %1 no cartafol seleccionado.%n%nDesexa continuar de todos os xeitos?