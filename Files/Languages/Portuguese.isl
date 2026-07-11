; *** Inno Setup version 6.5.0+ Portuguese (Portugal) messages ***
;
; Original translation by Nuno Silva (nars@gmx.net)
; Revised and updated to AO90 by BlackSpirits (blackspirits@gmail.com)
; Last modified: 11 July 2026 by BlackSpirits

[LangOptions]
LanguageName=Português (Portugal)
LanguageID=$0816
LanguageCodePage=1252

[Messages]

; *** Application titles
SetupAppTitle=Instalação
SetupWindowTitle=%1 - Instalação
UninstallAppTitle=Desinstalação
UninstallAppFullTitle=%1 - Desinstalação

; *** Misc. common
InformationTitle=Informação
ConfirmTitle=Confirmação
ErrorTitle=Erro

; *** SetupLdr messages
SetupLdrStartupMessage=O Assistente de Instalação irá instalar o programa %1. Deseja continuar?
LdrCannotCreateTemp=Não foi possível criar um ficheiro temporário. Instalação cancelada
LdrCannotExecTemp=Não foi possível executar um ficheiro na pasta temporária. Instalação cancelada
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nErro %2: %3
SetupFileMissing=O ficheiro %1 não foi encontrado na pasta de instalação. Corrija o problema ou obtenha uma nova cópia do programa.
SetupFileCorrupt=Os ficheiros de instalação estão corrompidos. Obtenha uma nova cópia do programa.
SetupFileCorruptOrWrongVer=Os ficheiros de instalação estão corrompidos, ou são incompatíveis com esta versão do Assistente de Instalação. Corrija o problema ou obtenha uma nova cópia do programa.
InvalidParameter=Foi especificado um parâmetro inválido na linha de comandos:%n%n%1
SetupAlreadyRunning=A instalação já está em execução.
WindowsVersionNotSupported=Este programa não suporta a versão do Windows que está a utilizar.
WindowsServicePackRequired=Este programa necessita de %1 Service Pack %2 ou mais recente.
NotOnThisPlatform=Este programa não pode ser executado no %1.
OnlyOnThisPlatform=Este programa deve ser executado no %1.
OnlyOnTheseArchitectures=Este programa só pode ser instalado em versões do Windows preparadas para as seguintes arquiteturas de processador:%n%n%1
WinVersionTooLowError=Este programa necessita do %1 versão %2 ou mais recente.
WinVersionTooHighError=Este programa não pode ser instalado no %1 versão %2 ou mais recente.
AdminPrivilegesRequired=Deve iniciar sessão como administrador para instalar este programa.
PowerUserPrivilegesRequired=Deve iniciar sessão como administrador ou membro do grupo de Utilizadores Avançados para instalar este programa.
SetupAppRunningError=O Assistente de Instalação detetou que o %1 está em execução.%n%nFeche agora todas as instâncias e, em seguida, clique em OK para continuar ou em Cancelar para sair.
UninstallAppRunningError=O Assistente de Desinstalação detetou que o %1 está em execução.%n%nFeche agora todas as instâncias e, em seguida, clique em OK para continuar ou em Cancelar para sair.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Selecione o modo de instalação
PrivilegesRequiredOverrideInstruction=Selecione o modo de instalação
PrivilegesRequiredOverrideText1=%1 pode ser instalado para todos os utilizadores (necessita de privilégios administrativos), ou só para si.
PrivilegesRequiredOverrideText2=%1 pode ser instalado só para si, ou para todos os utilizadores (necessita de privilégios administrativos).
PrivilegesRequiredOverrideAllUsers=Instalar para &todos os utilizadores
PrivilegesRequiredOverrideAllUsersRecommended=Instalar para &todos os utilizadores (recomendado)
PrivilegesRequiredOverrideCurrentUser=Instalar apenas para &mim
PrivilegesRequiredOverrideCurrentUserRecommended=Instalar apenas para &mim (recomendado)

; *** Misc. errors
ErrorCreatingDir=O Assistente de Instalação não conseguiu criar a pasta "%1"
ErrorTooManyFilesInDir=Não é possível criar um ficheiro na pasta "%1" porque esta contém demasiados ficheiros

; *** Setup common messages
ExitSetupTitle=Terminar a instalação
ExitSetupMessage=A instalação não está completa. Se terminar agora, o programa não será instalado.%n%nMais tarde poderá executar novamente este Assistente de Instalação e concluir a instalação.%n%nDeseja terminar a instalação?
AboutSetupMenuItem=&Acerca do Assistente de Instalação...
AboutSetupTitle=Acerca do Assistente de Instalação
AboutSetupMessage=%1 versão %2%n%3%n%nSite do %1:%n%4
AboutSetupNote=
TranslatorNote=Tradução para português (Portugal) por Nuno Silva (nars@gmx.net) e BlackSpirits (blackspirits@gmail.com)

; *** Buttons
ButtonBack=< &Anterior
ButtonNext=&Seguinte >
ButtonInstall=&Instalar
ButtonOK=OK
ButtonCancel=Cancelar
ButtonYes=&Sim
ButtonYesToAll=Sim para &todos
ButtonNo=&Não
ButtonNoToAll=Nã&o para todos
ButtonFinish=&Concluir
ButtonBrowse=&Procurar...
ButtonWizardBrowse=P&rocurar...
ButtonNewFolder=&Criar nova pasta

; *** "Select Language" dialog messages
SelectLanguageTitle=Selecione o idioma do Assistente de Instalação
SelectLanguageLabel=Selecione o idioma a utilizar durante a instalação.

; *** Common wizard text
ClickNext=Clique em Seguinte para continuar ou em Cancelar para cancelar a instalação.
BeveledLabel=
BrowseDialogTitle=Procurar pasta
BrowseDialogLabel=Selecione uma pasta na lista abaixo e clique em OK.
NewFolderName=Nova pasta

; *** "Welcome" wizard page
WelcomeLabel1=Bem-vindo ao Assistente de Instalação do [name]
WelcomeLabel2=O Assistente de Instalação irá instalar o [name/ver] no seu computador.%n%nÉ recomendado que feche todas as outras aplicações antes de continuar.

; *** "Password" wizard page
WizardPassword=Palavra-passe
PasswordLabel1=Esta instalação está protegida por palavra-passe.
PasswordLabel3=Introduza a palavra-passe e, em seguida, clique em Seguinte para continuar. A palavra-passe distingue maiúsculas de minúsculas.
PasswordEditLabel=&Palavra-passe:
IncorrectPassword=A palavra-passe que introduziu não está correta. Tente novamente.

; *** "License Agreement" wizard page
WizardLicense=Contrato de licença
LicenseLabel=É importante que leia as seguintes informações antes de continuar.
LicenseLabel3=Leia atentamente o seguinte contrato de licença. Deve aceitar os termos do contrato antes de continuar a instalação.
LicenseAccepted=A&ceito o contrato
LicenseNotAccepted=&Não aceito o contrato

; *** "Information" wizard pages
WizardInfoBefore=Informação
InfoBeforeLabel=É importante que leia as seguintes informações antes de continuar.
InfoBeforeClickLabel=Quando estiver pronto para continuar, clique em Seguinte.
WizardInfoAfter=Informação
InfoAfterLabel=É importante que leia as seguintes informações antes de continuar.
InfoAfterClickLabel=Quando estiver pronto para continuar, clique em Seguinte.

; *** "User Information" wizard page
WizardUserInfo=Informações do utilizador
UserInfoDesc=Introduza as suas informações.
UserInfoName=Nome do &utilizador:
UserInfoOrg=&Organização:
UserInfoSerial=&Número de série:
UserInfoNameRequired=Deve introduzir um nome.

; *** "Select Destination Location" wizard page
WizardSelectDir=Selecione a localização de destino
SelectDirDesc=Onde deverá ser instalado o [name]?
SelectDirLabel3=O [name] será instalado na seguinte pasta.
SelectDirBrowseLabel=Para continuar, clique em Seguinte. Se pretender selecionar uma pasta diferente, clique em Procurar.
DiskSpaceGBLabel=É necessário ter pelo menos [gb] GB de espaço livre em disco.
DiskSpaceMBLabel=É necessário ter pelo menos [mb] MB de espaço livre em disco.
CannotInstallToNetworkDrive=O Assistente de Instalação não pode instalar numa unidade de rede.
CannotInstallToUNCPath=O Assistente de Instalação não pode instalar num caminho UNC.
InvalidPath=É necessário indicar o caminho completo com a letra de unidade; por exemplo:%n%nC:\App%n%nou um caminho UNC no formato:%n%n\\servidor\partilha
InvalidDrive=A unidade ou partilha UNC selecionada não existe ou não está acessível. Selecione outra.
DiskSpaceWarningTitle=Não há espaço suficiente no disco
DiskSpaceWarning=O Assistente de Instalação necessita de pelo menos %1 KB de espaço livre, mas a unidade selecionada tem apenas %2 KB disponíveis.%n%nDeseja continuar de qualquer forma?
DirNameTooLong=O nome ou caminho para a pasta é demasiado longo.
InvalidDirName=O nome da pasta não é válido.
BadDirName32=O nome da pasta não pode conter nenhum dos seguintes carateres:%n%n%1
DirExistsTitle=A pasta já existe
DirExists=A pasta:%n%n%1%n%njá existe. Deseja instalar nessa pasta de qualquer forma?
DirDoesntExistTitle=A pasta não existe
DirDoesntExist=A pasta:%n%n%1%n%nnão existe. Deseja que esta pasta seja criada?

; *** "Select Components" wizard page
WizardSelectComponents=Selecione os componentes
SelectComponentsDesc=Que componentes deverão ser instalados?
SelectComponentsLabel2=Selecione os componentes que pretende instalar e desmarque os componentes que não pretende instalar. Clique em Seguinte quando estiver pronto para continuar.
FullInstallation=Instalação completa
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalação compacta
CustomInstallation=Instalação personalizada
NoUninstallWarningTitle=Componentes encontrados
NoUninstallWarning=O Assistente de Instalação detetou que os seguintes componentes já estão instalados no seu computador:%n%n%1%n%nDesmarcar estes componentes não irá desinstalá-los.%n%nDeseja continuar de qualquer forma?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=A seleção atual necessita de pelo menos [gb] GB de espaço em disco.
ComponentsDiskSpaceMBLabel=A seleção atual necessita de pelo menos [mb] MB de espaço em disco.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Selecione tarefas adicionais
SelectTasksDesc=Que tarefas adicionais deverão ser executadas?
SelectTasksLabel2=Selecione as tarefas adicionais a executar pelo Assistente de Instalação durante a instalação do [name] e, em seguida, clique em Seguinte.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Selecione a pasta do Menu Iniciar
SelectStartMenuFolderDesc=Onde deverão ser colocados os atalhos do programa?
SelectStartMenuFolderLabel3=Os atalhos do programa serão criados na seguinte pasta do Menu Iniciar.
SelectStartMenuFolderBrowseLabel=Para continuar, clique em Seguinte. Se pretender selecionar uma pasta diferente, clique em Procurar.
MustEnterGroupName=É necessário introduzir um nome para a pasta.
GroupNameTooLong=O nome ou caminho para a pasta é demasiado longo.
InvalidGroupName=O nome da pasta não é válido.
BadGroupName=O nome da pasta não pode conter nenhum dos seguintes carateres:%n%n%1
NoProgramGroupCheck2=&Não criar uma pasta no Menu Iniciar

; *** "Ready to Install" wizard page
WizardReady=Pronto para instalar
ReadyLabel1=O Assistente de Instalação está agora pronto para começar a instalar o [name] no seu computador.
ReadyLabel2a=Clique em Instalar para continuar a instalação ou em Anterior se desejar rever ou alterar alguma definição.
ReadyLabel2b=Clique em Instalar para continuar a instalação.
ReadyMemoUserInfo=Informações do utilizador:
ReadyMemoDir=Localização de destino:
ReadyMemoType=Tipo de instalação:
ReadyMemoComponents=Componentes selecionados:
ReadyMemoGroup=Pasta do Menu Iniciar:
ReadyMemoTasks=Tarefas adicionais:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=A transferir ficheiros...
ButtonStopDownload=&Parar transferência
StopDownload=Tem a certeza de que pretende parar a transferência?
ErrorDownloadAborted=Transferência cancelada
ErrorDownloadFailed=Falha na transferência: %1 %2
ErrorDownloadSizeFailed=Falha ao obter o tamanho: %1 %2
ErrorProgress=Progresso inválido: %1 de %2
ErrorFileSize=Tamanho de ficheiro inválido: esperado %1, encontrado %2

; *** TExtractionWizardPage wizard page and ExtractArchive
ExtractingLabel=A extrair ficheiros...
ButtonStopExtraction=&Parar extração
StopExtraction=Tem a certeza de que pretende parar a extração?
ErrorExtractionAborted=Extração cancelada
ErrorExtractionFailed=Falha na extração: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=A palavra-passe está incorreta
ArchiveIsCorrupted=O ficheiro comprimido está corrompido
ArchiveUnsupportedFormat=Formato de ficheiro comprimido não suportado

; *** "Preparing to Install" wizard page
WizardPreparing=A preparar a instalação
PreparingDesc=A preparar a instalação do [name] no seu computador.
PreviousInstallNotCompleted=A instalação/desinstalação de um programa anterior não foi concluída. Terá de reiniciar o computador para concluir essa instalação.%n%nDepois de reiniciar o computador, execute novamente este Assistente de Instalação para concluir a instalação do [name].
CannotContinue=A instalação não pode continuar. Clique em Cancelar para sair.
ApplicationsFound=As seguintes aplicações estão a utilizar ficheiros que necessitam de ser atualizados pelo Assistente de Instalação. Recomenda-se que permita que o Assistente de Instalação feche automaticamente estas aplicações.
ApplicationsFound2=As seguintes aplicações estão a utilizar ficheiros que necessitam de ser atualizados pelo Assistente de Instalação. Recomenda-se que permita que o Assistente de Instalação feche automaticamente estas aplicações. Depois de concluir a instalação, o Assistente de Instalação tentará reiniciar as aplicações.
CloseApplications=&Fechar as aplicações automaticamente
DontCloseApplications=&Não fechar as aplicações
ErrorCloseApplications=O Assistente de Instalação não conseguiu fechar automaticamente todas as aplicações. Antes de continuar, recomenda-se que feche todas as aplicações que utilizem ficheiros que necessitem de ser atualizados pelo Assistente de Instalação.
PrepareToInstallNeedsRestart=O Assistente de Instalação tem de reiniciar o seu computador. Depois de reiniciar o computador, execute novamente o Assistente de Instalação para concluir a instalação do [name].%n%nDeseja reiniciar agora?

; *** "Installing" wizard page
WizardInstalling=A instalar
InstallingLabel=Aguarde enquanto o Assistente de Instalação instala o [name] no seu computador.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=A concluir o Assistente de Instalação do [name]
FinishedLabelNoIcons=O Assistente de Instalação concluiu a instalação do [name] no seu computador.
FinishedLabel=O Assistente de Instalação concluiu a instalação do [name] no seu computador. A aplicação pode ser iniciada através dos atalhos instalados.
ClickFinish=Clique em Concluir para sair do Assistente de Instalação.
FinishedRestartLabel=Para concluir a instalação do [name], o Assistente de Instalação tem de reiniciar o seu computador. Deseja reiniciar agora?
FinishedRestartMessage=Para concluir a instalação do [name], o Assistente de Instalação tem de reiniciar o seu computador.%n%nDeseja reiniciar agora?
ShowReadmeCheck=Sim, desejo ver o ficheiro README
YesRadio=&Sim, desejo reiniciar o computador agora
NoRadio=&Não, desejo reiniciar o computador mais tarde
; used for example as 'Run MyProg.exe'
RunEntryExec=Executar %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Visualizar %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=O Assistente de Instalação precisa do disco seguinte
SelectDiskLabel2=Introduza o disco %1 e clique em OK.%n%nSe os ficheiros deste disco estiverem num local diferente do mostrado abaixo, indique o caminho correto ou clique em Procurar.
PathLabel=&Caminho:
FileNotInDir2=O ficheiro "%1" não foi encontrado em "%2". Introduza o disco correto ou selecione outra pasta.
SelectDirectoryLabel=Indique a localização do disco seguinte.

; *** Installation phase messages
SetupAborted=A instalação não foi concluída.%n%nCorrija o problema e execute novamente o Assistente de Instalação.
AbortRetryIgnoreSelectAction=Selecione uma ação
AbortRetryIgnoreRetry=&Tentar novamente
AbortRetryIgnoreIgnore=&Ignorar o erro e continuar
AbortRetryIgnoreCancel=Cancelar a instalação
RetryCancelSelectAction=Selecione uma ação
RetryCancelRetry=&Tentar novamente
RetryCancelCancel=Cancelar

; *** Installation status messages
StatusClosingApplications=A fechar aplicações...
StatusCreateDirs=A criar pastas...
StatusExtractFiles=A extrair ficheiros...
StatusDownloadFiles=A transferir ficheiros...
StatusCreateIcons=A criar atalhos...
StatusCreateIniEntries=A criar entradas INI...
StatusCreateRegistryEntries=A criar entradas no registo...
StatusRegisterFiles=A registar ficheiros...
StatusSavingUninstall=A guardar informações para desinstalação...
StatusRunProgram=A concluir a instalação...
StatusRestartingApplications=A reiniciar aplicações...
StatusRollback=A reverter as alterações...

; *** Misc. errors
ErrorInternal2=Erro interno: %1
ErrorFunctionFailedNoCode=%1 falhou
ErrorFunctionFailed=%1 falhou; código %2
ErrorFunctionFailedWithMessage=%1 falhou; código %2.%n%3
ErrorExecutingProgram=Não é possível executar o ficheiro:%n%1

; *** Registry errors
ErrorRegOpenKey=Erro ao abrir a chave de registo:%n%1\%2
ErrorRegCreateKey=Erro ao criar a chave de registo:%n%1\%2
ErrorRegWriteKey=Erro ao escrever na chave de registo:%n%1\%2

; *** INI errors
ErrorIniEntry=Erro ao criar uma entrada INI no ficheiro "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Omitir este ficheiro (não recomendado)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignorar o erro e continuar (não recomendado)
SourceIsCorrupted=O ficheiro de origem está corrompido
SourceDoesntExist=O ficheiro de origem "%1" não existe
SourceVerificationFailed=A verificação do ficheiro de origem falhou: %1
VerificationSignatureDoesntExist=O ficheiro de assinatura "%1" não existe
VerificationSignatureInvalid=O ficheiro de assinatura "%1" é inválido
VerificationKeyNotFound=O ficheiro de assinatura "%1" utiliza uma chave desconhecida
VerificationFileNameIncorrect=O nome do ficheiro está incorreto
VerificationFileTagIncorrect=A etiqueta do ficheiro está incorreta
VerificationFileSizeIncorrect=O tamanho do ficheiro está incorreto
VerificationFileHashIncorrect=O hash do ficheiro está incorreto
ExistingFileReadOnly2=O ficheiro existente não pode ser substituído porque tem o atributo "só de leitura".
ExistingFileReadOnlyRetry=&Remover o atributo "só de leitura" e tentar novamente
ExistingFileReadOnlyKeepExisting=&Manter o ficheiro existente
ErrorReadingExistingDest=Ocorreu um erro ao tentar ler o ficheiro existente:
FileExistsSelectAction=Selecione uma ação
FileExists2=O ficheiro já existe.
FileExistsOverwriteExisting=&Substituir o ficheiro existente
FileExistsKeepExisting=&Manter o ficheiro existente
FileExistsOverwriteOrKeepAll=&Aplicar esta ação aos próximos conflitos
ExistingFileNewerSelectAction=Selecione uma ação
ExistingFileNewer2=O ficheiro existente é mais recente do que aquele que o Assistente de Instalação está a tentar instalar.
ExistingFileNewerOverwriteExisting=&Substituir o ficheiro existente
ExistingFileNewerKeepExisting=&Manter o ficheiro existente (recomendado)
ExistingFileNewerOverwriteOrKeepAll=&Aplicar esta ação aos próximos conflitos
ErrorChangingAttr=Ocorreu um erro ao tentar alterar os atributos do ficheiro existente:
ErrorCreatingTemp=Ocorreu um erro ao tentar criar um ficheiro na pasta de destino:
ErrorReadingSource=Ocorreu um erro ao tentar ler o ficheiro de origem:
ErrorCopying=Ocorreu um erro ao tentar copiar um ficheiro:
ErrorDownloading=Ocorreu um erro ao tentar transferir um ficheiro:
ErrorExtracting=Ocorreu um erro ao tentar extrair um ficheiro comprimido:
ErrorReplacingExistingFile=Ocorreu um erro ao tentar substituir o ficheiro existente:
ErrorRestartReplace=RestartReplace falhou:
ErrorRenamingTemp=Ocorreu um erro ao tentar mudar o nome de um ficheiro na pasta de destino:
ErrorRegisterServer=Não foi possível registar o ficheiro DLL/OCX: %1
ErrorRegSvr32Failed=O RegSvr32 falhou com o código de saída %1
ErrorRegisterTypeLib=Não foi possível registar a biblioteca de tipos: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32 bits
UninstallDisplayNameMark64Bit=64 bits
UninstallDisplayNameMarkAllUsers=Todos os utilizadores
UninstallDisplayNameMarkCurrentUser=Utilizador atual

; *** Post-installation errors
ErrorOpeningReadme=Ocorreu um erro ao tentar abrir o ficheiro README.
ErrorRestartingComputer=O Assistente de Instalação não conseguiu reiniciar o computador. Reinicie-o manualmente.

; *** Uninstaller messages
UninstallNotFound=O ficheiro "%1" não existe. Não é possível desinstalar.
UninstallOpenError=Não foi possível abrir o ficheiro "%1". Não é possível desinstalar
UninstallUnsupportedVer=O ficheiro de registo de desinstalação "%1" está num formato que não é reconhecido por esta versão do desinstalador. Não é possível desinstalar
UninstallUnknownEntry=Foi encontrada uma entrada desconhecida (%1) no ficheiro de registo de desinstalação
ConfirmUninstall=Tem a certeza de que deseja remover completamente o %1 e todos os seus componentes?
UninstallOnlyOnWin64=Esta desinstalação só pode ser realizada na versão de 64 bits do Windows.
OnlyAdminCanUninstall=Esta desinstalação só pode ser realizada por um utilizador com privilégios administrativos.
UninstallStatusLabel=Aguarde enquanto o %1 é removido do seu computador.
UninstalledAll=O %1 foi removido do seu computador com sucesso.
UninstalledMost=A desinstalação do %1 está concluída.%n%nAlguns elementos não puderam ser removidos. Estes elementos podem ser removidos manualmente.
UninstalledAndNeedsRestart=Para concluir a desinstalação do %1, o computador tem de ser reiniciado.%n%nDeseja reiniciar agora?
UninstallDataCorrupted=O ficheiro "%1" está corrompido. Não é possível desinstalar

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Remover ficheiro partilhado?
ConfirmDeleteSharedFile2=O sistema indica que o seguinte ficheiro partilhado já não está a ser utilizado por nenhum programa. Deseja que o Assistente de Desinstalação remova este ficheiro partilhado?%n%nSe algum programa ainda estiver a utilizar este ficheiro e este for removido, esse programa poderá não funcionar corretamente. Se não tiver a certeza, selecione Não. Manter o ficheiro no sistema não causará qualquer problema.
SharedFileNameLabel=Nome do ficheiro:
SharedFileLocationLabel=Localização:
WizardUninstalling=Estado da desinstalação
StatusUninstalling=A desinstalar o %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=A instalar %1.
ShutdownBlockReasonUninstallingApp=A desinstalar %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versão %2
AdditionalIcons=Atalhos adicionais:
CreateDesktopIcon=Criar um atalho no &Ambiente de Trabalho
CreateQuickLaunchIcon=&Criar atalho na barra de Iniciação Rápida
ProgramOnTheWeb=%1 na Web
UninstallProgram=Desinstalar o %1
LaunchProgram=Executar o %1
AssocFileExtension=Associa&r o %1 aos ficheiros com a extensão %2
AssocingFileExtension=A associar o %1 aos ficheiros com a extensão %2...
AutoStartProgramGroupDescription=Arranque:
AutoStartProgram=Iniciar %1 automaticamente
AddonHostProgramNotFound=Não foi possível localizar %1 na pasta selecionada.%n%nDeseja continuar de qualquer forma?
