; *** Inno Setup version 6.1.0+ Brazilian Portuguese messages made by Cesar82 cesar.zanetti.82@gmail.com ***
;
; To download user-contributed translations of this file, go to:
;   https://jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Português Brasileiro
LanguageID=$0416
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
SetupAppTitle=Instalador
SetupWindowTitle=Instalador - %1
UninstallAppTitle=Desinstalar
UninstallAppFullTitle=Desinstalar %1

; *** Misc. common
InformationTitle=Informação
ConfirmTitle=Confirmar
ErrorTitle=Erro

; *** SetupLdr messages
SetupLdrStartupMessage=Isso instalará o %1. Deseja continuar?
LdrCannotCreateTemp=Não foi possível criar um arquivo temporário. Instalação abortada
LdrCannotExecTemp=Não foi possível executar o arquivo no diretório temporário. Instalação abortada
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nErro %2: %3
SetupFileMissing=O arquivo %1 está faltando do diretório de instalação. Por favor, corrija o problema ou obtenha uma nova cópia do programa.
SetupFileCorrupt=Os arquivos de instalação estão corrompidos. Por favor, obtenha uma nova cópia do programa.
SetupFileCorruptOrWrongVer=Os arquivos de instalação estão corrompidos ou são incompatíveis com esta versão do Instalador. Por favor, corrija o problema ou obtenha uma nova cópia do programa.
InvalidParameter=Um parâmetro inválido foi passado na linha de comando:%n%n%1
SetupAlreadyRunning=O instalador já está em execução.
WindowsVersionNotSupported=Este programa não suporta a versão do Windows que seu computador está executando.
WindowsServicePackRequired=Este programa requer o %1 Service Pack %2 ou posterior.
NotOnThisPlatform=Este programa não executará no %1.
OnlyOnThisPlatform=Este programa deve ser executado no %1.
OnlyOnTheseArchitectures=Este programa só pode ser instalado em versões do Windows projetadas para as seguintes arquiteturas de processadores:%n%n% 1
MissingWOW64APIs=A versão do Windows que você está executando não inclui a funcionalidade requerida pelo Instalador para realizar uma instalação de 64 bits. Para corrigir este problema, por favor, instale o Service Pack %1.
WinVersionTooLowError=Este programa requer %1 versão %2 ou posterior.
WinVersionTooHighError=Este programa não pode ser instalado no %1 versão %2 ou posterior.
AdminPrivilegesRequired=Você deve estar logado como administrador ao instalar este programa.
PowerUserPrivilegesRequired=Você deve estar logado como administrador ou como membro do grupo de Usuários Avançados ao instalar este programa.
SetupAppRunningError=O Instalador detectou que o %1 está atualmente em execução.%n%nPor favor, feche todas as instâncias agora e clique em OK para continuar ou Cancelar para sair.
UninstallAppRunningError=O Desinstalador detectou que o %1 está atualmente em execução.%n%nPor favor, feche todas as instâncias agora e clique em OK para continuar ou Cancelar para sair.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Selecionar Modo de Instalação
PrivilegesRequiredOverrideInstruction=Selecionar modo de instalação
PrivilegesRequiredOverrideText1=%1 pode ser instalado para todos os usuários (requer privilégios administrativos) ou somente para você.
PrivilegesRequiredOverrideText2=%1 pode ser instalado somente para você ou para todos os usuários (requer privilégios administrativos).
PrivilegesRequiredOverrideAllUsers=Instalar para &todos os usuários
PrivilegesRequiredOverrideAllUsersRecommended=Instalar para &todos os usuários (recomendado)
PrivilegesRequiredOverrideCurrentUser=Instalar apenas para &mim
PrivilegesRequiredOverrideCurrentUserRecommended=Instalar apenas para &mim (recomendado)

; *** Misc. errors
ErrorCreatingDir=Não foi possível criar o diretório "%1"
ErrorTooManyFilesInDir=Não foi possível criar um arquivo no diretório "%1" porque ele contém muitos arquivos

; *** Setup common messages
ExitSetupTitle=Sair do Instalador
ExitSetupMessage=A instalação não está completa. Se você sair agora, o programa não será instalado.%n%nVocê pode executar o Instalador novamente em outro momento para completar a instalação.%n%nSair do Instalador?
AboutSetupMenuItem=&Sobre o Instalador...
AboutSetupTitle=Sobre o Instalador
AboutSetupMessage=%1 versão %2%n%3%n%nPágina inicial do %1:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< &Voltar
ButtonNext=&Avançar >
ButtonInstall=&Instalar
ButtonOK=OK
ButtonCancel=Cancelar
ButtonYes=&Sim
ButtonYesToAll=Sim para &Todos
ButtonNo=&Não
ButtonNoToAll=Nã&o para Todos
ButtonFinish=&Concluir
ButtonBrowse=&Procurar...
ButtonWizardBrowse=P&rocurar...
ButtonNewFolder=&Criar Nova Pasta

; *** "Select Language" dialog messages
SelectLanguageTitle=Selecionar Idioma do Instalador
SelectLanguageLabel=Selecione o idioma a ser utilizado durante a instalação:

; *** Common wizard text
ClickNext=Clique em Avançar para continuar ou em Cancelar para sair do Instalador.
BeveledLabel=
BrowseDialogTitle=Procurar Pasta
BrowseDialogLabel=Selecione uma pasta na lista abaixo e clique em OK.
NewFolderName=Nova Pasta

; *** "Welcome" wizard page
WelcomeLabel1=Bem-vindo ao Assistente de Instalação do [name]
WelcomeLabel2=Isso instalará o [name/ver] no seu computador.%n%nRecomendamos que você feche todos os outros aplicativos antes de continuar.

; *** "Password" wizard page
WizardPassword=Senha
PasswordLabel1=Esta instalação está protegida por senha.
PasswordLabel3=Por favor, forneça a senha e clique em Avançar para continuar. As senhas diferenciam maiúsculas de minúsculas.
PasswordEditLabel=&Senha:
IncorrectPassword=A senha que você digitou está incorreta. Por favor, tente novamente.

; *** "License Agreement" wizard page
WizardLicense=Contrato de Licença
LicenseLabel=Por favor, leia as seguintes informações importantes antes de continuar.
LicenseLabel3=Leia o Contrato de Licença a seguir. Você deve aceitar os termos deste contrato antes de continuar com a instalação.
LicenseAccepted=Eu &aceito o contrato
LicenseNotAccepted=Eu &não aceito o contrato

; *** "Information" wizard pages
WizardInfoBefore=Informação
InfoBeforeLabel=Por favor, leia as seguintes informações importantes antes de continuar.
InfoBeforeClickLabel=Quando estiver pronto para continuar com a instalação, clique em Avançar.
WizardInfoAfter=Informação
InfoAfterLabel=Por favor, leia as seguintes informações importantes antes de continuar.
InfoAfterClickLabel=Quando estiver pronto para continuar com a instalação, clique em Avançar.

; *** "User Information" wizard page
WizardUserInfo=Informações do Usuário
UserInfoDesc=Por favor, insira suas informações.
UserInfoName=&Nome do Usuário:
UserInfoOrg=&Organização:
UserInfoSerial=&Número de Série:
UserInfoNameRequired=Você deve inserir um nome.

; *** "Select Destination Location" wizard page
WizardSelectDir=Selecionar Local de Instalação
SelectDirDesc=Onde o [name] deve ser instalado?
SelectDirLabel3=O Instalador instalará o [name] na seguinte pasta.
SelectDirBrowseLabel=Para continuar, clique em Avançar. Se desejar selecionar uma pasta diferente, clique em Procurar.
DiskSpaceGBLabel=É necessário pelo menos [gb] GB de espaço livre em disco.
DiskSpaceMBLabel=É necessário pelo menos [mb] MB de espaço livre em disco.
CannotInstallToNetworkDrive=O Instalador não pode instalar em uma unidade de rede.
CannotInstallToUNCPath=O Instalador não pode instalar em um caminho UNC.
InvalidPath=Você deve inserir um caminho completo com letra de unidade; por exemplo:%n%nC:\APP%n%ou um caminho UNC no formato:%n%n\\server\share
InvalidDrive=A unidade ou compartilhamento UNC que você selecionou não existe ou não está acessível. Por favor, selecione outra.
DiskSpaceWarningTitle=Espaço em Disco Insuficiente
DiskSpaceWarning=O Instalador requer pelo menos %1 KB de espaço livre para instalar, mas a unidade selecionada possui apenas %2 KB disponíveis.%n%nDeseja continuar assim mesmo?
DirNameTooLong=O nome da pasta ou caminho é muito longo.
InvalidDirName=O nome da pasta não é válido.
BadDirName32=Os nomes de pastas não podem incluir nenhum dos seguintes caracteres:%n%n%1
DirExistsTitle=Pasta Existe
DirExists=A pasta:%n%n%1%n%já existe. Deseja instalar nessa pasta mesmo assim?
DirDoesntExistTitle=Pasta Não Existe
DirDoesntExist=A pasta:%n%n%1%n%nnão existe. Deseja que a pasta seja criada?

; *** "Select Components" wizard page
WizardSelectComponents=Selecionar Componentes
SelectComponentsDesc=Quais componentes devem ser instalados?
SelectComponentsLabel2=Selecione os componentes que deseja instalar; desmarque os componentes que não deseja instalar. Clique em Avançar quando estiver pronto para continuar.
FullInstallation=Instalação Completa
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalação Compacta
CustomInstallation=Instalação Personalizada
NoUninstallWarningTitle=Componentes Já Existem
NoUninstallWarning=O Instalador detectou que os seguintes componentes já estão instalados no seu computador:%n%n%1%n%nDesmarcar esses componentes não os desinstalará.%n%nDeseja continuar assim mesmo?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=A seleção atual requer pelo menos [gb] GB de espaço em disco.
ComponentsDiskSpaceMBLabel=A seleção atual requer pelo menos [mb] MB de espaço em disco.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Selecionar Tarefas Adicionais
SelectTasksDesc=Quais tarefas adicionais devem ser realizadas?
SelectTasksLabel2=Selecione as tarefas adicionais que deseja que o Instalador execute enquanto instala o [name], e clique em Avançar.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Selecionar Pasta do Menu Iniciar
SelectStartMenuFolderDesc=Onde o Instalador deve colocar os atalhos do programa?
SelectStartMenuFolderLabel3=O Instalador criará os atalhos do programa na seguinte pasta do Menu Iniciar.
SelectStartMenuFolderBrowseLabel=Para continuar, clique em Avançar. Se desejar selecionar uma pasta diferente, clique em Procurar.
MustEnterGroupName=Você deve inserir um nome de pasta.
GroupNameTooLong=O nome da pasta ou caminho é muito longo.
InvalidGroupName=O nome da pasta não é válido.
BadGroupName=O nome da pasta não pode incluir nenhum dos seguintes caracteres:%n%n%1
NoProgramGroupCheck2=&Não criar uma pasta no Menu Iniciar

; *** "Ready to Install" wizard page
WizardReady=Pronto para Instalar
ReadyLabel1=O Instalador está pronto para começar a instalar o [name] no seu computador.
ReadyLabel2a=Clique em Instalar para continuar com a instalação, ou clique em Voltar se quiser revisar ou alterar as configurações.
ReadyLabel2b=Clique em Instalar para continuar com a instalação.
ReadyMemoUserInfo=Informações do usuário:
ReadyMemoDir=Local de instalação:
ReadyMemoType=Tipo de instalação:
ReadyMemoComponents=Componentes selecionados:
ReadyMemoGroup=Pasta do Menu Iniciar:
ReadyMemoTasks=Tarefas adicionais:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel=Baixando arquivos adicionais...
ButtonStopDownload=&Parar download
StopDownload=Tem certeza de que deseja parar o download?
ErrorDownloadAborted=Download abortado
ErrorDownloadFailed=Falha no download: %1 %2
ErrorDownloadSizeFailed=Falha ao obter o tamanho: %1 %2
ErrorFileHash1=Falha na verificação do hash do arquivo: %1
ErrorFileHash2=Hash do arquivo inválido: esperado %1, encontrado %2
ErrorProgress=Progresso inválido: %1 de %2
ErrorFileSize=Tamanho do arquivo inválido: esperado %1, encontrado %2

; *** "Preparing to Install" wizard page
WizardPreparing=Preparando para Instalar
PreparingDesc=O Instalador está se preparando para instalar o [name] no seu computador.
PreviousInstallNotCompleted=A instalação ou remoção de um programa anterior não foi concluída. Você precisará reiniciar o computador para concluir essa instalação.%n%nApós reiniciar o computador, execute o Instalador novamente para concluir a instalação do [name].
CannotContinue=O Instalador não pode continuar. Clique em Cancelar para sair.
ApplicationsFound=Os seguintes aplicativos estão usando arquivos que precisam ser atualizados pelo Instalador. É recomendável permitir que o Instalador feche automaticamente esses aplicativos.
ApplicationsFound2=Os seguintes aplicativos estão usando arquivos que precisam ser atualizados pelo Instalador. É recomendável permitir que o Instalador feche automaticamente esses aplicativos. Após a conclusão da instalação, o Instalador tentará reiniciar os aplicativos.
CloseApplications=&Fechar automaticamente os aplicativos
DontCloseApplications=&Não fechar os aplicativos
ErrorCloseApplications=O Instalador não conseguiu fechar automaticamente todos os aplicativos. É recomendável que você feche todos os aplicativos que estão usando arquivos que precisam ser atualizados pelo Instalador antes de continuar.
PrepareToInstallNeedsRestart=O Instalador deve reiniciar seu computador. Após reiniciar o computador, execute o Instalador novamente para concluir a instalação do [name].%n%nVocê gostaria de reiniciar agora?

; *** "Installing" wizard page
WizardInstalling=Instalando
InstallingLabel=Por favor, aguarde enquanto o Instalador instala o [name] no seu computador.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Concluindo o Assistente de Instalação do [name]
FinishedLabelNoIcons=O Instalador terminou de instalar o [name] no seu computador.
FinishedLabel=O Instalador terminou de instalar o [name] no seu computador. O aplicativo pode ser iniciado selecionando os ícones instalados.
ClickFinish=Clique em Concluir para sair do Instalador.
FinishedRestartLabel=Para concluir a instalação do [name], o Instalador deve reiniciar seu computador. Deseja reiniciar agora?
FinishedRestartMessage=Para concluir a instalação do [name], o Instalador deve reiniciar seu computador.%n%nDeseja reiniciar agora?
ShowReadmeCheck=Sim, eu gostaria de ver o arquivo README
YesRadio=&Sim, reiniciar o computador agora
NoRadio=&Não, eu reiniciarei o computador mais tarde
; used for example as 'Run MyProg.exe'
RunEntryExec=Executar %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Visualizar %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=O Instalador Precisa do Próximo Disco
SelectDiskLabel2=Por favor, insira o Disco %1 e clique em OK.%n%nSe os arquivos deste disco puderem ser encontrados em uma pasta diferente da exibida abaixo, insira o caminho correto ou clique em Procurar.
PathLabel=&Caminho:
FileNotInDir2=O arquivo "%1" não pôde ser localizado em "%2". Por favor, insira o disco correto ou selecione outra pasta.
SelectDirectoryLabel=Por favor, especifique o local do próximo disco.

; *** Installation phase messages
SetupAborted=A instalação não foi concluída.%n%nPor favor, corrija o problema e execute o Instalador novamente.
AbortRetryIgnoreSelectAction=Selecionar ação
AbortRetryIgnoreRetry=&Tentar novamente
AbortRetryIgnoreIgnore=&Ignorar o erro e continuar
AbortRetryIgnoreCancel=Cancelar instalação

; *** Installation status messages
StatusClosingApplications=Fechando aplicativos...
StatusCreateDirs=Criando diretórios...
StatusExtractFiles=Extraindo arquivos...
StatusCreateIcons=Criando atalhos...
StatusCreateIniEntries=Criando entradas INI...
StatusCreateRegistryEntries=Criando entradas no registro...
StatusRegisterFiles=Registrando arquivos...
StatusSavingUninstall=Salvando informações de desinstalação...
StatusRunProgram=Concluindo a instalação...
StatusRestartingApplications=Reiniciando aplicativos...
StatusRollback=Revertendo alterações...

; *** Misc. errors
ErrorInternal2=Erro interno: %1
ErrorFunctionFailedNoCode=%1 falhou
ErrorFunctionFailed=%1 falhou; código %2
ErrorFunctionFailedWithMessage=%1 falhou; código %2.%n%3
ErrorExecutingProgram=Não foi possível executar o arquivo:%n%1

; *** Registry errors
ErrorRegOpenKey=Erro ao abrir a chave do registro:%n%1\%2
ErrorRegCreateKey=Erro ao criar a chave do registro:%n%1\%2
ErrorRegWriteKey=Erro ao escrever na chave do registro:%n%1\%2

; *** INI errors
ErrorIniEntry=Erro ao criar a entrada INI no arquivo "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Ignorar este arquivo (não recomendado)
FileAbortRetryIgnoreIgnoreNotRecommended=&Ignorar o erro e continuar (não recomendado)
SourceIsCorrupted=O arquivo de origem está corrompido
SourceDoesntExist=O arquivo de origem "%1" não existe
ExistingFileReadOnly2=O arquivo existente não pôde ser substituído porque está marcado como somente leitura.
ExistingFileReadOnlyRetry=&Remover o atributo somente leitura e tentar novamente
ExistingFileReadOnlyKeepExisting=&Manter o arquivo existente
ErrorReadingExistingDest=Ocorreu um erro ao tentar ler o arquivo existente:
FileExistsSelectAction=Selecionar ação
FileExists2=O arquivo já existe.
FileExistsOverwriteExisting=&Substituir o arquivo existente
FileExistsKeepExisting=&Manter o arquivo existente
FileExistsOverwriteOrKeepAll=&Fazer isso para os próximos conflitos
ExistingFileNewerSelectAction=Selecionar ação
ExistingFileNewer2=O arquivo existente é mais recente do que o que o Instalador está tentando instalar.
ExistingFileNewerOverwriteExisting=&Substituir o arquivo existente
ExistingFileNewerKeepExisting=&Manter o arquivo existente (recomendado)
ExistingFileNewerOverwriteOrKeepAll=&Fazer isso para os próximos conflitos
ErrorChangingAttr=Ocorreu um erro ao tentar alterar os atributos do arquivo existente:
ErrorCreatingTemp=Ocorreu um erro ao tentar criar um arquivo no diretório de destino:
ErrorReadingSource=Ocorreu um erro ao tentar ler o arquivo de origem:
ErrorCopying=Ocorreu um erro ao tentar copiar um arquivo:
ErrorReplacingExistingFile=Ocorreu um erro ao tentar substituir o arquivo existente:
ErrorRestartReplace=Falha ao reiniciar a substituição:
ErrorRenamingTemp=Ocorreu um erro ao tentar renomear um arquivo no diretório de destino:
ErrorRegisterServer=Não foi possível registrar o DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 falhou com código de saída %1
ErrorRegisterTypeLib=Não foi possível registrar a biblioteca de tipos: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32 bits
UninstallDisplayNameMark64Bit=64 bits
UninstallDisplayNameMarkAllUsers=Todos os usuários
UninstallDisplayNameMarkCurrentUser=Usuário atual

; *** Post-installation errors
ErrorOpeningReadme=Ocorreu um erro ao tentar abrir o arquivo README.
ErrorRestartingComputer=O Instalador não conseguiu reiniciar o computador. Por favor, faça isso manualmente.

; *** Uninstaller messages
UninstallNotFound=O arquivo "%1" não existe. Não é possível desinstalar.
UninstallOpenError=Não foi possível abrir o arquivo "%1". Não é possível desinstalar.
UninstallUnsupportedVer=O arquivo de log de desinstalação "%1" está em um formato não reconhecido por esta versão do desinstalador. Não é possível desinstalar.
UninstallUnknownEntry=Uma entrada desconhecida (%1) foi encontrada no log de desinstalação.
ConfirmUninstall=Você tem certeza de que deseja remover completamente %1 e todos os seus componentes?
UninstallOnlyOnWin64=Esta instalação só pode ser desinstalada em Windows de 64 bits.
OnlyAdminCanUninstall=Esta instalação só pode ser desinstalada por um usuário com privilégios administrativos.
UninstallStatusLabel=Por favor, aguarde enquanto %1 é removido do seu computador.
UninstalledAll=%1 foi removido com sucesso do seu computador.
UninstalledMost=Desinstalação do %1 concluída.%n%nAlguns elementos não puderam ser removidos. Estes podem ser removidos manualmente.
UninstalledAndNeedsRestart=Para concluir a desinstalação de %1, seu computador deve ser reiniciado.%n%nVocê gostaria de reiniciar agora?
UninstallDataCorrupted=O arquivo "%1" está corrompido. Não é possível desinstalar.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Remover Arquivo Compartilhado?
ConfirmDeleteSharedFile2=O sistema indica que o seguinte arquivo compartilhado não está mais em uso por nenhum programa. Você gostaria que a Desinstalação removesse este arquivo compartilhado?%n%nSe algum programa ainda estiver usando este arquivo e ele for removido, esses programas podem não funcionar corretamente. Se você estiver em dúvida, escolha Não. Manter o arquivo em seu sistema não causará nenhum dano.
SharedFileNameLabel=Nome do arquivo:
SharedFileLocationLabel=Localização:
WizardUninstalling=Status da Desinstalação
StatusUninstalling=Desinstalando o %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Instalando %1.
ShutdownBlockReasonUninstallingApp=Desinstalando %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versão %2
AdditionalIcons=Ícones adicionais:
CreateDesktopIcon=Criar um ícone &na área de trabalho
CreateQuickLaunchIcon=Criar um ícone de &Início Rápido
ProgramOnTheWeb=%1 na Web
UninstallProgram=Desinstalar %1
LaunchProgram=Iniciar %1
AssocFileExtension=&Associar %1 com a extensão de arquivo %2
AssocingFileExtension=Associando %1 com a extensão de arquivo %2...
AutoStartProgramGroupDescription=Inicialização:
AutoStartProgram=Iniciar automaticamente o %1
AddonHostProgramNotFound=%1 não pôde ser localizado na pasta que você selecionou.%n%nVocê gostaria de continuar mesmo assim?
