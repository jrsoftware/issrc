; *** Inno Setup version 5.1.11+ Romanian messages ***
; Translator : Alexandru Bogdan Munteanu (muntealb@gmail.com)
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
LanguageName=Rom�n<0103>
LanguageID=$0418
LanguageCodePage=1250
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
SetupAppTitle=Instalare
SetupWindowTitle=Instalare - %1
UninstallAppTitle=Dezinstalare
UninstallAppFullTitle=Dezinstalare %1

; *** Misc. common
InformationTitle=Informa�ii
ConfirmTitle=Confirmare
ErrorTitle=Eroare

; *** SetupLdr messages
SetupLdrStartupMessage=Va fi instalat programul %1. Dori�i s� continua�i?
LdrCannotCreateTemp=Nu se poate crea un fi�ier temporar. Instalare abandonat�
LdrCannotExecTemp=Nu se poate executa un fi�ier din dosarul temporar. Instalare abandonat�

; *** Startup error messages
LastErrorMessage=%1.%n%nEroarea %2: %3
SetupFileMissing=Fi�ierul %1 lipse�te din dosarul de instalare. Corecta�i problema sau face�i rost de o copie nou� a programului.
SetupFileCorrupt=Fi�ierele de instalare s�nt deteriorate. Face�i rost de o copie nou� a programului.
SetupFileCorruptOrWrongVer=Fi�ierele de instalare s�nt deteriorate sau s�nt incompatibile cu aceast� versiune a Instalatorului. Remedia�i problema sau ob�ine�i o copie nou� a programului.
NotOnThisPlatform=Acest program nu va rula pe %1.
OnlyOnThisPlatform=Acest program trebuie s� ruleze pe %1.
OnlyOnTheseArchitectures=Acest program poate fi instalat doar pe versiuni de Windows proiectate pentru urm�toarele arhitecturi de procesor:%n%n%1
MissingWOW64APIs=Versiunea de Windows pe care o rula�i nu include func�ionalitatea cerut� de Instalator pentru a realiza o instalare pe 64-bi�i. Pentru a corecta problema, va trebui s� instala�i Service Pack %1.
WinVersionTooLowError=Acest program necesit� %1 versiunea %2 sau mai nou�.
WinVersionTooHighError=Acest program nu poate fi instalat pe %1 versiunea %2 sau mai nou�.
AdminPrivilegesRequired=Trebuie s� fi�i logat ca Administrator pentru instalarea acestui program.
PowerUserPrivilegesRequired=Trebuie s� fi�i logat ca Administrator sau ca Membru al Grupului de Utilizatori �mputernici�i pentru a instala acest program.
SetupAppRunningError=Instalatorul a detectat c� %1 ruleaz� �n acest moment.%n%n�nchide�i toate instan�ele programului respectiv, apoi clica�i OK pentru a continua sau Anuleaz� pentru a abandona instalarea.
UninstallAppRunningError=Dezinstalatorul a detectat c� %1 ruleaz� �n acest moment.%n%n�nchide�i toate instan�ele programului respectiv, apoi clica�i OK pentru a continua sau Anuleaz� pentru a abandona dezinstalarea.

; *** Misc. errors
ErrorCreatingDir=Instalatorul nu a putut crea dosarul "%1"
ErrorTooManyFilesInDir=Nu se poate crea un fi�ier �n dosarul "%1" din cauz� c� are deja prea multe fi�iere

; *** Setup common messages
ExitSetupTitle=Abandonarea Instal�rii
ExitSetupMessage=Instalarea nu este terminat�. Dac� o abandona�i acum, programul nu va fi instalat.%n%nPute�i s� rula�i Instalatorul din nou alt� dat� pentru a termina instalarea.%n%nAbandona�i Instalarea?
AboutSetupMenuItem=&Despre Instalator...
AboutSetupTitle=Despre Instalator
AboutSetupMessage=%1 versiunea %2%n%3%n%n%1 sit:%n%4
AboutSetupNote=
TranslatorNote=

; *** Buttons
ButtonBack=< �na&poi
ButtonNext=&Continu� >
ButtonInstall=&Instaleaz�
ButtonOK=OK
ButtonCancel=Anuleaz�
ButtonYes=&Da
ButtonYesToAll=Da la &Tot
ButtonNo=&Nu
ButtonNoToAll=N&u la Tot
ButtonFinish=�nchide
ButtonBrowse=&Exploreaz�...
ButtonWizardBrowse=Explo&reaz�...
ButtonNewFolder=Creea&z� Dosar Nou

; *** "Select Language" dialog messages
SelectLanguageTitle=Selectarea Limbii Instalatorului
SelectLanguageLabel=Selecta�i limba folosit� pentru instalare:

; *** Common wizard text
ClickNext=Clica�i Continu� pentru a avansa cu instalarea sau Anuleaz� pentru a o abandona.
BeveledLabel=
BrowseDialogTitle=Explorare dup� Dosar
BrowseDialogLabel=Selecta�i un dosar din lista de mai jos, apoi clica�i OK.
NewFolderName=Dosar Nou

; *** "Welcome" wizard page
WelcomeLabel1=Bun venit la Instalarea [name]
WelcomeLabel2=Programul [name/ver] va fi instalat pe calculator.%n%nEste recomandat s� �nchide�i toate celelalte aplica�ii �nainte de a continua.

; *** "Password" wizard page
WizardPassword=Parol�
PasswordLabel1=Aceast� instalare este protejat� prin parol�.
PasswordLabel3=Completa�i parola, apoi clica�i Continu� pentru a merge mai departe. Se ia �n considerare tipul literelor din parol� (Majuscule/minuscule).
PasswordEditLabel=&Parol�:
IncorrectPassword=Parola pe care a�i introdus-o nu este corect�. Re�ncerca�i.

; *** "License Agreement" wizard page
WizardLicense=Acord de Licen�iere
LicenseLabel=Citi�i informa�iile urm�toare �nainte de a continua, s�nt importante.
LicenseLabel3=Citi�i urm�torul Acord de Licen�iere. Trebuie s� accepta�i termenii acestui acord �nainte de a continua instalarea.
LicenseAccepted=&Accept licen�a
LicenseNotAccepted=&Nu accept licen�a

; *** "Information" wizard pages
WizardInfoBefore=Informa�ii
InfoBeforeLabel=Citi�i informa�iile urm�toare �nainte de a continua, s�nt importante.
InfoBeforeClickLabel=C�nd s�nte�i gata de a trece la Instalare, clica�i Continu�.
WizardInfoAfter=Informa�ii
InfoAfterLabel=Citi�i informa�iile urm�toare �nainte de a continua, s�nt importante.
InfoAfterClickLabel=C�nd s�nte�i gata de a trece la Instalare, clica�i Continu�.

; *** "User Information" wizard page
WizardUserInfo=Informa�ii despre Utilizator
UserInfoDesc=Introduce�i informa�iile solicitate.
UserInfoName=&Utilizator:
UserInfoOrg=&Organiza�ie:
UserInfoSerial=Num�r de &Serie:
UserInfoNameRequired=Trebuie s� introduce�i un nume.

; *** "Select Destination Location" wizard page
WizardSelectDir=Selectarea Locului de Destina�ie
SelectDirDesc=Unde dori�i s� instala�i [name]?
SelectDirLabel3=Instalatorul va pune [name] �n dosarul specificat mai jos.
SelectDirBrowseLabel=Pentru a avansa cu instalarea, clica�i Continu�. Dac� dori�i s� selecta�i un alt dosar, clica�i Exploreaz�.
DiskSpaceMBLabel=Este necesar un spa�iu liber de stocare de cel pu�in [mb] MB.
ToUNCPathname=Instalatorul nu poate realiza instalarea pe o cale �n format UNC. Dac� �ncerca�i s� instala�i �ntr-o re�ea, va trebui s� mapa�i un dispozitiv de re�ea.
InvalidPath=Trebuie s� introduce�i o cale complet�, inclusiv litera dispozitivului; de exemplu:%n%nC:\APP%n%nsau o cale UNC de forma:%n%n\\server\share
InvalidDrive=Dispozitivul sau partajul UNC pe care l-a�i selectat nu exist� sau nu este accesibil. Selecta�i altul.
DiskSpaceWarningTitle=Spa�iu de Stocare Insuficient
DiskSpaceWarning=Instalarea necesit� cel pu�in %1 KB de spa�iu de stocare liber, dar dispozitivul selectat are doar %2 KB liberi.%n%nDori�i s� continua�i oricum?
DirNameTooLong=Numele dosarului sau al c�ii este prea lung.
InvalidDirName=Numele dosarului nu este valid.
BadDirName32=Numele dosarelor nu pot include unul din urm�toarele caractere:%n%n%1
DirExistsTitle=Dosarul Exist�
DirExists=Dosarul:%n%n%1%n%nexist� deja. Dori�i totu�i s� instala�i �n acel dosar?
DirDoesntExistTitle=Dosarul Nu Exist�
DirDoesntExist=Dosarul:%n%n%1%n%nnu exist�. Dori�i ca el s� fie creat?

; *** "Select Components" wizard page
WizardSelectComponents=Selectarea Componentelor
SelectComponentsDesc=Care dintre componente ar trebui instalate?
SelectComponentsLabel2=Selecta�i componentele de instalat; deselecta�i componentele pe care nu dori�i s� le instala�i. Clica�i Continu� pentru a merge mai departe.
FullInstallation=Instalare Complet�
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalare Compact�
CustomInstallation=Instalare Personalizat�
NoUninstallWarningTitle=Componentele Exist�
NoUninstallWarning=Instalatorul a detectat c� urm�toarele componente s�nt deja instalate pe calculator:%n%n%1%n%nDeselectarea lor nu le va dezinstala.%n%nDori�i s� continua�i oricum?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Selec�ia curent� necesit� cel pu�in [mb] MB spa�iu de stocare.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Selectarea Sarcinilor Suplimentare
SelectTasksDesc=Ce sarcini suplimentare ar trebui �ndeplinite?
SelectTasksLabel2=Selecta�i sarcinile suplimentare care ar trebui �ndeplinite �n timpul instal�rii [name], apoi clica�i Continu�.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Selectarea Dosarului din Meniul de Pornire
SelectStartMenuFolderDesc=Unde ar trebui s� fie plasate scurt�turile programului?
SelectStartMenuFolderLabel3=Scurt�turile vor fi plasate �n dosarul specificat mai jos al Meniului de Pornire (Start Menu).
SelectStartMenuFolderBrowseLabel=Pentru a avansa cu instalarea, clica�i Continu�. Dac� dori�i s� selecta�i un alt dosar, clica�i Exploreaz�.
MustEnterGroupName=Trebuie s� introduce�i numele dosarului.
GroupNameTooLong=Numele dosarului sau al c�ii este prea lung.
InvalidGroupName=Numele dosarului nu este valid.
BadGroupName=Numele dosarului nu poate include unul dintre caracterele urm�toarele:%n%n%1
NoProgramGroupCheck2=Nu crea un &dosar �n Meniul de Pornire

; *** "Ready to Install" wizard page
WizardReady=Preg�tit de Instalare
ReadyLabel1=Instalatorul e preg�tit pentru instalarea [name] pe calculator.
ReadyLabel2a=Clica�i Instaleaz� pentru a continua cu instalarea, sau clica�i �napoi dac� dori�i s� revede�i sau s� schimba�i set�rile.
ReadyLabel2b=Clica�i Instaleaz� pentru a continua cu instalarea.
ReadyMemoUserInfo=Info Utilizator:
ReadyMemoDir=Loc de Destina�ie:
ReadyMemoType=Tip de Instalare:
ReadyMemoComponents=Componente Selectate:
ReadyMemoGroup=Dosarul Meniului de Pornire:
ReadyMemoTasks=Sarcini Suplimentare:

; *** "Preparing to Install" wizard page
WizardPreparing=Preg�tire pentru Instalare
PreparingDesc=Instalatorul preg�te�te instalarea [name] pe calculator.
PreviousInstallNotCompleted=Instalarea/dezinstalarea anterioar� a unui program nu a fost terminat�. Va trebui s� reporni�i calculatorul pentru a termina opera�iunea precedent�.%n%nDup� repornirea calculatorului, rula�i Instalatorul din nou pentru a realiza instalarea [name].
CannotContinue=Instalarea nu poate continua. Clica�i Anuleaz� pentru a o �nchide.

; *** "Installing" wizard page
WizardInstalling=Instalare �n Desf�urare
InstallingLabel=A�tepta�i �n timp ce se instaleaz� [name] pe calculator.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Finalizarea Instal�rii [name]
FinishedLabelNoIcons=Instalarea [name] pe calculator a fost terminat�.
FinishedLabel=Instalarea [name] pe calculator a fost terminat�. Aplica�ia poate fi lansat� clic�nd pe iconi�ele instalate.
ClickFinish=Clica�i �nchide pentru a p�r�si Instalatorul.
FinishedRestartLabel=Pentru a termina instalarea [name], trebuie repornit calculatorul. Dori�i s� fie repornit acum?
FinishedRestartMessage=Pentru a termina instalarea [name], trebuie repornit calculatorul.%n%nDori�i s� fie repornit acum?
ShowReadmeCheck=Da, a� dori s� v�d fi�ierul de informare (README)
YesRadio=&Da, reporne�te calculatorul acum
NoRadio=&Nu, voi reporni eu calculatorul mai t�rziu
; used for example as 'Run MyProg.exe'
RunEntryExec=Ruleaz� %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Vezi %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Instalatorul Necesit� Discul Urm�tor
SelectDiskLabel2=Introduce�i Discul %1 �i clica�i OK.%n%nDac� fi�ierele de pe acest disc pot fi g�site �ntr-un alt dosar dec�t cel afi�at mai jos, introduce�i calea corect� sau clica�i Exploreaz�.
PathLabel=&Cale:
FileNotInDir2=Fi�ierul "%1" nu poate fi g�sit �n "%2". Introduce�i discul corect sau selecta�i al dosar.
SelectDirectoryLabel=Specifica�i locul discului urm�tor.

; *** Installation phase messages
SetupAborted=Instalarea nu a fost terminat�.%n%nCorecta�i problema �i rula�i Instalarea din nou.
EntryAbortRetryIgnore=Clica�i Re�ncearc� pentru a �ncerca din nou, Ignor� pentru a continua oricum, sau Abandoneaz� pentru a anula instalarea.

; *** Installation status messages
StatusCreateDirs=Se creeaz� dosarele...
StatusExtractFiles=Se extrag fi�ierele...
StatusCreateIcons=Se creeaz� scurt�turile...
StatusCreateIniEntries=Se creeaz� intr�rile INI...
StatusCreateRegistryEntries=Se creeaz� intr�rile �n registru...
StatusRegisterFiles=Se �nregistreaz� fi�ierele...
StatusSavingUninstall=Se salveaz� informa�iile de dezinstalare...
StatusRunProgram=Se finalizeaz� instalarea...
StatusRollback=Se revine la starea ini�ial�, anul�nd modific�rile f�cute...

; *** Misc. errors
ErrorInternal2=Eroare Intern�: %1
ErrorFunctionFailedNoCode=%1 a e�uat
ErrorFunctionFailed=%1 a e�uat; cod %2
ErrorFunctionFailedWithMessage=%1 a e�uat; cod %2.%n%3
ErrorExecutingProgram=Nu se poate executa fi�ierul:%n%1

; *** Registry errors
ErrorRegOpenKey=Eroare la deschiderea cheii de registru:%n%1\%2
ErrorRegCreateKey=Eroare la crearea cheii de registru:%n%1\%2
ErrorRegWriteKey=Eroare la scrierea �n cheia de registru:%n%1\%2

; *** INI errors
ErrorIniEntry=Eroare la crearea intr�rii INI �n fi�ierul "%1".

; *** File copying errors
FileAbortRetryIgnore=Clica�i Re�ncearc� pentru a �ncerca din nou, Ignor� pentru a s�ri acest fi�ier (nerecomandat), sau Abandoneaz� pentru a anula instalarea.
FileAbortRetryIgnore2=Clica�i Re�ncearc� pentru a �ncerca din nou, Ignor� pentru a continua oricum (nerecomandat), sau Abandoneaz� pentru a anula instalarea.
SourceIsCorrupted=Fi�ierul surs� este deteriorat
SourceDoesntExist=Fi�ierul surs� "%1" nu exist�
ExistingFileReadOnly=Fi�ierul deja existent este marcat doar-citire.%n%nClica�i Re�ncearc� pentru a �nl�tura atributul doar-citire �i a �ncerca din nou, Ignor� pentru a s�ri acest fi�ier, sau Abandoneaz� pentru a anula instalarea.
ErrorReadingExistingDest=A ap�rut o eroare �n timpul citirii fi�ierului deja existent:
FileExists=Fi�ierul exist� deja.%n%Dori�i ca el s� fie suprascris de Instalator?
ExistingFileNewer=Fi�ierul deja existent este mai nou dec�t cel care trebuie instalat. Este recomandat s� �l p�stra�i pe cel existent.%n%nDori�i s� p�stra�i fi�ierul deja existent?
ErrorChangingAttr=A ap�rut o eroare �n timpul schimb�rii atributelor fi�ierului deja existent:
ErrorCreatingTemp=A ap�rut o eroare �n timpul cre�rii fi�ierului �n dosarul de destina�ie:
ErrorReadingSource=A ap�rut o eroare �n timpul citirii fi�ierului surs�:
ErrorCopying=A ap�rut o eroare �n timpul copierii fi�ierului:
ErrorReplacingExistingFile=A ap�rut o eroare �n timpul �nlocuirii fi�ierului deja existent:
ErrorRestartReplace=Repornirea/�nlocuirea a e�uat:
ErrorRenamingTemp=A ap�rut o eroare �n timpul redenumirii fi�ierului din dosarul de destina�ie:
ErrorRegisterServer=Nu se poate �nregistra DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 a e�uat, av�nd codul de ie�ire %1
ErrorRegisterTypeLib=Nu se poate �nregistra biblioteca de tipul: %1

; *** Post-installation errors
ErrorOpeningReadme=A ap�rut o eroare �n timp ce se �ncerca deschiderea fi�ierului de informare (README).
ErrorRestartingComputer=Instalatorul nu a putut reporni calculatorul. Va trebui s�-l reporni�i manual.

; *** Uninstaller messages
UninstallNotFound=Fi�ierul "%1" nu exist�. Dezinstalarea nu poate fi f�cut�.
UninstallOpenError=Fi�ierul "%1" nu poate fi deschis. Dezinstalarea nu poate fi f�cut�
UninstallUnsupportedVer=Fi�ierul "%1" ce con�ine jurnalul de dezinstalare este �ntr-un format nerecunoscut de aceast� versiune a dezinstalatorului. Dezinstalarea nu poate fi f�cut�
UninstallUnknownEntry=A fost �nt�lnit� o intrare necunoscut� (%1) �n jurnalul de dezinstalare
ConfirmUninstall=Sigur dori�i s� �nl�tura�i complet %1 �i componentele sale?
UninstallOnlyOnWin64=Aceast� instalare poate fi dezinstalat� doar pe un sistem Windows 64-bi�i.
OnlyAdminCanUninstall=Aceast� instalare poate fi dezinstalat� doar de c�tre un utilizator cu drepturi de Administrator.
UninstallStatusLabel=A�tepta�i ca %1 s� fie �nl�turat de pe calculator.
UninstalledAll=%1 a fost �nl�turat cu succes de pe calculator.
UninstalledMost=Dezinstalare complet� a %1.%n%nAnumite elemente nu au putut fi �nl�turate. Acestea pot fi �nl�turate manual.
UninstalledAndNeedsRestart=Pentru a termina dezinstalarea %1, calculatorul trebuie repornit.%n%nDori�i s� fie repornit acum?
UninstallDataCorrupted=Fi�ierul "%1" este deteriorat. Dezinstalarea nu poate fi f�cut�

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=�terg Fi�ierul Partajat?
ConfirmDeleteSharedFile2=Sistemul indic� faptul c� fi�ierul partajat urm�tor pare s� nu mai fie folosit de vreun alt program. Dori�i ca Dezinstalatorul s� �tearg� acest fi�ier partajat?%n%nDac� totu�i mai exist� programe care folosesc fi�ierul �i el este �ters, acele programe ar putea s� func�ioneze defectuos. Dac� nu s�nte�i sigur, alege�i Nu. L�sarea fi�ierului �n sistem nu va produce nici o nepl�cere.
SharedFileNameLabel=Nume Fi�ier:
SharedFileLocationLabel=Loc:
WizardUninstalling=Starea Dezinstal�rii
StatusUninstalling=Dezinstalez %1...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versiunea %2
AdditionalIcons=Iconi�e suplimentare:
CreateDesktopIcon=Creeaz� o iconi�� pe &Birou (Desktop)
CreateQuickLaunchIcon=Creeaz� o iconi�� �n Bara de &Lansare Rapid� (Quick Launch)
ProgramOnTheWeb=%1 pe internet
UninstallProgram=Dezinstaleaz� %1
LaunchProgram=Lanseaz� %1
AssocFileExtension=&Asociaz� %1 cu extensia de fi�iere %2
AssocingFileExtension=Asociez %1 cu extensia de fi�iere %2...
