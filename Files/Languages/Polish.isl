; *** Inno Setup version 5.5.3+ Polish messages ***
; Krzysztof Cynarski <krzysztof at cynarski.net>
; Proofreading and corrections:
; �ukasz Abramczuk <lukasz.abramczuk at gmail.com>
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/is3rdparty.php
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; $jrsoftware: issrc/Files/Languages/Polish.isl,v 1.16 2007/03/09 16:56:52 jr Exp $

[LangOptions]
LanguageName=Polski
LanguageID=$0415
LanguageCodePage=1250

[Messages]

; *** Application titles
SetupAppTitle=Instalator
SetupWindowTitle=Instalacja - %1
UninstallAppTitle=Dezinstalacja
UninstallAppFullTitle=Odinstaluj %1

; *** Misc. common
InformationTitle=Informacja
ConfirmTitle=Potwierd�
ErrorTitle=B��d

; *** SetupLdr messages
SetupLdrStartupMessage=Ten program zainstaluje aplikacj� %1. Czy chcesz kontynuowa�?
LdrCannotCreateTemp=Nie mo�na utworzy� pliku tymczasowego. Instalacja przerwana.
LdrCannotExecTemp=Nie mo�na uruchomi� pliku w folderze tymczasowym. Instalacja przerwana.

; *** Startup error messages
LastErrorMessage=%1.%n%nB��d %2: %3
SetupFileMissing=W folderze instalacyjnym brakuje pliku %1.%nProsz� usun�� problem lub uzyska� now� kopi� programu instalacyjnego.
SetupFileCorrupt=Pliki sk�adowe Instalatora s� uszkodzone. Prosz� uzyska� now� kopi� Instalatora od producenta.
SetupFileCorruptOrWrongVer=Pliki sk�adowe instalatora s� uszkodzone lub niezgodne z t� wersj� Instalatora. Prosz� rozwi�za� ten problem lub uzyska� now� kopi� Instalatora od producenta.
InvalidParameter=W linii komend zosta� przekazany nieprawid�owy parametr:%n%n%1
SetupAlreadyRunning=Instalator jest ju� uruchomiony.
WindowsVersionNotSupported=Ta aplikacja nie wspiera aktualnie uruchomionej na Twoim komputerze wersji Windows.
WindowsServicePackRequired=Ta aplikacja wymaga %1 z dodatkiem Service Pack %2 lub p�niejszym.
NotOnThisPlatform=Tej aplikacji nie mo�na uruchomi� w systemie %1.
OnlyOnThisPlatform=Ta aplikacja wymaga systemu %1.
OnlyOnTheseArchitectures=Ta aplikacja mo�e by� uruchomiona tylko w systemie Windows zaprojektowanym na procesory o architekturach:%n%n%1
MissingWOW64APIs=Ta wersja systemu Windows nie zawiera komponent�w niezb�dnych do przeprowadzenia 64-bitowej instalacji. Aby usun�� ten problem, prosz� zainstalowa� Service Pack %1.
WinVersionTooLowError=Ta aplikacja wymaga %1 w wersji %2 lub p�niejszej.
WinVersionTooHighError=Ta aplikacja nie mo�e by� zainstalowany w wersji %2 lub p�niejszej systemu %1.
AdminPrivilegesRequired=Aby przeprowadzi� instalacj� tej aplikacji, U�ytkownik musi by� zalogowany z uprawnieniami administratora.
PowerUserPrivilegesRequired=Aby przeprowadzi� instalacj� tej aplikacji, U�ytkownik musi by� zalogowany z uprawnieniami administratora lub u�ytkownika zaawansowanego.
SetupAppRunningError=Instalator wykry�, i� aplikacja %1 jest aktualnie uruchomiona.%n%nZamknij wszystkie okna tej aplikacji, a nast�pnie wybierz przycisk OK, aby kontynuowa�, lub Anuluj, aby przerwa� instalacj�.
UninstallAppRunningError=Dezinstalator wykry�, i� aplikacja %1 jest aktualnie uruchomiona.%n%nZamknij teraz wszystkie okna tej aplikacji, a nast�pnie wybierz przycisk OK, aby kontynuowa�, lub Anuluj, aby przerwa� dezinstalacj�.

; *** Misc. errors
ErrorCreatingDir=Instalator nie m�g� utworzy� katalogu "%1"
ErrorTooManyFilesInDir=Nie mo�na utworzy� pliku w katalogu %1, poniewa� zawiera on zbyt wiele plik�w

; *** Setup common messages
ExitSetupTitle=Zako�cz instalacj�
ExitSetupMessage=Instalacja nie jest zako�czona. Je�eli przerwiesz j� teraz, aplikacja nie zostanie zainstalowana. Mo�na ponowi� instalacj� p�niej, uruchamiaj�c pakiet Instalatora.%n%nCzy chcesz przerwa� instalacj� ?
AboutSetupMenuItem=&O Instalatorze...
AboutSetupTitle=O Instalatorze
AboutSetupMessage=%1 wersja %2%n%3%n%n Strona domowa %1:%n%4
AboutSetupNote=
TranslatorNote=Wersja Polska: Krzysztof Cynarski%n<krzysztof at cynarski.net>%nKorekta: �ukasz Abramczuk%n<lukasz.abramczuk at gmail.com>

; *** Buttons
ButtonBack=< &Wstecz
ButtonNext=&Dalej >
ButtonInstall=&Instaluj
ButtonOK=OK
ButtonCancel=Anuluj
ButtonYes=&Tak
ButtonYesToAll=Tak na &wszystkie
ButtonNo=&Nie
ButtonNoToAll=N&ie na wszystkie
ButtonFinish=&Zako�cz
ButtonBrowse=&Przegl�daj...
ButtonWizardBrowse=P&rzegl�daj...
ButtonNewFolder=&Utw�rz nowy folder

; *** "Select Language" dialog messages
SelectLanguageTitle=Wybierz j�zyk instalacji
SelectLanguageLabel=Wybierz j�zyk u�ywany podczas instalacji:

; *** Common wizard text
ClickNext=Wybierz przycisk Dalej, aby kontynuowa�, lub Anuluj, aby zako�czy� instalacj�.
BeveledLabel=
BrowseDialogTitle=Wska� folder
BrowseDialogLabel=Wybierz folder z poni�szej listy, a nast�pnie wybierz przycisk OK.
NewFolderName=Nowy folder

; *** "Welcome" wizard page
WelcomeLabel1=Witamy w Instalatorze aplikacji [name].
WelcomeLabel2=Instalator zainstaluje teraz aplikacj� [name/ver] na Twoim komputerze.%n%nZalecane jest zamkni�cie wszystkich innych uruchomionych program�w przed rozpocz�ciem procesu instalacji.

; *** "Password" wizard page
WizardPassword=Has�o
PasswordLabel1=Ta instalacja jest zabezpieczona has�em.
PasswordLabel3=Podaj has�o, a nast�pnie wybierz przycisk Dalej, aby kontynuowa�. W has�ach rozr�niane s� wielkie i ma�e litery.
PasswordEditLabel=&Has�o:
IncorrectPassword=Wprowadzone has�o nie jest prawid�owe. Spr�buj ponownie.

; *** "License Agreement" wizard page
WizardLicense=Umowa Licencyjna
LicenseLabel=Przed kontynuacj� prosz� przeczyta� poni�sze wa�ne informacje.
LicenseLabel3=Prosz� przeczyta� tekst Umowy Licencyjnej. Musisz zgodzi� si� na warunki tej umowy przed kontynuacj� instalacji.
LicenseAccepted=&Akceptuj� warunki umowy
LicenseNotAccepted=&Nie akceptuj� warunk�w umowy

; *** "Information" wizard pages
WizardInfoBefore=Informacja
InfoBeforeLabel=Przed przej�ciem do dalszego etapu instalacji, prosz� przeczyta� poni�sz� informacj�.
InfoBeforeClickLabel=Kiedy b�dziesz gotowy do instalacji, kliknij przycisk Dalej.
WizardInfoAfter=Informacja
InfoAfterLabel=Przed przej�ciem do dalszego etapu instalacji, prosz� przeczyta� poni�sz� informacj�.
InfoAfterClickLabel=Gdy b�dziesz gotowy do zako�czenia instalacji, kliknij przycisk Dalej.

; *** "User Information" wizard page
WizardUserInfo=Dane U�ytkownika
UserInfoDesc=Prosz� poda� swoje dane.
UserInfoName=&Nazwisko:
UserInfoOrg=&Organizacja:
UserInfoSerial=Numer &seryjny:
UserInfoNameRequired=Musisz poda� nazwisko.

; *** "Select Destination Location" wizard page
WizardSelectDir=Wybierz docelow� lokalizacj�
SelectDirDesc=Gdzie ma by� zainstalowana aplikacja [name]?
SelectDirLabel3=Instalator zainstaluje aplikacj� [name] do poni�szego folderu.
SelectDirBrowseLabel=Kliknij przycisk Dalej, aby kontynuowa�. Je�li chcesz okre�li� inny folder, kliknij przycisk Przegl�daj.
DiskSpaceMBLabel=Potrzeba przynajmniej [mb] MB wolnego miejsca na dysku.
CannotInstallToNetworkDrive=Instalator nie mo�e zainstalowa� aplikacji na dysku sieciowym.
CannotInstallToUNCPath=Instalator nie mo�e zainstalowa� aplikacji w �cie�ce UNC.
InvalidPath=Musisz wprowadzi� pe�n� �cie�k� wraz z liter� dysku, np.:%n%nC:\PROGRAM%n%nlub �cie�k� sieciow� (UNC) w formacie:%n%n\\serwer\udzia�
InvalidDrive=Wybrany dysk lub udost�pniony folder sieciowy nie istnieje. Prosz� wybra� inny.
DiskSpaceWarningTitle=Niewystarczaj�ca ilo�� wolnego miejsca na dysku
DiskSpaceWarning=Instalator wymaga co najmniej %1 KB wolnego miejsca na dysku. Wybrany dysk posiada tylko %2 KB dost�pnego miejsca.%n%nCzy pomimo to chcesz kontynuowa�?
DirNameTooLong=Nazwa folderu lub �cie�ki jest za d�uga.
InvalidDirName=Niepoprawna nazwa folderu.
BadDirName32=Nazwa folderu nie mo�e zawiera� �adnego z nast�puj�cych znak�w:%n%n%1
DirExistsTitle=Ten folder ju� istnieje
DirExists=Poni�szy folder ju� istnienie:%n%n%1%n%nCzy pomimo to chcesz zainstalowa� aplikacj� w tym folderze?
DirDoesntExistTitle=Nie ma takiego folderu
DirDoesntExist=Poni�szy folder nie istnieje:%n%n%1%n%nCzy chcesz, aby zosta� utworzony?

; *** "Select Components" wizard page
WizardSelectComponents=Zaznacz komponenty
SelectComponentsDesc=Kt�re komponenty maj� by� zainstalowane?
SelectComponentsLabel2=Zaznacz komponenty, kt�re chcesz zainstalowa�, odznacz te, kt�rych nie chcesz zainstalowa�. Kliknij przycisk Dalej, aby kontynuowa�.
FullInstallation=Instalacja pe�na
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalacja podstawowa
CustomInstallation=Instalacja u�ytkownika
NoUninstallWarningTitle=Zainstalowane komponenty
NoUninstallWarning=Instalator wykry�, �e na Twoim komputerze s� ju� zainstalowane nast�puj�ce komponenty:%n%n%1%n%nOdznaczenie kt�regokolwiek z nich nie spowoduje ich dezinstalacji.%n%nCzy pomimo tego chcesz kontynuowa�?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Wybrane komponenty wymagaj� co najmniej [mb] MB na dysku.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Zaznacz dodatkowe zadania
SelectTasksDesc=Kt�re dodatkowe zadania maj� by� wykonane?
SelectTasksLabel2=Zaznacz dodatkowe zadania, kt�re Instalator ma wykona� podczas instalacji aplikacji [name], a nast�pnie kliknij przycisk Dalej, aby kontynuowa�.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Wybierz folder Menu Start
SelectStartMenuFolderDesc=Gdzie maj� by� umieszczone skr�ty do aplikacji?
SelectStartMenuFolderLabel3=Instalator stworzy skr�ty do aplikacji w poni�szym folderze Menu Start.
SelectStartMenuFolderBrowseLabel=Kliknij przycisk Dalej, aby kontynuowa�. Je�li chcesz okre�li� inny folder, kliknij przycisk Przegl�daj.
MustEnterGroupName=Musisz wprowadzi� nazw� folderu.
GroupNameTooLong=Nazwa folderu lub �cie�ki jest za d�uga.
InvalidGroupName=Niepoprawna nazwa folderu.
BadGroupName=Nazwa folderu nie mo�e zawiera� �adnego z nast�puj�cych znak�w:%n%n%1
NoProgramGroupCheck2=Nie tw�rz folderu w &Menu Start

; *** "Ready to Install" wizard page
WizardReady=Gotowy do rozpocz�cia instalacji
ReadyLabel1=Instalator jest ju� gotowy do rozpocz�cia instalacji aplikacji [name] na Twoim komputerze.
ReadyLabel2a=Kliknij przycisk Instaluj, aby rozpocz�� instalacj� lub Wstecz, je�li chcesz przejrze� lub zmieni� ustawienia.
ReadyLabel2b=Kliknij przycisk Instaluj, aby kontynuowa� instalacj�.
ReadyMemoUserInfo=Informacje u�ytkownika:
ReadyMemoDir=Lokalizacja docelowa:
ReadyMemoType=Rodzaj instalacji:
ReadyMemoComponents=Wybrane komponenty:
ReadyMemoGroup=Folder w Menu Start:
ReadyMemoTasks=Dodatkowe zadania:

; *** "Preparing to Install" wizard page
WizardPreparing=Przygotowanie do instalacji
PreparingDesc=Instalator przygotowuje instalacj� aplikacji [name] na Twoim komputerze.
PreviousInstallNotCompleted=Instalacja (usuni�cie) poprzedniej wersji aplikacji nie zosta�a zako�czona. B�dziesz musia� ponownie uruchomi� komputer, aby zako�czy� instalacj�. %n%nPo ponownym uruchomieniu komputera uruchom ponownie instalatora, aby zako�czy� instalacj� aplikacji [name].
CannotContinue=Instalator nie mo�e kontynuowa�. Kliknij przycisk Anuluj, aby przerwa� instalacj�.
ApplicationsFound=Poni�sze aplikacje u�ywaj� plik�w, kt�re musz� by� uaktualnione przez Instalator. Zalecane jest, aby pozwoli� Instalatorowi automatycznie zamkn�� te aplikacje.
ApplicationsFound2=Poni�sze aplikacje u�ywaj� plik�w, kt�re musz� by� uaktualnione przez Instalator. Zalecane jest, aby pozwoli� Instalatorowi automatycznie zamkn�� te aplikacje. Po zako�czonej instalacji Instalator podejmie pr�b� ich ponownego uruchomienia.
CloseApplications=&Automatycznie zamknij aplikacje
DontCloseApplications=&Nie zamykaj aplikacji
ErrorCloseApplications=Instalator nie by� w stanie zamkn�� automatycznie wszystkich aplikacji. Zalecane jest zamkni�cie wszystkich tych aplikacji, kt�re aktualnie u�ywaj� uaktualnianych przez Instalator plik�w.

; *** "Installing" wizard page
WizardInstalling=Instalacja
InstallingLabel=Poczekaj, a� instalator zainstaluje aplikacj� [name] na Twoim komputerze.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Zako�czono instalacj� aplikacji [name]
FinishedLabelNoIcons=Instalator zako�czy� instalacj� aplikacji [name] na Twoim komputerze.
FinishedLabel=Instalator zako�czy� instalacj� aplikacji [name] na Twoim komputerze. Aplikacja mo�e by� uruchomiona poprzez u�ycie zainstalowanych skr�t�w.
ClickFinish=Kliknij przycisk Zako�cz, aby zako�czy� instalacj�.
FinishedRestartLabel=Aby zako�czy� instalacj� aplikacji [name], Instalator musi ponownie uruchomi� Tw�j komputer. Czy chcesz teraz uruchomi� komputer ponownie?
FinishedRestartMessage=Aby zako�czy� instalacj� aplikacji [name], Instalator musi ponownie uruchomi� Tw�j komputer.%n%nCzy chcesz teraz uruchomi� komputer ponownie?
ShowReadmeCheck=Tak, chc� przeczyta� dodatkowe informacje
YesRadio=&Tak, teraz uruchom ponownie
NoRadio=&Nie, sam zrestartuj� p�niej
; used for example as 'Run MyProg.exe'
RunEntryExec=Uruchom %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Poka� %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Instalator potrzebuje nast�pnego dysku
SelectDiskLabel2=Prosz� w�o�y� dysk %1 i klikn�� przycisk OK.%n%nJe�li wymieniony poni�ej folder nie okre�la po�o�enia plik�w z tego dysku, wprowad� poprawn� �cie�k� lub kliknij przycisk Przegl�daj.
PathLabel=�&cie�ka:
FileNotInDir2=Plik "%1" nie zosta� znaleziony na dysku "%2". Prosz� w�o�y� w�a�ciwy dysk lub wybra� inny folder.
SelectDirectoryLabel=Prosz� okre�li� lokalizacj� nast�pnego dysku.

; *** Installation phase messages
SetupAborted=Instalacja nie zosta�a zako�czona.%n%nProsz� rozwi�za� problem i ponownie rozpocz�� instalacj�.
EntryAbortRetryIgnore=Mo�esz ponowi� nieudan� czynno��, zignorowa� j� (nie zalecane) lub przerwa� instalacj�.

; *** Installation status messages
StatusClosingApplications=Zamykanie aplikacji...
StatusCreateDirs=Tworzenie folder�w...
StatusExtractFiles=Dekompresja plik�w...
StatusCreateIcons=Tworzenie ikon aplikacji...
StatusCreateIniEntries=Tworzenie zapis�w w plikach INI...
StatusCreateRegistryEntries=Tworzenie zapis�w w rejestrze...
StatusRegisterFiles=Rejestracja plik�w...
StatusSavingUninstall=Zachowanie informacji dezinstalatora...
StatusRunProgram=Ko�czenie instalacji...
StatusRestartingApplications=Ponowne uruchamianie aplikacji...
StatusRollback=Cofanie zmian...

; *** Misc. errors
ErrorInternal2=Wewn�trzny b��d: %1
ErrorFunctionFailedNoCode=B��d podczas wykonywania %1
ErrorFunctionFailed=B��d podczas wykonywania %1; kod %2
ErrorFunctionFailedWithMessage=B��d podczas wykonywania %1; kod %2.%n%3
ErrorExecutingProgram=Nie mo�na uruchomi�:%n%1

; *** Registry errors
ErrorRegOpenKey=B��d podczas otwierania klucza rejestru:%n%1\%2
ErrorRegCreateKey=B��d podczas tworzenia klucza rejestru:%n%1\%2
ErrorRegWriteKey=B��d podczas zapisu do klucza rejestru:%n%1\%2

; *** INI errors
ErrorIniEntry=B��d podczas tworzenia pozycji w pliku INI: "%1".

; *** File copying errors
FileAbortRetryIgnore=Mo�esz ponowi� nieudan� czynno��, zignorowa� j�, aby omin�� ten plik (nie zalecane), lub przerwa� instalacj�.
FileAbortRetryIgnore2=Mo�esz ponowi� nieudan� czynno��, zignorowa� j� (nie zalecane) lub przerwa� instalacj�.
SourceIsCorrupted=Plik �r�d�owy jest uszkodzony
SourceDoesntExist=Plik �r�d�owy "%1" nie istnieje
ExistingFileReadOnly=Istniej�cy plik jest oznaczony jako tylko-do-odczytu.%n%nMo�esz ponowi� (aby usun�� oznaczenie), zignorowa� (aby omin�� ten plik) lub przerwa� instalacj�.
ErrorReadingExistingDest=Wyst�pi� b��d podczas pr�by odczytu istniej�cego pliku:
FileExists=Plik ju� istnieje.%n%nCzy chcesz, aby Instalator zamieni� go na nowy?
ExistingFileNewer=Istniej�cy plik jest nowszy ni� ten, kt�ry Instalator pr�buje skopiowa�. Zalecanym jest zachowanie istniej�cego pliku.%n%nCzy chcesz zachowa� istniej�cy plik?
ErrorChangingAttr=Wyst�pi� b��d podczas pr�by zmiany atrybut�w docelowego pliku:
ErrorCreatingTemp=Wyst�pi� b��d podczas pr�by utworzenia pliku w folderze docelowym:
ErrorReadingSource=Wyst�pi� b��d podczas pr�by odczytu pliku �r�d�owego:
ErrorCopying=Wyst�pi� b��d podczas pr�by kopiowania pliku:
ErrorReplacingExistingFile=Wyst�pi� b��d podczas pr�by zamiany istniej�cego pliku:
ErrorRestartReplace=Pr�ba zast�pienia plik�w podczas restartu komputera nie powiod�a si�.
ErrorRenamingTemp=Wyst�pi� b��d podczas pr�by zmiany nazwy pliku w folderze docelowym:
ErrorRegisterServer=Nie mo�na zarejestrowa� DLL/OCX: %1
ErrorRegSvr32Failed=Funkcja RegSvr32 zako�czy�a si� z kodem b��du %1
ErrorRegisterTypeLib=Nie mog� zarejestrowa� biblioteki typ�w: %1

; *** Post-installation errors
ErrorOpeningReadme=Wyst�pi� b��d podczas pr�by otwarcia pliku README.
ErrorRestartingComputer=Instalator nie m�g� zrestartowa� tego komputera. Prosz� zrobi� to samodzielnie.

; *** Uninstaller messages
UninstallNotFound=Plik "%1" nie istnieje. Nie mo�na go odinstalowa�.
UninstallOpenError=Plik "%1" nie m�g� by� otwarty. Nie mo�na odinstalowa�.
UninstallUnsupportedVer=Ta wersja programu dezinstalacyjnego nie rozpoznaje formatu logu dezinstalacji. Nie mo�na odinstalowa�.
UninstallUnknownEntry=W logu dezinstalacji wyst�pi�a nieznana pozycja (%1)
ConfirmUninstall=Czy na pewno chcesz usun�� aplikacj� %1 i wszystkie jej sk�adniki?
UninstallOnlyOnWin64=Ta aplikacja mo�e by� odinstalowana tylko w 64-bitowej wersji systemu Windows.
OnlyAdminCanUninstall=Ta instalacja mo�e by� usuni�ta tylko przez u�ytkownika z prawami administratora.
UninstallStatusLabel=Poczekaj, a� aplikacja %1 zostanie usuni�ta z Twojego komputera.
UninstalledAll=Aplikacja %1 zosta�a usuni�ta z Twojego komputera.
UninstalledMost=Dezinstalacja aplikacji %1 zako�czona.%n%nNiekt�re elementy nie mog�y by� usuni�te. Mo�esz je usun�� r�cznie.
UninstalledAndNeedsRestart=Tw�j komputer musi zosta� ponownie uruchomiony, aby zako�czy� proces dezinstalacji %1.%n%nCzy chcesz teraz ponownie uruchomi� komputer?
UninstallDataCorrupted=Plik "%1" jest uszkodzony. Nie mo�na odinstalowa�.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Usun�� plik wsp�dzielony?
ConfirmDeleteSharedFile2=System wykry�, �e nast�puj�cy plik nie jest ju� u�ywany przez �aden program. Czy chcesz odinstalowa� ten plik wsp�dzielony?%n%nJe�li inne programy nadal u�ywaj� tego pliku, a zostanie on usuni�ty, mog� one przesta� dzia�a� prawid�owo. Je�li nie jeste� pewny, wybierz przycisk Nie. Pozostawienie tego pliku w Twoim systemie nie spowoduje �adnych szk�d.
SharedFileNameLabel=Nazwa pliku:
SharedFileLocationLabel=Po�o�enie:
WizardUninstalling=Stan dezinstalacji
StatusUninstalling=Dezinstalacja %1...

; *** Shutdown block reasons	
ShutdownBlockReasonInstallingApp=Intstalacja %1.
ShutdownBlockReasonUninstallingApp=Dezinstalacja %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 wersja %2
AdditionalIcons=Dodatkowe ikony:
CreateDesktopIcon=Utw�rz ikon� na &pulpicie
CreateQuickLaunchIcon=Utw�rz ikon� na pasku &szybkiego uruchamiania
ProgramOnTheWeb=Strona internetowa aplikacji %1
UninstallProgram=Dezinstalacja aplikacji %1
LaunchProgram=Uruchom aplikacj� %1
AssocFileExtension=&Przypisz aplikacj� %1 do rozszerzenia pliku %2
AssocingFileExtension=Przypisywanie aplikacji %1 do rozszerzenia pliku %2...
AutoStartProgramGroupDescription=Autostart:
AutoStartProgram=Automatycznie uruchamiaj aplikacj� %1
AddonHostProgramNotFound=Aplikacja %1 nie zosta�a znaleziona we wskazanym przez Ciebie folderze.%n%nCzy pomimo tego chcesz kontynuowa�?
