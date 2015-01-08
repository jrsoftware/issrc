; *** Inno Setup version 5.5.3+ Polish messages ***
; Krzysztof Cynarski <krzysztof at cynarski.net>
; Proofreading and corrections:
; Łukasz Abramczuk <lukasz.abramczuk at gmail.com>
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
ConfirmTitle=Potwierdź
ErrorTitle=Błąd

; *** SetupLdr messages
SetupLdrStartupMessage=Ten program zainstaluje aplikację %1. Czy chcesz kontynuować?
LdrCannotCreateTemp=Nie można utworzyć pliku tymczasowego. Instalacja przerwana.
LdrCannotExecTemp=Nie można uruchomić pliku w folderze tymczasowym. Instalacja przerwana.

; *** Startup error messages
LastErrorMessage=%1.%n%nBłąd %2: %3
SetupFileMissing=W folderze instalacyjnym brakuje pliku %1.%nProszę usunąć problem lub uzyskać nową kopię programu instalacyjnego.
SetupFileCorrupt=Pliki składowe Instalatora są uszkodzone. Proszę uzyskać nową kopię Instalatora od producenta.
SetupFileCorruptOrWrongVer=Pliki składowe instalatora są uszkodzone lub niezgodne z tą wersją Instalatora. Proszę rozwiązać ten problem lub uzyskać nową kopię Instalatora od producenta.
InvalidParameter=W linii komend został przekazany nieprawidłowy parametr:%n%n%1
SetupAlreadyRunning=Instalator jest już uruchomiony.
WindowsVersionNotSupported=Ta aplikacja nie wspiera aktualnie uruchomionej na Twoim komputerze wersji Windows.
WindowsServicePackRequired=Ta aplikacja wymaga %1 z dodatkiem Service Pack %2 lub późniejszym.
NotOnThisPlatform=Tej aplikacji nie można uruchomić w systemie %1.
OnlyOnThisPlatform=Ta aplikacja wymaga systemu %1.
OnlyOnTheseArchitectures=Ta aplikacja może być uruchomiona tylko w systemie Windows zaprojektowanym na procesory o architekturach:%n%n%1
MissingWOW64APIs=Ta wersja systemu Windows nie zawiera komponentów niezbędnych do przeprowadzenia 64-bitowej instalacji. Aby usunąć ten problem, proszę zainstalować Service Pack %1.
WinVersionTooLowError=Ta aplikacja wymaga %1 w wersji %2 lub późniejszej.
WinVersionTooHighError=Ta aplikacja nie może być zainstalowany w wersji %2 lub późniejszej systemu %1.
AdminPrivilegesRequired=Aby przeprowadzić instalację tej aplikacji, Użytkownik musi być zalogowany z uprawnieniami administratora.
PowerUserPrivilegesRequired=Aby przeprowadzić instalację tej aplikacji, Użytkownik musi być zalogowany z uprawnieniami administratora lub użytkownika zaawansowanego.
SetupAppRunningError=Instalator wykrył, iż aplikacja %1 jest aktualnie uruchomiona.%n%nZamknij wszystkie okna tej aplikacji, a następnie wybierz przycisk OK, aby kontynuować, lub Anuluj, aby przerwać instalację.
UninstallAppRunningError=Dezinstalator wykrył, iż aplikacja %1 jest aktualnie uruchomiona.%n%nZamknij teraz wszystkie okna tej aplikacji, a następnie wybierz przycisk OK, aby kontynuować, lub Anuluj, aby przerwać dezinstalację.

; *** Misc. errors
ErrorCreatingDir=Instalator nie mógł utworzyć katalogu "%1"
ErrorTooManyFilesInDir=Nie można utworzyć pliku w katalogu %1, ponieważ zawiera on zbyt wiele plików

; *** Setup common messages
ExitSetupTitle=Zakończ instalację
ExitSetupMessage=Instalacja nie jest zakończona. Jeżeli przerwiesz ją teraz, aplikacja nie zostanie zainstalowana. Można ponowić instalację później, uruchamiając pakiet Instalatora.%n%nCzy chcesz przerwać instalację ?
AboutSetupMenuItem=&O Instalatorze...
AboutSetupTitle=O Instalatorze
AboutSetupMessage=%1 wersja %2%n%3%n%n Strona domowa %1:%n%4
AboutSetupNote=
TranslatorNote=Wersja Polska: Krzysztof Cynarski%n<krzysztof at cynarski.net>%nKorekta: Łukasz Abramczuk%n<lukasz.abramczuk at gmail.com>

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
ButtonFinish=&Zakończ
ButtonBrowse=&Przeglądaj...
ButtonWizardBrowse=P&rzeglądaj...
ButtonNewFolder=&Utwórz nowy folder

; *** "Select Language" dialog messages
SelectLanguageTitle=Wybierz język instalacji
SelectLanguageLabel=Wybierz język używany podczas instalacji:

; *** Common wizard text
ClickNext=Wybierz przycisk Dalej, aby kontynuować, lub Anuluj, aby zakończyć instalację.
BeveledLabel=
BrowseDialogTitle=Wskaż folder
BrowseDialogLabel=Wybierz folder z poniższej listy, a następnie wybierz przycisk OK.
NewFolderName=Nowy folder

; *** "Welcome" wizard page
WelcomeLabel1=Witamy w Instalatorze aplikacji [name].
WelcomeLabel2=Instalator zainstaluje teraz aplikację [name/ver] na Twoim komputerze.%n%nZalecane jest zamknięcie wszystkich innych uruchomionych programów przed rozpoczęciem procesu instalacji.

; *** "Password" wizard page
WizardPassword=Hasło
PasswordLabel1=Ta instalacja jest zabezpieczona hasłem.
PasswordLabel3=Podaj hasło, a następnie wybierz przycisk Dalej, aby kontynuować. W hasłach rozróżniane są wielkie i małe litery.
PasswordEditLabel=&Hasło:
IncorrectPassword=Wprowadzone hasło nie jest prawidłowe. Spróbuj ponownie.

; *** "License Agreement" wizard page
WizardLicense=Umowa Licencyjna
LicenseLabel=Przed kontynuacją proszę przeczytać poniższe ważne informacje.
LicenseLabel3=Proszę przeczytać tekst Umowy Licencyjnej. Musisz zgodzić się na warunki tej umowy przed kontynuacją instalacji.
LicenseAccepted=&Akceptuję warunki umowy
LicenseNotAccepted=&Nie akceptuję warunków umowy

; *** "Information" wizard pages
WizardInfoBefore=Informacja
InfoBeforeLabel=Przed przejściem do dalszego etapu instalacji, proszę przeczytać poniższą informację.
InfoBeforeClickLabel=Kiedy będziesz gotowy do instalacji, kliknij przycisk Dalej.
WizardInfoAfter=Informacja
InfoAfterLabel=Przed przejściem do dalszego etapu instalacji, proszę przeczytać poniższą informację.
InfoAfterClickLabel=Gdy będziesz gotowy do zakończenia instalacji, kliknij przycisk Dalej.

; *** "User Information" wizard page
WizardUserInfo=Dane Użytkownika
UserInfoDesc=Proszę podać swoje dane.
UserInfoName=&Nazwisko:
UserInfoOrg=&Organizacja:
UserInfoSerial=Numer &seryjny:
UserInfoNameRequired=Musisz podać nazwisko.

; *** "Select Destination Location" wizard page
WizardSelectDir=Wybierz docelową lokalizację
SelectDirDesc=Gdzie ma być zainstalowana aplikacja [name]?
SelectDirLabel3=Instalator zainstaluje aplikację [name] do poniższego folderu.
SelectDirBrowseLabel=Kliknij przycisk Dalej, aby kontynuować. Jeśli chcesz określić inny folder, kliknij przycisk Przeglądaj.
DiskSpaceMBLabel=Potrzeba przynajmniej [mb] MB wolnego miejsca na dysku.
CannotInstallToNetworkDrive=Instalator nie może zainstalować aplikacji na dysku sieciowym.
CannotInstallToUNCPath=Instalator nie może zainstalować aplikacji w ścieżce UNC.
InvalidPath=Musisz wprowadzić pełną ścieżkę wraz z literą dysku, np.:%n%nC:\PROGRAM%n%nlub ścieżkę sieciową (UNC) w formacie:%n%n\\serwer\udział
InvalidDrive=Wybrany dysk lub udostępniony folder sieciowy nie istnieje. Proszę wybrać inny.
DiskSpaceWarningTitle=Niewystarczająca ilość wolnego miejsca na dysku
DiskSpaceWarning=Instalator wymaga co najmniej %1 KB wolnego miejsca na dysku. Wybrany dysk posiada tylko %2 KB dostępnego miejsca.%n%nCzy pomimo to chcesz kontynuować?
DirNameTooLong=Nazwa folderu lub ścieżki jest za długa.
InvalidDirName=Niepoprawna nazwa folderu.
BadDirName32=Nazwa folderu nie może zawierać żadnego z następujących znaków:%n%n%1
DirExistsTitle=Ten folder już istnieje
DirExists=Poniższy folder już istnienie:%n%n%1%n%nCzy pomimo to chcesz zainstalować aplikację w tym folderze?
DirDoesntExistTitle=Nie ma takiego folderu
DirDoesntExist=Poniższy folder nie istnieje:%n%n%1%n%nCzy chcesz, aby został utworzony?

; *** "Select Components" wizard page
WizardSelectComponents=Zaznacz komponenty
SelectComponentsDesc=Które komponenty mają być zainstalowane?
SelectComponentsLabel2=Zaznacz komponenty, które chcesz zainstalować, odznacz te, których nie chcesz zainstalować. Kliknij przycisk Dalej, aby kontynuować.
FullInstallation=Instalacja pełna
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalacja podstawowa
CustomInstallation=Instalacja użytkownika
NoUninstallWarningTitle=Zainstalowane komponenty
NoUninstallWarning=Instalator wykrył, że na Twoim komputerze są już zainstalowane następujące komponenty:%n%n%1%n%nOdznaczenie któregokolwiek z nich nie spowoduje ich dezinstalacji.%n%nCzy pomimo tego chcesz kontynuować?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Wybrane komponenty wymagają co najmniej [mb] MB na dysku.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Zaznacz dodatkowe zadania
SelectTasksDesc=Które dodatkowe zadania mają być wykonane?
SelectTasksLabel2=Zaznacz dodatkowe zadania, które Instalator ma wykonać podczas instalacji aplikacji [name], a następnie kliknij przycisk Dalej, aby kontynuować.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Wybierz folder Menu Start
SelectStartMenuFolderDesc=Gdzie mają być umieszczone skróty do aplikacji?
SelectStartMenuFolderLabel3=Instalator stworzy skróty do aplikacji w poniższym folderze Menu Start.
SelectStartMenuFolderBrowseLabel=Kliknij przycisk Dalej, aby kontynuować. Jeśli chcesz określić inny folder, kliknij przycisk Przeglądaj.
MustEnterGroupName=Musisz wprowadzić nazwę folderu.
GroupNameTooLong=Nazwa folderu lub ścieżki jest za długa.
InvalidGroupName=Niepoprawna nazwa folderu.
BadGroupName=Nazwa folderu nie może zawierać żadnego z następujących znaków:%n%n%1
NoProgramGroupCheck2=Nie twórz folderu w &Menu Start

; *** "Ready to Install" wizard page
WizardReady=Gotowy do rozpoczęcia instalacji
ReadyLabel1=Instalator jest już gotowy do rozpoczęcia instalacji aplikacji [name] na Twoim komputerze.
ReadyLabel2a=Kliknij przycisk Instaluj, aby rozpocząć instalację lub Wstecz, jeśli chcesz przejrzeć lub zmienić ustawienia.
ReadyLabel2b=Kliknij przycisk Instaluj, aby kontynuować instalację.
ReadyMemoUserInfo=Informacje użytkownika:
ReadyMemoDir=Lokalizacja docelowa:
ReadyMemoType=Rodzaj instalacji:
ReadyMemoComponents=Wybrane komponenty:
ReadyMemoGroup=Folder w Menu Start:
ReadyMemoTasks=Dodatkowe zadania:

; *** "Preparing to Install" wizard page
WizardPreparing=Przygotowanie do instalacji
PreparingDesc=Instalator przygotowuje instalację aplikacji [name] na Twoim komputerze.
PreviousInstallNotCompleted=Instalacja (usunięcie) poprzedniej wersji aplikacji nie została zakończona. Będziesz musiał ponownie uruchomić komputer, aby zakończyć instalację. %n%nPo ponownym uruchomieniu komputera uruchom ponownie instalatora, aby zakończyć instalację aplikacji [name].
CannotContinue=Instalator nie może kontynuować. Kliknij przycisk Anuluj, aby przerwać instalację.
ApplicationsFound=Poniższe aplikacje używają plików, które muszą być uaktualnione przez Instalator. Zalecane jest, aby pozwolić Instalatorowi automatycznie zamknąć te aplikacje.
ApplicationsFound2=Poniższe aplikacje używają plików, które muszą być uaktualnione przez Instalator. Zalecane jest, aby pozwolić Instalatorowi automatycznie zamknąć te aplikacje. Po zakończonej instalacji Instalator podejmie próbę ich ponownego uruchomienia.
CloseApplications=&Automatycznie zamknij aplikacje
DontCloseApplications=&Nie zamykaj aplikacji
ErrorCloseApplications=Instalator nie był w stanie zamknąć automatycznie wszystkich aplikacji. Zalecane jest zamknięcie wszystkich tych aplikacji, które aktualnie używają uaktualnianych przez Instalator plików.

; *** "Installing" wizard page
WizardInstalling=Instalacja
InstallingLabel=Poczekaj, aż instalator zainstaluje aplikację [name] na Twoim komputerze.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Zakończono instalację aplikacji [name]
FinishedLabelNoIcons=Instalator zakończył instalację aplikacji [name] na Twoim komputerze.
FinishedLabel=Instalator zakończył instalację aplikacji [name] na Twoim komputerze. Aplikacja może być uruchomiona poprzez użycie zainstalowanych skrótów.
ClickFinish=Kliknij przycisk Zakończ, aby zakończyć instalację.
FinishedRestartLabel=Aby zakończyć instalację aplikacji [name], Instalator musi ponownie uruchomić Twój komputer. Czy chcesz teraz uruchomić komputer ponownie?
FinishedRestartMessage=Aby zakończyć instalację aplikacji [name], Instalator musi ponownie uruchomić Twój komputer.%n%nCzy chcesz teraz uruchomić komputer ponownie?
ShowReadmeCheck=Tak, chcę przeczytać dodatkowe informacje
YesRadio=&Tak, teraz uruchom ponownie
NoRadio=&Nie, sam zrestartuję później
; used for example as 'Run MyProg.exe'
RunEntryExec=Uruchom %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Pokaż %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Instalator potrzebuje następnego dysku
SelectDiskLabel2=Proszę włożyć dysk %1 i kliknąć przycisk OK.%n%nJeśli wymieniony poniżej folder nie określa położenia plików z tego dysku, wprowadź poprawną ścieżkę lub kliknij przycisk Przeglądaj.
PathLabel=Ś&cieżka:
FileNotInDir2=Plik "%1" nie został znaleziony na dysku "%2". Proszę włożyć właściwy dysk lub wybrać inny folder.
SelectDirectoryLabel=Proszę określić lokalizację następnego dysku.

; *** Installation phase messages
SetupAborted=Instalacja nie została zakończona.%n%nProszę rozwiązać problem i ponownie rozpocząć instalację.
EntryAbortRetryIgnore=Możesz ponowić nieudaną czynność, zignorować ją (nie zalecane) lub przerwać instalację.

; *** Installation status messages
StatusClosingApplications=Zamykanie aplikacji...
StatusCreateDirs=Tworzenie folderów...
StatusExtractFiles=Dekompresja plików...
StatusCreateIcons=Tworzenie ikon aplikacji...
StatusCreateIniEntries=Tworzenie zapisów w plikach INI...
StatusCreateRegistryEntries=Tworzenie zapisów w rejestrze...
StatusRegisterFiles=Rejestracja plików...
StatusSavingUninstall=Zachowanie informacji dezinstalatora...
StatusRunProgram=Kończenie instalacji...
StatusRestartingApplications=Ponowne uruchamianie aplikacji...
StatusRollback=Cofanie zmian...

; *** Misc. errors
ErrorInternal2=Wewnętrzny błąd: %1
ErrorFunctionFailedNoCode=Błąd podczas wykonywania %1
ErrorFunctionFailed=Błąd podczas wykonywania %1; kod %2
ErrorFunctionFailedWithMessage=Błąd podczas wykonywania %1; kod %2.%n%3
ErrorExecutingProgram=Nie można uruchomić:%n%1

; *** Registry errors
ErrorRegOpenKey=Błąd podczas otwierania klucza rejestru:%n%1\%2
ErrorRegCreateKey=Błąd podczas tworzenia klucza rejestru:%n%1\%2
ErrorRegWriteKey=Błąd podczas zapisu do klucza rejestru:%n%1\%2

; *** INI errors
ErrorIniEntry=Błąd podczas tworzenia pozycji w pliku INI: "%1".

; *** File copying errors
FileAbortRetryIgnore=Możesz ponowić nieudaną czynność, zignorować ją, aby ominąć ten plik (nie zalecane), lub przerwać instalację.
FileAbortRetryIgnore2=Możesz ponowić nieudaną czynność, zignorować ją (nie zalecane) lub przerwać instalację.
SourceIsCorrupted=Plik źródłowy jest uszkodzony
SourceDoesntExist=Plik źródłowy "%1" nie istnieje
ExistingFileReadOnly=Istniejący plik jest oznaczony jako tylko-do-odczytu.%n%nMożesz ponowić (aby usunąć oznaczenie), zignorować (aby ominąć ten plik) lub przerwać instalację.
ErrorReadingExistingDest=Wystąpił błąd podczas próby odczytu istniejącego pliku:
FileExists=Plik już istnieje.%n%nCzy chcesz, aby Instalator zamienił go na nowy?
ExistingFileNewer=Istniejący plik jest nowszy niż ten, który Instalator próbuje skopiować. Zalecanym jest zachowanie istniejącego pliku.%n%nCzy chcesz zachować istniejący plik?
ErrorChangingAttr=Wystąpił błąd podczas próby zmiany atrybutów docelowego pliku:
ErrorCreatingTemp=Wystąpił błąd podczas próby utworzenia pliku w folderze docelowym:
ErrorReadingSource=Wystąpił błąd podczas próby odczytu pliku źródłowego:
ErrorCopying=Wystąpił błąd podczas próby kopiowania pliku:
ErrorReplacingExistingFile=Wystąpił błąd podczas próby zamiany istniejącego pliku:
ErrorRestartReplace=Próba zastąpienia plików podczas restartu komputera nie powiodła się.
ErrorRenamingTemp=Wystąpił błąd podczas próby zmiany nazwy pliku w folderze docelowym:
ErrorRegisterServer=Nie można zarejestrować DLL/OCX: %1
ErrorRegSvr32Failed=Funkcja RegSvr32 zakończyła się z kodem błędu %1
ErrorRegisterTypeLib=Nie mogę zarejestrować biblioteki typów: %1

; *** Post-installation errors
ErrorOpeningReadme=Wystąpił błąd podczas próby otwarcia pliku README.
ErrorRestartingComputer=Instalator nie mógł zrestartować tego komputera. Proszę zrobić to samodzielnie.

; *** Uninstaller messages
UninstallNotFound=Plik "%1" nie istnieje. Nie można go odinstalować.
UninstallOpenError=Plik "%1" nie mógł być otwarty. Nie można odinstalować.
UninstallUnsupportedVer=Ta wersja programu dezinstalacyjnego nie rozpoznaje formatu logu dezinstalacji. Nie można odinstalować.
UninstallUnknownEntry=W logu dezinstalacji wystąpiła nieznana pozycja (%1)
ConfirmUninstall=Czy na pewno chcesz usunąć aplikację %1 i wszystkie jej składniki?
UninstallOnlyOnWin64=Ta aplikacja może być odinstalowana tylko w 64-bitowej wersji systemu Windows.
OnlyAdminCanUninstall=Ta instalacja może być usunięta tylko przez użytkownika z prawami administratora.
UninstallStatusLabel=Poczekaj, aż aplikacja %1 zostanie usunięta z Twojego komputera.
UninstalledAll=Aplikacja %1 została usunięta z Twojego komputera.
UninstalledMost=Dezinstalacja aplikacji %1 zakończona.%n%nNiektóre elementy nie mogły być usunięte. Możesz je usunąć ręcznie.
UninstalledAndNeedsRestart=Twój komputer musi zostać ponownie uruchomiony, aby zakończyć proces dezinstalacji %1.%n%nCzy chcesz teraz ponownie uruchomić komputer?
UninstallDataCorrupted=Plik "%1" jest uszkodzony. Nie można odinstalować.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Usunąć plik współdzielony?
ConfirmDeleteSharedFile2=System wykrył, że następujący plik nie jest już używany przez żaden program. Czy chcesz odinstalować ten plik współdzielony?%n%nJeśli inne programy nadal używają tego pliku, a zostanie on usunięty, mogą one przestać działać prawidłowo. Jeśli nie jesteś pewny, wybierz przycisk Nie. Pozostawienie tego pliku w Twoim systemie nie spowoduje żadnych szkód.
SharedFileNameLabel=Nazwa pliku:
SharedFileLocationLabel=Położenie:
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
CreateDesktopIcon=Utwórz ikonę na &pulpicie
CreateQuickLaunchIcon=Utwórz ikonę na pasku &szybkiego uruchamiania
ProgramOnTheWeb=Strona internetowa aplikacji %1
UninstallProgram=Dezinstalacja aplikacji %1
LaunchProgram=Uruchom aplikację %1
AssocFileExtension=&Przypisz aplikację %1 do rozszerzenia pliku %2
AssocingFileExtension=Przypisywanie aplikacji %1 do rozszerzenia pliku %2...
AutoStartProgramGroupDescription=Autostart:
AutoStartProgram=Automatycznie uruchamiaj aplikację %1
AddonHostProgramNotFound=Aplikacja %1 nie została znaleziona we wskazanym przez Ciebie folderze.%n%nCzy pomimo tego chcesz kontynuować?
