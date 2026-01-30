; -- CodeAutomation2.iss --
;
; This script shows how to use IUnknown based COM Automation objects.
;
; Note: some unneeded interface functions which had special types have been replaced
; by dummies to avoid having to define those types. Do not remove these dummies as
; that would change the function indices which is bad. Also, not all function
; prototypes have been tested, only those used by this example.

[Setup]
AppName=My Program
AppVersion=1.5
WizardStyle=modern dynamic
DisableWelcomePage=no
CreateAppDir=no
Uninstallable=no
DisableProgramGroupPage=yes
DefaultGroupName=My Program
OutputDir=userdocs:Inno Setup Examples Output

[Code]

{--- IShellLink ---}

const
  CLSID_ShellLink = '{00021401-0000-0000-C000-000000000046}';

type
  IShellLinkW = interface(IUnknown)
    '{000214F9-0000-0000-C000-000000000046}'
    procedure Dummy;
    procedure Dummy2;
    procedure Dummy3;
    procedure GetDescription(pszName: String; cchMaxName: Integer); safecall;
    procedure SetDescription(pszName: String); safecall;
    procedure GetWorkingDirectory(pszDir: String; cchMaxPath: Integer); safecall;
    procedure SetWorkingDirectory(pszDir: String); safecall;
    procedure GetArguments(pszArgs: String; cchMaxPath: Integer); safecall;
    procedure SetArguments(pszArgs: String); safecall;
    function GetHotkey: Word; safecall;
    procedure SetHotkey(wHotkey: Word); safecall;
    function GetShowCmd: Integer; safecall;
    procedure SetShowCmd(iShowCmd: Integer); safecall;
    procedure GetIconLocation(pszIconPath: String; cchIconPath: Integer;
      out piIcon: Integer); safecall;
    procedure SetIconLocation(pszIconPath: String; iIcon: Integer); safecall;
    procedure SetRelativePath(pszPathRel: String; dwReserved: DWORD); safecall;
    procedure Resolve(Wnd: HWND; fFlags: DWORD); safecall;
    procedure SetPath(pszFile: String); safecall;
  end;

  IPersist = interface(IUnknown)
    '{0000010C-0000-0000-C000-000000000046}'
    procedure GetClassID(var classID: TGUID); safecall;
  end;

  IPersistFile = interface(IPersist)
    '{0000010B-0000-0000-C000-000000000046}'
    procedure IsDirty; safecall;
    procedure Load(pszFileName: String; dwMode: Longint); safecall;
    procedure Save(pszFileName: String; fRemember: BOOL); safecall;
    procedure SaveCompleted(pszFileName: String); safecall;
    function GetCurFile: String; safecall;
  end;

procedure IShellLinkButtonOnClick(Sender: TObject);
var
  Obj: IUnknown;
  SL: IShellLinkW;
  PF: IPersistFile;
begin
  { Create the main ShellLink COM Automation object }
  Obj := CreateComObject(StringToGuid(CLSID_ShellLink));

  { Set the shortcut properties }
  SL := IShellLinkW(Obj);
  SL.SetPath(ExpandConstant('{srcexe}'));
  SL.SetArguments('');
  SL.SetShowCmd(SW_SHOWNORMAL);

  { Save the shortcut }
  PF := IPersistFile(Obj);
  PF.Save(ExpandConstant('{autodesktop}\CodeAutomation2 Test.lnk'), True);

  MsgBox('Saved a shortcut named ''CodeAutomation2 Test'' on the desktop.', mbInformation, mb_Ok);
end;

{--- ITaskScheduler ---}

const
  CLSID_TaskScheduler = '{148BD52A-A2AB-11CE-B11F-00AA00530503}';
  CLSID_Task = '{148BD520-A2AB-11CE-B11F-00AA00530503}';
  IID_Task = '{148BD524-A2AB-11CE-B11F-00AA00530503}';
  TASK_TIME_TRIGGER_DAILY = 1;

type
  ITaskScheduler = interface(IUnknown)
    '{148BD527-A2AB-11CE-B11F-00AA00530503}'
    procedure SetTargetComputer(pwszComputer: String); safecall;
    function GetTargetComputer: String; safecall;
    procedure Dummy;
    function Activate(pwszName: String; var riid: TGUID): IUnknown; safecall;
    procedure Delete(pwszName: String); safecall;
    function NewWorkItem(pwszTaskName: String; var rclsid: TGUID; var riid: TGUID): IUnknown; safecall;
    procedure Dummy2;
    function IsOfType(pwszName: String; var riid: TGUID): HResult;
  end;

  TDaily = record
    DaysInterval: WORD;
  end;

  TWeekly = record
    WeeksInterval: WORD;
    rgfDaysOfTheWeek: WORD;
  end;

  TMonthlyDate = record
    rgfDays: DWORD;
    rgfMonths: WORD;
  end;

  TMonthlyDow = record
    wWhichWeek: WORD;
    rgfDaysOfTheWeek: WORD;
    rgfMonths: WORD;
  end;

  { ROPS doesn't support unions, replace this with the type you need and adjust padding (end size has to be 48). }
  TTriggerTypeUnion = record
    Daily: TDaily;
    Pad1: WORD;
    Pad2: WORD;
    Pad3: WORD;
  end;

  TTaskTrigger = record
    cbTriggerSize: WORD;
    Reserved1: WORD;
    wBeginYear: WORD;
    wBeginMonth: WORD;
    wBeginDay: WORD;
    wEndYear: WORD;
    wEndMonth: WORD;
    wEndDay: WORD;
    wStartHour: WORD;
    wStartMinute: WORD;
    MinutesDuration: DWORD;
    MinutesInterval: DWORD;
    rgFlags: DWORD;
    TriggerType: DWORD;
    Type_: TTriggerTypeUnion;
    Reserved2: WORD;
    wRandomMinutesInterval: WORD;
  end;

  ITaskTrigger = interface(IUnknown)
    '{148BD52B-A2AB-11CE-B11F-00AA00530503}'
    procedure SetTrigger(var pTrigger: TTaskTrigger); safecall;
    procedure GetTrigger(var pTrigger: TTaskTrigger); safecall;
    procedure GetTriggerString(var ppwszTrigger: String); safecall;
  end;

  IScheduledWorkItem = interface(IUnknown)
    '{A6B952F0-A4B1-11D0-997D-00AA006887EC}'
    function CreateTrigger(out piNewTrigger: Word): ITaskTrigger; safecall;
    procedure DeleteTrigger(iTrigger: Word); safecall;
    function GetTriggerCount: Word; safecall;
    function GetTrigger(iTrigger: Word): ITaskTrigger; safecall;
    function GetTriggerString(iTrigger: Word): String; safecall;
    procedure Dummy;
    procedure Dummy2;
    procedure SetIdleWait(wIdleMinutes: Word; wDeadlineMinutes: Word); safecall;
    procedure GetIdleWait(out pwIdleMinutes: Word; out pwDeadlineMinutes: Word); safecall;
    procedure Run; safecall;
    procedure Terminate; safecall;
    procedure EditWorkItem(hParent: HWND; dwReserved: DWORD); safecall;
    procedure Dummy3;
    function GetStatus: HRESULT; safecall;
    function GetExitCode: DWORD; safecall;
    procedure SetComment(pwszComment: String); safecall;
    function GetComment: String; safecall;
    procedure SetCreator(pwszCreator: String); safecall;
    function GetCreator: String; safecall;
    procedure SetWorkItemData(cbData: Word; var rgbData: Byte); safecall;
    procedure GetWorkItemData(out pcbData: Word; out prgbData: Byte); safecall;
    procedure SetErrorRetryCount(wRetryCount: Word); safecall;
    function GetErrorRetryCount: Word; safecall;
    procedure SetErrorRetryInterval(wRetryInterval: Word); safecall;
    function GetErrorRetryInterval: Word; safecall;
    procedure SetFlags(dwFlags: DWORD); safecall;
    function GetFlags: DWORD; safecall;
    procedure SetAccountInformation(pwszAccountName: String; pwszPassword: String); safecall;
    function GetAccountInformation: String; safecall;
  end;

  ITask = interface(IScheduledWorkItem)
    '{148BD524-A2AB-11CE-B11F-00AA00530503}'
    procedure SetApplicationName(pwszApplicationName: String); safecall;
    procedure GetApplicationName(out ppwszApplicationName: String); safecall;
    procedure SetParameters(pwszParameters: String); safecall;
    procedure GetParameters(out ppwszParameters: String); safecall;
    procedure SetWorkingDirectory(pwszWorkingDirectory: String); safecall;
    procedure GetWorkingDirectory(out ppwszWorkingDirectory: String); safecall;
    procedure SetPriority(dwPriority: DWORD); safecall;
    procedure GetPriority(out pdwPriority: DWORD); safecall;
    procedure SetTaskFlags(dwFlags: DWORD); safecall;
    procedure GetTaskFlags(out pdwFlags: DWORD); safecall;
    procedure SetMaxRunTime(dwMaxRunTimeMS: DWORD); safecall;
    procedure GetMaxRunTime(out pdwMaxRunTimeMS: DWORD); safecall;
  end;


procedure ITaskSchedulerButtonOnClick(Sender: TObject);
var
  Obj, Obj2: IUnknown;
  TaskScheduler: ITaskScheduler;
  G1, G2: TGUID;
  Task: ITask;
  iNewTrigger: WORD;
  TaskTrigger: ITaskTrigger;
  TaskTrigger2: TTaskTrigger;
  PF: IPersistFile;
begin
  { Create the main TaskScheduler COM Automation object }
  Obj := CreateComObject(StringToGuid(CLSID_TaskScheduler));

  { Create the Task COM automation object }
  TaskScheduler := ITaskScheduler(Obj);
  G1 := StringToGuid(CLSID_Task);
  G2 := StringToGuid(IID_Task);
  //This will throw an exception if the task already exists
  Obj2 := TaskScheduler.NewWorkItem('CodeAutomation2 Test', G1, G2);

  { Set the task properties }
  Task := ITask(Obj2);
  Task.SetComment('CodeAutomation2 Test Comment');
  Task.SetApplicationName(ExpandConstant('{srcexe}'));

  { Set the task account information }
  //Uncomment the following and provide actual user info to get a runnable task
  //Task.SetAccountInformation('username', 'password');

  { Create the TaskTrigger COM automation object }
  TaskTrigger := Task.CreateTrigger(iNewTrigger);

  { Set the task trigger properties }
  with TaskTrigger2 do begin
    cbTriggerSize := SizeOf(TaskTrigger2);
    wBeginYear := 2009;
    wBeginMonth := 10;
    wBeginDay := 1;
    wStartHour := 12;
    TriggerType := TASK_TIME_TRIGGER_DAILY;
    Type_.Daily.DaysInterval := 1;
  end;
  TaskTrigger.SetTrigger(TaskTrigger2);

  { Save the task }
  PF := IPersistFile(Obj2);
  PF.Save('', True);

  MsgBox('Created a daily task named named ''CodeAutomation2 Test''.' + #13#13 + 'Note: Account information not set so the task won''t actually run, uncomment the SetAccountInfo call and provide actual user info to get a runnable task.', mbInformation, mb_Ok);
end;

{---}

procedure CreateButton(ALeft, ATop: Integer; ACaption: String; ANotifyEvent: TNotifyEvent);
begin
  with TNewButton.Create(WizardForm) do begin
    Left := ALeft;
    Top := ATop;
    Width := (WizardForm.CancelButton.Width*3)/2;
    Height := WizardForm.CancelButton.Height;
    Caption := ACaption;
    OnClick := ANotifyEvent;
    Parent := WizardForm.WelcomePage;
  end;
end;

procedure InitializeWizard();
var
  Left, LeftInc, Top, TopInc: Integer;
begin
  Left := WizardForm.WelcomeLabel2.Left;
  LeftInc := (WizardForm.CancelButton.Width*3)/2 + ScaleX(8);
  TopInc := WizardForm.CancelButton.Height + ScaleY(8);
  Top := WizardForm.WelcomeLabel2.Top + WizardForm.WelcomeLabel2.Height - 4*TopInc;

  CreateButton(Left, Top, '&IShellLink...', @IShellLinkButtonOnClick);
  Top := Top + TopInc;
  CreateButton(Left, Top, '&ITaskScheduler...', @ITaskSchedulerButtonOnClick);
end;