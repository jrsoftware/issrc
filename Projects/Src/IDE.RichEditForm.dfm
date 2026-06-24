object RichEditForm: TRichEditForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = '*'
  ClientHeight = 500
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  StyleElements = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object ToolBarPanel: TPanel
    Left = 0
    Top = 0
    Width = 700
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    StyleName = 'Windows'
    object ToolBar: TToolBar
      AlignWithMargins = True
      Left = 7
      Top = 4
      Width = 690
      Height = 25
      Margins.Left = 7
      Margins.Top = 4
      Margins.Bottom = 0
      Images = ThemedToolbarVirtualImageList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Transparent = True
      object NewButton: TToolButton
        Left = 0
        Top = 0
        Hint = 'New (%1)'
        Action = NewAction
        ImageIndex = 0
        ImageName = 'document-new'
      end
      object OpenButton: TToolButton
        Left = 23
        Top = 0
        Hint = 'Open (%1)'
        Action = OpenAction
        ImageIndex = 1
        ImageName = 'folder-open-filled-arrow-down-right'
      end
      object SaveButton: TToolButton
        Left = 46
        Top = 0
        Hint = 'Save (%1)'
        Action = SaveAction
        ImageIndex = 2
        ImageName = 'save-filled'
      end
      object SaveAsButton: TToolButton
        Left = 69
        Top = 0
        Hint = 'Save As'
        Action = SaveAsAction
        ImageIndex = 11
        ImageName = 'save-as-filled'
      end
      object ToolButton3: TToolButton
        Left = 92
        Top = 0
        Width = 8
        Style = tbsSeparator
      end
      object UndoButton: TToolButton
        Left = 100
        Top = 0
        Hint = 'Undo (%1)'
        Action = UndoAction
        ImageIndex = 14
        ImageName = 'command-undo-1'
      end
      object RedoButton: TToolButton
        Left = 123
        Top = 0
        Hint = 'Redo (%1)'
        Action = RedoAction
        ImageIndex = 15
        ImageName = 'command-redo-1'
      end
      object ToolButton1: TToolButton
        Left = 146
        Top = 0
        Width = 8
        Style = tbsSeparator
      end
      object CutButton: TToolButton
        Left = 154
        Top = 0
        Hint = 'Cut (%1)'
        Action = CutAction
        ImageIndex = 16
        ImageName = 'clipboard-cut'
      end
      object CopyButton: TToolButton
        Left = 177
        Top = 0
        Hint = 'Copy (%1)'
        Action = CopyAction
        ImageIndex = 17
        ImageName = 'clipboard-copy'
      end
      object PasteButton: TToolButton
        Left = 200
        Top = 0
        Hint = 'Paste (%1)'
        Action = PasteAction
        ImageIndex = 18
        ImageName = 'clipboard-paste'
      end
      object ToolButton2: TToolButton
        Left = 223
        Top = 0
        Width = 8
        Style = tbsSeparator
      end
      object SelectAllButton: TToolButton
        Left = 231
        Top = 0
        Hint = 'Select All (%1)'
        Action = SelectAllAction
        ImageIndex = 19
        ImageName = 'select-all'
      end
    end
  end
  object ActionList: TActionList
    Left = 24
    Top = 48
    object NewAction: TAction
      Caption = '&New'
      ShortCut = 16462
      OnExecute = NewActionExecute
    end
    object OpenAction: TAction
      Caption = '&Open...'
      ShortCut = 16463
      OnExecute = OpenActionExecute
    end
    object SaveAction: TAction
      Caption = '&Save'
      ShortCut = 16467
      OnExecute = SaveActionExecute
    end
    object SaveAsAction: TAction
      Caption = 'Save &As...'
      OnExecute = SaveActionExecute
    end
    object UndoAction: TEditUndo
      Caption = '&Undo'
    end
    object RedoAction: TAction
      Caption = '&Redo'
      OnExecute = RedoActionExecute
      OnUpdate = RedoActionUpdate
    end
    object CutAction: TEditCut
      Caption = 'Cu&t'
    end
    object CopyAction: TEditCopy
      Caption = '&Copy'
    end
    object PasteAction: TEditPaste
      Caption = '&Paste'
    end
    object SelectAllAction: TEditSelectAll
      Caption = 'Select &All'
    end
  end
  object ThemedToolbarVirtualImageList: TVirtualImageList
    AutoFill = True
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'document-new'
        Name = 'document-new'
      end
      item
        CollectionIndex = 1
        CollectionName = 'folder-open-filled-arrow-down-right'
        Name = 'folder-open-filled-arrow-down-right'
      end
      item
        CollectionIndex = 2
        CollectionName = 'save-filled'
        Name = 'save-filled'
      end
      item
        CollectionIndex = 3
        CollectionName = 'build'
        Name = 'build'
      end
      item
        CollectionIndex = 4
        CollectionName = 'build-cancel-2'
        Name = 'build-cancel-2'
      end
      item
        CollectionIndex = 5
        CollectionName = 'debug-start-filled'
        Name = 'debug-start-filled'
      end
      item
        CollectionIndex = 6
        CollectionName = 'debug-break-all-filled'
        Name = 'debug-break-all-filled'
      end
      item
        CollectionIndex = 7
        CollectionName = 'install'
        Name = 'install'
      end
      item
        CollectionIndex = 8
        CollectionName = 'uninstall'
        Name = 'uninstall'
      end
      item
        CollectionIndex = 9
        CollectionName = 'button-help'
        Name = 'button-help'
      end
      item
        CollectionIndex = 10
        CollectionName = 'debug-stop-filled'
        Name = 'debug-stop-filled'
      end
      item
        CollectionIndex = 11
        CollectionName = 'save-as-filled'
        Name = 'save-as-filled'
      end
      item
        CollectionIndex = 12
        CollectionName = 'save-all-filled'
        Name = 'save-all-filled'
      end
      item
        CollectionIndex = 13
        CollectionName = 'printer'
        Name = 'printer'
      end
      item
        CollectionIndex = 14
        CollectionName = 'command-undo-1'
        Name = 'command-undo-1'
      end
      item
        CollectionIndex = 15
        CollectionName = 'command-redo-1'
        Name = 'command-redo-1'
      end
      item
        CollectionIndex = 16
        CollectionName = 'clipboard-cut'
        Name = 'clipboard-cut'
      end
      item
        CollectionIndex = 17
        CollectionName = 'clipboard-copy'
        Name = 'clipboard-copy'
      end
      item
        CollectionIndex = 18
        CollectionName = 'clipboard-paste'
        Name = 'clipboard-paste'
      end
      item
        CollectionIndex = 19
        CollectionName = 'select-all'
        Name = 'select-all'
      end
      item
        CollectionIndex = 20
        CollectionName = 'find'
        Name = 'find'
      end
      item
        CollectionIndex = 21
        CollectionName = 'replace'
        Name = 'replace'
      end
      item
        CollectionIndex = 22
        CollectionName = 'control-edit'
        Name = 'control-edit'
      end
      item
        CollectionIndex = 23
        CollectionName = 'debug-step-into'
        Name = 'debug-step-into'
      end
      item
        CollectionIndex = 24
        CollectionName = 'debug-step-over'
        Name = 'debug-step-over'
      end
      item
        CollectionIndex = 25
        CollectionName = 'debug-step-out'
        Name = 'debug-step-out'
      end
      item
        CollectionIndex = 26
        CollectionName = 'debug-breakpoint-filled'
        Name = 'debug-breakpoint-filled'
      end
      item
        CollectionIndex = 27
        CollectionName = 'variables'
        Name = 'variables'
      end
      item
        CollectionIndex = 28
        CollectionName = 'heart-filled'
        Name = 'heart-filled'
      end
      item
        CollectionIndex = 29
        CollectionName = 'alert-filled'
        Name = 'alert-filled'
      end
      item
        CollectionIndex = 30
        CollectionName = 'home'
        Name = 'home'
      end
      item
        CollectionIndex = 31
        CollectionName = 'button-info'
        Name = 'button-info'
      end
      item
        CollectionIndex = 32
        CollectionName = 'application'
        Name = 'application'
      end
      item
        CollectionIndex = 33
        CollectionName = 'folder-open-filled-find'
        Name = 'folder-open-filled-find'
      end
      item
        CollectionIndex = 34
        CollectionName = 'gear-filled'
        Name = 'gear-filled'
      end
      item
        CollectionIndex = 35
        CollectionName = 'key-filled'
        Name = 'key-filled'
      end
      item
        CollectionIndex = 36
        CollectionName = 'unused\letter-a-arrow-right-2'
        Name = 'unused\letter-a-arrow-right-2'
      end
      item
        CollectionIndex = 37
        CollectionName = 'symbol-cancel'
        Name = 'symbol-cancel'
      end
      item
        CollectionIndex = 38
        CollectionName = 'comment-text-script-filled'
        Name = 'comment-text-script-filled'
      end
      item
        CollectionIndex = 39
        CollectionName = 'control-tree-script-filled'
        Name = 'control-tree-script-filled'
      end
      item
        CollectionIndex = 40
        CollectionName = 'documents-script-filled'
        Name = 'documents-script-filled'
      end
      item
        CollectionIndex = 41
        CollectionName = 'tag-script-filled'
        Name = 'tag-script-filled'
      end
      item
        CollectionIndex = 42
        CollectionName = 'control-tab-filled-arrow-left-2'
        Name = 'control-tab-filled-arrow-left-2'
      end
      item
        CollectionIndex = 43
        CollectionName = 'control-tab-filled-arrow-right-2'
        Name = 'control-tab-filled-arrow-right-2'
      end
      item
        CollectionIndex = 44
        CollectionName = 'unused\control-tab-filled-cancel-2'
        Name = 'unused\control-tab-filled-cancel-2'
      end
      item
        CollectionIndex = 45
        CollectionName = 'control-tab-filled-redo-1'
        Name = 'control-tab-filled-redo-1'
      end
      item
        CollectionIndex = 46
        CollectionName = 'unused\find-arrow-left-2'
        Name = 'unused\find-arrow-left-2'
      end
      item
        CollectionIndex = 47
        CollectionName = 'unused\find-arrow-right-2'
        Name = 'unused\find-arrow-right-2'
      end
      item
        CollectionIndex = 48
        CollectionName = 'announcement'
        Name = 'announcement'
      end
      item
        CollectionIndex = 49
        CollectionName = 'debug-start-filled-arrow-right-2'
        Name = 'debug-start-filled-arrow-right-2'
      end
      item
        CollectionIndex = 50
        CollectionName = 'zoom-in'
        Name = 'zoom-in'
      end
      item
        CollectionIndex = 51
        CollectionName = 'zoom-out'
        Name = 'zoom-out'
      end
      item
        CollectionIndex = 52
        CollectionName = 'unused\debug-breakpoint-filled-eraser'
        Name = 'unused\debug-breakpoint-filled-eraser'
      end
      item
        CollectionIndex = 53
        CollectionName = 'debug-breakpoints-filled-eraser'
        Name = 'debug-breakpoints-filled-eraser'
      end
      item
        CollectionIndex = 54
        CollectionName = 'button-arrow-left'
        Name = 'button-arrow-left'
      end
      item
        CollectionIndex = 55
        CollectionName = 'button-arrow-right'
        Name = 'button-arrow-right'
      end
      item
        CollectionIndex = 56
        CollectionName = 'folder-open-filled'
        Name = 'folder-open-filled'
      end
      item
        CollectionIndex = 57
        CollectionName = 'eraser'
        Name = 'eraser'
      end
      item
        CollectionIndex = 58
        CollectionName = 'symbol-add'
        Name = 'symbol-add'
      end
      item
        CollectionIndex = 59
        CollectionName = 'symbol-remove'
        Name = 'symbol-remove'
      end
      item
        CollectionIndex = 60
        CollectionName = 'padlock-filled'
        Name = 'padlock-filled'
      end
      item
        CollectionIndex = 61
        CollectionName = 'shopping-cart'
        Name = 'shopping-cart'
      end
      item
        CollectionIndex = 62
        CollectionName = 'document-font'
        Name = 'document-font'
      end>
    ImageCollection = ImagesModule.LightToolBarImageCollection
    Left = 24
    Top = 120
  end
end
