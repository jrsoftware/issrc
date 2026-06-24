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
  object ActionList: TActionList
    Left = 24
    Top = 24
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
end
