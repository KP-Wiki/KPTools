object KP_MapInstaller_MainForm: TKP_MapInstaller_MainForm
  Left = 0
  Top = 0
  Caption = 'Knights Province Map Installer'
  ClientHeight = 306
  ClientWidth = 766
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    766
    306)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 56
    Height = 13
    Caption = 'Map name: '
  end
  object lblMapName: TLabel
    Left = 70
    Top = 8
    Width = 15
    Height = 13
    Caption = 'MN'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 52
    Height = 13
    Caption = 'Map type: '
  end
  object Label6: TLabel
    Left = 416
    Top = 40
    Width = 42
    Height = 13
    Caption = 'file size: '
  end
  object lblFileSize: TLabel
    Left = 464
    Top = 40
    Width = 6
    Height = 13
    Caption = '0'
  end
  object btnCancel: TButton
    Left = 8
    Top = 273
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Cancel'
    TabOrder = 0
    OnClick = btnCancelClick
  end
  object btnInstall: TButton
    Left = 683
    Top = 273
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Install'
    TabOrder = 2
    OnClick = btnInstallClick
  end
  object lvMapItems: TListView
    Left = 8
    Top = 72
    Width = 750
    Height = 195
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Filename'
        Width = 295
      end
      item
        Alignment = taRightJustify
        Caption = 'Size'
        Width = 100
      end
      item
        Caption = 'Date (GMT)'
        Width = 150
      end
      item
        Caption = 'Permissions'
        Width = 100
      end
      item
        Caption = 'File type'
        Width = 100
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object rbSP: TRadioButton
    Left = 66
    Top = 39
    Width = 79
    Height = 17
    Hint = 'Select a map type.'
    Caption = 'Singleplayer'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = rbClick
  end
  object rbMP: TRadioButton
    Left = 151
    Top = 39
    Width = 74
    Height = 17
    Hint = 'Select a map type.'
    Caption = 'Multiplayer'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = rbClick
  end
  object rbCamp: TRadioButton
    Left = 231
    Top = 39
    Width = 74
    Height = 17
    Hint = 'Select a map type.'
    Caption = 'Campaign'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = rbClick
  end
end
