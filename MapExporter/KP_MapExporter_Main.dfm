object KP_MapExporter_MainForm: TKP_MapExporter_MainForm
  Left = 0
  Top = 0
  Caption = 'Knights Province Map Exporter'
  ClientHeight = 179
  ClientWidth = 499
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    499
    179)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 13
    Width = 73
    Height = 13
    Caption = 'Map directory: '
  end
  object lblNotice: TLabel
    Left = 8
    Top = 96
    Width = 40
    Height = 13
    Caption = 'lblNotice'
  end
  object lblFileName: TLabel
    Left = 70
    Top = 56
    Width = 25
    Height = 13
    Caption = 'None'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 56
    Height = 13
    Caption = 'Map name: '
  end
  object edtMapFolderPath: TEdit
    Left = 87
    Top = 10
    Width = 372
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'C:\'
  end
  object btnMapFolderSelect: TButton
    Left = 465
    Top = 8
    Width = 26
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = btnMapFolderSelectClick
  end
  object btnCancel: TButton
    Left = 8
    Top = 146
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
    ExplicitTop = 372
  end
  object btnExport: TButton
    Left = 416
    Top = 146
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Export'
    TabOrder = 3
    OnClick = btnExportClick
    ExplicitTop = 372
  end
end
