object FormSendEmail: TFormSendEmail
  Left = 0
  Top = 0
  Caption = 'SendEmail'
  ClientHeight = 475
  ClientWidth = 494
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlInfo: TPanel
    Left = 0
    Top = 0
    Width = 494
    Height = 21
    Cursor = crHandPoint
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = '  GITHUB: https://github.com/dliocode/sendemail'
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
    OnClick = pnlInfoClick
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 21
    Width = 494
    Height = 435
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlStatus'
    TabOrder = 1
    object PageControlSendEmail: TPageControl
      Left = 0
      Top = 0
      Width = 494
      Height = 435
      ActivePage = tabConfiguration
      Align = alClient
      TabOrder = 0
      object tabConfiguration: TTabSheet
        Caption = 'Configuration'
        object pnlConfiguration: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 480
          Height = 401
          Align = alClient
          BevelOuter = bvNone
          DoubleBuffered = True
          ParentBackground = False
          ParentDoubleBuffered = False
          TabOrder = 0
          DesignSize = (
            480
            401)
          object Label3: TLabel
            Left = 10
            Top = 36
            Width = 62
            Height = 13
            Caption = 'Email (From)'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label5: TLabel
            Left = 10
            Top = 129
            Width = 62
            Height = 13
            Caption = 'Server SMTP'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label6: TLabel
            Left = 357
            Top = 129
            Width = 21
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Port'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label7: TLabel
            Left = 357
            Top = 83
            Width = 51
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Encrypted'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label16: TLabel
            Left = 10
            Top = 175
            Width = 23
            Height = 13
            Caption = 'User'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label17: TLabel
            Left = 244
            Top = 175
            Width = 49
            Height = 13
            Caption = 'Password'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label1: TLabel
            Left = 403
            Top = 129
            Width = 25
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Auth'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label4: TLabel
            Left = 10
            Top = 83
            Width = 82
            Height = 13
            Caption = 'Name (optional)'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label14: TLabel
            Left = 299
            Top = 225
            Width = 52
            Height = 13
            Caption = 'Log Mode'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object cmbEncrypted: TComboBox
            Left = 357
            Top = 102
            Width = 112
            Height = 22
            Style = csOwnerDrawFixed
            Anchors = [akTop, akRight]
            TabOrder = 2
            Items.Strings = (
              'None'
              'SSL'
              'TSL'
              'SSL/TLS')
          end
          object edtUser: TEdit
            Left = 10
            Top = 194
            Width = 225
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 6
          end
          object edtPassword: TEdit
            Left = 244
            Top = 194
            Width = 225
            Height = 21
            PasswordChar = '*'
            TabOrder = 7
          end
          object edtHost: TEdit
            Left = 10
            Top = 148
            Width = 341
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            CharCase = ecLowerCase
            TabOrder = 3
          end
          object edtPort: TEdit
            Left = 357
            Top = 148
            Width = 40
            Height = 21
            Anchors = [akTop, akRight]
            CharCase = ecLowerCase
            TabOrder = 4
          end
          object edtFrom: TEdit
            Left = 10
            Top = 55
            Width = 459
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            CharCase = ecLowerCase
            TabOrder = 0
          end
          object cmbAuth: TComboBox
            Left = 403
            Top = 148
            Width = 66
            Height = 21
            Anchors = [akTop, akRight]
            ItemIndex = 0
            TabOrder = 5
            Text = 'No'
            Items.Strings = (
              'No'
              'Yes')
          end
          object edtFromName: TEdit
            Left = 10
            Top = 102
            Width = 341
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object pnlHeaderEmailConfiguration: TPanel
            Left = 0
            Top = 0
            Width = 480
            Height = 21
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = '  Configuration'
            Color = 3987455
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 8
          end
          object ragLoadSettings: TRadioGroup
            Left = 10
            Top = 259
            Width = 459
            Height = 78
            Caption = 'Load Settings - Server SMTP'
            Columns = 3
            ItemIndex = 0
            Items.Strings = (
              'Gmail'
              'Outlook/Office 365'
              'Hotmail'
              'Yahoo'
              'SendGrid'
              'LocalWeb'
              'SparkPost'
              'Elastic Email'
              'Mail')
            TabOrder = 9
            OnClick = ragLoadSettingsClick
          end
          object chkReceiptRecipient: TCheckBox
            Left = 10
            Top = 224
            Width = 143
            Height = 17
            Caption = 'Request read confirmation'
            TabOrder = 10
          end
          object cmbLogMode: TComboBox
            Left = 357
            Top = 222
            Width = 112
            Height = 22
            Style = csOwnerDrawFixed
            ItemIndex = 1
            TabOrder = 11
            Text = 'tLib'
            Items.Strings = (
              'tComponent'
              'tLib'
              'tAll'
              'None')
          end
        end
      end
      object tabRecipient: TTabSheet
        Caption = 'Recipient'
        ImageIndex = 1
        object pnlRecipient: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 480
          Height = 401
          Align = alClient
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 0
          DesignSize = (
            480
            401)
          object Label2: TLabel
            Left = 10
            Top = 180
            Width = 38
            Height = 13
            Caption = 'Subject'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label18: TLabel
            Left = 10
            Top = 36
            Width = 48
            Height = 13
            Caption = 'Email (To)'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label19: TLabel
            Left = 255
            Top = 36
            Width = 82
            Height = 13
            Caption = 'Name (optional)'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label8: TLabel
            Left = 10
            Top = 83
            Width = 14
            Height = 13
            Caption = 'CC'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label9: TLabel
            Left = 255
            Top = 83
            Width = 82
            Height = 13
            Caption = 'Name (optional)'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label10: TLabel
            Left = 10
            Top = 132
            Width = 20
            Height = 13
            Caption = 'BCC'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label11: TLabel
            Left = 255
            Top = 129
            Width = 82
            Height = 13
            Caption = 'Name (optional)'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label12: TLabel
            Left = 10
            Top = 287
            Width = 45
            Height = 13
            Caption = 'Message'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object Label13: TLabel
            Left = 389
            Top = 180
            Width = 36
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Priority'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ExplicitLeft = 439
          end
          object edtTo: TEdit
            Left = 10
            Top = 55
            Width = 239
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 0
          end
          object mmMessage: TMemo
            Left = 10
            Top = 306
            Width = 371
            Height = 87
            Anchors = [akLeft, akTop, akRight]
            Lines.Strings = (
              'Texto normal'
              ''
              '<h1>Texto com HTML</h1>')
            TabOrder = 8
          end
          object edtToName: TEdit
            Left = 255
            Top = 55
            Width = 217
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object edtSubject: TEdit
            Left = 10
            Top = 199
            Width = 373
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 6
          end
          object Panel1: TPanel
            Left = 0
            Top = 0
            Width = 480
            Height = 21
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = '  Recipient'
            Color = 15838531
            Font.Charset = ANSI_CHARSET
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 9
          end
          object edtCc: TEdit
            Left = 10
            Top = 102
            Width = 239
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 2
          end
          object edtCcName: TEdit
            Left = 255
            Top = 102
            Width = 217
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
          end
          object edtBcc: TEdit
            Left = 10
            Top = 151
            Width = 239
            Height = 21
            CharCase = ecLowerCase
            TabOrder = 4
          end
          object edtBccName: TEdit
            Left = 255
            Top = 151
            Width = 217
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 5
          end
          object lbAttachment: TListBox
            Left = 10
            Top = 233
            Width = 371
            Height = 49
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ItemHeight = 13
            MultiSelect = True
            PopupMenu = PopupMenu
            TabOrder = 10
          end
          object btnSend: TButton
            Left = 387
            Top = 306
            Width = 83
            Height = 87
            Anchors = [akTop, akRight]
            Caption = 'Send'
            TabOrder = 11
            OnClick = btnSendClick
          end
          object btnAttachment: TButton
            Left = 387
            Top = 233
            Width = 83
            Height = 49
            Anchors = [akTop, akRight]
            Caption = 'Attachment'
            TabOrder = 12
            OnClick = btnAttachmentClick
          end
          object cmbPriority: TComboBox
            Left = 389
            Top = 199
            Width = 81
            Height = 22
            Style = csOwnerDrawFixed
            Anchors = [akTop, akRight]
            ItemIndex = 2
            TabOrder = 7
            Text = 'mpNormal'
            Items.Strings = (
              'mpHighest'
              'mpHigh'
              'mpNormal'
              'mpLow'
              'mpLowest')
          end
        end
      end
      object tabLog: TTabSheet
        Caption = 'Log'
        ImageIndex = 2
        object PanelLog: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 480
          Height = 401
          Align = alClient
          BevelOuter = bvNone
          Caption = 'PanelLog'
          ParentBackground = False
          TabOrder = 0
          object memoLog: TMemo
            AlignWithMargins = True
            Left = 10
            Top = 31
            Width = 460
            Height = 360
            Margins.Left = 10
            Margins.Top = 10
            Margins.Right = 10
            Margins.Bottom = 10
            Align = alClient
            BevelInner = bvNone
            BevelOuter = bvNone
            ScrollBars = ssVertical
            TabOrder = 0
          end
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 480
            Height = 21
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = '  Log'
            Color = clMaroon
            Font.Charset = ANSI_CHARSET
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentBackground = False
            ParentFont = False
            TabOrder = 1
          end
        end
      end
    end
  end
  object ProgressBar: TProgressBar
    Left = 0
    Top = 456
    Width = 494
    Height = 19
    Align = alBottom
    MarqueeInterval = 1
    TabOrder = 2
  end
  object PopupMenu: TPopupMenu
    Left = 200
    Top = 400
    object Remover1: TMenuItem
      Caption = 'Remover'
      OnClick = Remover1Click
    end
  end
end
