unit USendEmail;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Winapi.ShellAPI,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.pngimage, Vcl.ComCtrls,
  IdComponent, Vcl.Menus,
  SendEmail;

type
  TFormSendEmail = class(TForm)
    pnlInfo: TPanel;
    pnlStatus: TPanel;
    PageControlSendEmail: TPageControl;
    tabConfiguration: TTabSheet;
    tabRecipient: TTabSheet;
    tabLog: TTabSheet;
    pnlConfiguration: TPanel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label1: TLabel;
    Label4: TLabel;
    cmbEncrypted: TComboBox;
    edtUser: TEdit;
    edtPassword: TEdit;
    edtHost: TEdit;
    edtPort: TEdit;
    edtFrom: TEdit;
    cmbAuth: TComboBox;
    edtFromName: TEdit;
    pnlHeaderEmailConfiguration: TPanel;
    ragLoadSettings: TRadioGroup;
    chkReceiptRecipient: TCheckBox;
    pnlRecipient: TPanel;
    Label2: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    edtTo: TEdit;
    mmMessage: TMemo;
    edtToName: TEdit;
    edtSubject: TEdit;
    Panel1: TPanel;
    edtCc: TEdit;
    edtCcName: TEdit;
    edtBcc: TEdit;
    edtBccName: TEdit;
    lbAttachment: TListBox;
    btnSend: TButton;
    btnAttachment: TButton;
    cmbPriority: TComboBox;
    PanelLog: TPanel;
    memoLog: TMemo;
    Panel2: TPanel;
    cmbLogMode: TComboBox;
    Label14: TLabel;
    ProgressBar: TProgressBar;
    PopupMenu: TPopupMenu;
    Remover1: TMenuItem;
    btnSendAsync: TButton;
    procedure btnAttachmentClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlInfoClick(Sender: TObject);
    procedure ragLoadSettingsClick(Sender: TObject);
    procedure Remover1Click(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnSendAsyncClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadSettings;
    procedure ConfigurationsDefault;
  public
    { Public declarations }
  end;

var
  FormSendEmail: TFormSendEmail;

implementation

{$R *.dfm}


procedure TFormSendEmail.btnAttachmentClick(Sender: TObject);
var
  LOpenDialog: TFileOpenDialog;
begin
  LOpenDialog := TFileOpenDialog.Create(Self);
  try
    LOpenDialog.Options := [fdoAllowMultiSelect];
    LOpenDialog.DefaultFolder := ExtractFilePath(Application.ExeName);
    if LOpenDialog.Execute then
      lbAttachment.Items.AddStrings(LOpenDialog.Files);
  finally
    LOpenDialog.Free;
  end;
end;

procedure TFormSendEmail.btnSendAsyncClick(Sender: TObject);
begin
  PageControlSendEmail.ActivePage := tabLog;

  LoadSettings;

  TSendEmail.New
    .SendAsync(
    procedure(AErro: Boolean; AMessageErro: string)
    begin
      if AErro then
        ShowMessage(AMessageErro)
      else
        ShowMessage('Message sent!')
    end, True);
end;

procedure TFormSendEmail.btnSendClick(Sender: TObject);
begin
  PageControlSendEmail.ActivePage := tabLog;

  LoadSettings;

  try
    TSendEmail.New.Send(True);
  except
    on E: exception do
    begin
      ShowMessage(E.Message);
      Exit;
    end;
  end;

  ShowMessage('E-mail sent');
end;

procedure TFormSendEmail.ConfigurationsDefault;
begin
  case ragLoadSettings.ItemIndex of
    0:
      begin // Gmail - Active https://myaccount.google.com/lesssecureapps
        edtHost.Text := 'smtp.gmail.com';
        edtPort.Text := '465';
        cmbEncrypted.ItemIndex := 3; // None SSL TSL SSL/TLS
        cmbAuth.ItemIndex := 1;      // No Yes
      end;

    1:
      begin // Outlook or Office 365
        edtHost.Text := 'smtp.office365.com';
        edtPort.Text := '587';
        cmbEncrypted.ItemIndex := 2; // None SSL TSL SSL/TLS
        cmbAuth.ItemIndex := 1;      // No Yes
      end;

    2:
      begin // Hotmail
        edtHost.Text := 'smtp.live.com';
        edtPort.Text := '587';
        cmbEncrypted.ItemIndex := 2; // None SSL TSL SSL/TLS
        cmbAuth.ItemIndex := 1;      // No Yes
      end;

    3:
      begin // Yahoo
        edtHost.Text := 'smtp.mail.yahoo.com.br';
        edtPort.Text := '587';
        cmbEncrypted.ItemIndex := 2; // None SSL TSL SSL/TLS
        cmbAuth.ItemIndex := 1;      // No Yes
      end;

    4:
      begin // SendGrid
        edtHost.Text := 'smtp.sendgrid.net';
        edtPort.Text := '465';
        cmbEncrypted.ItemIndex := 2; // None SSL TSL SSL/TLS
        cmbAuth.ItemIndex := 1;      // No Yes
      end;

    5:
      begin // Localweb
        edtHost.Text := 'email-ssl.com.br';
        edtPort.Text := '465';
        cmbEncrypted.ItemIndex := 2; // None SSL TSL SSL/TLS
        cmbAuth.ItemIndex := 1;      // No Yes
      end;

    6:
      begin // SparkPost
        edtHost.Text := 'smtp.sparkpostmail.com';
        edtPort.Text := '587';
        cmbEncrypted.ItemIndex := 2; // None SSL TSL SSL/TLS
        cmbAuth.ItemIndex := 1;      // No Yes
      end;

    7:
      begin
        edtHost.Text := 'smtp.elasticemail.com';
        edtPort.Text := '587';
        cmbEncrypted.ItemIndex := 0; // None SSL TSL SSL/TLS
        cmbAuth.ItemIndex := 1;      // No Yes
      end;

    8:
      begin
        edtHost.Text := 'smtp.mail.ru';
        edtPort.Text := '465';
        cmbEncrypted.ItemIndex := 2; // None SSL TSL SSL/TLS
        cmbAuth.ItemIndex := 1;      // No Yes
      end;
  end;
end;

procedure TFormSendEmail.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  ConfigurationsDefault;
  PageControlSendEmail.ActivePage := tabConfiguration;
end;

procedure TFormSendEmail.LoadSettings;
var
  I: Integer;
begin
  TSendEmail.New
    .OnLog(
    procedure(ALog: string)
    begin
      memoLog.Lines.Add(Format('%s ' + ALog, [FormatDateTime('dd/mm/yyyy hh:MM:ss', Now)]));
    end, TLogMode(cmbLogMode.ItemIndex))

    .OnWorkBegin(
    procedure(ACountMax: Int64)
    begin
      ProgressBar.Max := ACountMax;
      ProgressBar.Position := 0;
      ProgressBar.Refresh;
    end)

    .OnWork(
    procedure(ACount: Int64)
    begin
      ProgressBar.Position := ACount;
      ProgressBar.Refresh;
    end)

    .OnWorkEnd(
    procedure
    begin
      ProgressBar.Position := ProgressBar.Max;
      ProgressBar.Refresh;
    end)

  // Recipient
    .From(edtFrom.Text, edtFromName.Text)
    .AddTo(edtTo.Text, edtToName.Text)
    .AddCC(edtCc.Text, edtCcName.Text)
    .AddBCC(edtBcc.Text, edtBccName.Text)
    .Priority(TPriority(cmbPriority.ItemIndex))
    .Subject(edtSubject.Text)
    .Message(mmMessage.Text);

  if chkReceiptRecipient.Checked then
    TSendEmail.New.AddReceiptRecipient(edtFrom.Text, edtFromName.Text);

  // Add Attachment
  for I := 0 to Pred(lbAttachment.Count) do
    TSendEmail.New.AddAttachment(lbAttachment.Items.Strings[I]);

  // Configuration SMTP
  TSendEmail.New
    .Host(edtHost.Text)
    .Port(StrToIntDef(edtPort.Text, 587))
    .Auth(cmbAuth.ItemIndex = 1)
    .UserName(edtUser.Text)
    .Password(edtPassword.Text)
    .SSL((cmbEncrypted.ItemIndex = 1) or (cmbEncrypted.ItemIndex = 3))
    .TLS((cmbEncrypted.ItemIndex = 2) or (cmbEncrypted.ItemIndex = 3))
end;

procedure TFormSendEmail.pnlInfoClick(Sender: TObject);
var
  LURL: string;
begin
  LURL := pnlInfo.Caption;
  LURL := LURL.Replace('GITHUB: ', '').Replace(' ', '');

  ShellExecute(0, 'open', PChar(LURL), nil, nil, SW_SHOWNORMAL);
end;

procedure TFormSendEmail.ragLoadSettingsClick(Sender: TObject);
begin
  ConfigurationsDefault;
end;

procedure TFormSendEmail.Remover1Click(Sender: TObject);
begin
  lbAttachment.Items.Delete(lbAttachment.ItemIndex);
end;

end.
