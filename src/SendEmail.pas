unit SendEmail;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes,
  IdSMTP, IdSSL, IdSSLOpenSSL, IdExplicitTLSClientServerBase, IdMessage, IdMessageBuilder, idGlobal, IdComponent, IdAttachmentFile,
  System.TypInfo;

type
  TPriority = IdMessage.TIdMessagePriority;
  TAttachmentDisposition = (adAttachment, adInline);
  TLogMode = (lmComponent, lmLib, lmAll, lmNone);

  ISendEmail = interface
    ['{2B186D65-3B24-4D1B-B47F-9B145B59CD1F}']
    function From(const AEmail: string; const AName: string = ''): ISendEmail;
    function AddTo(const AEmail: string; const AName: string = ''): ISendEmail;
    function AddReceiptRecipient(const AEmail: string; const AName: string = ''): ISendEmail;
    function AddReplyTo(const AEmail: string; const AName: string = ''): ISendEmail;
    function AddCC(const AEmail: string; const AName: string = ''): ISendEmail;
    function AddBCC(const AEmail: string; const AName: string = ''): ISendEmail;
    function Priority(const APriority: TPriority): ISendEmail;
    function Subject(const ASubject: string): ISendEmail;
    function AddBody(const ABody: string; const IsBodyHTML: Boolean = True): ISendEmail;
    function AddAttachment(const AFileName: string; const ADisposition: TAttachmentDisposition = adInline): ISendEmail;
    function Host(const AHost: string): ISendEmail;
    function Port(const APort: Integer): ISendEmail;
    function Auth(const AValue: Boolean): ISendEmail;
    function UserName(const AUserName: string): ISendEmail;
    function Password(const APassword: string): ISendEmail;
    function SSL(const AValue: Boolean): ISendEmail;
    function TLS(const AValue: Boolean): ISendEmail;
    function Clear: ISendEmail;
    function Connect: ISendEmail;
    function Send(const ADisconnectAfterSending: Boolean = True): ISendEmail;
    function Disconnect: ISendEmail;
    function OnLog(const AStatus: TProc<string>; const ALogMode: TLogMode = lmComponent): ISendEmail;
    function OnWorkBegin(const AExecute: TProc<Int64>): ISendEmail;
    function OnWork(const AExecute: TProc<Int64>): ISendEmail;
    function OnWorkEnd(const AExecute: TProc): ISendEmail;
  end;

  TSendEmail = class(TInterfacedObject, ISendEmail)
  private
    FIdSMTP: TIdSMTP;
    FIdSSLOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
    FIdMessage: TIdMessage;
    FidMessageBuilderHTML: TIdMessageBuilderHtml;
    FSSL: Boolean;
    FTLS: Boolean;
    FLogExecute: TProc<string>;
    FLogMode: TLogMode;
    FWorkBegin: TProc<Int64>;
    FWork: TProc<Int64>;
    FWorkEnd: TProc;
    FMessageStream: TMemoryStream;

    FConnectMaxReconnection: Integer;
    FConnectCountReconnect: Integer;
    FSendMaxReconnection: Integer;
    FSendCountReconnect: Integer;

    function IsConnected: Boolean;
    procedure Reconnect(AResend: Boolean = False);

    procedure LogSMTPStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure LogSSLStatus(const AMsg: string);
    procedure Log(const ALog: string; const AForced: Boolean = False);

    procedure WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
  public
    function From(const AEmail: string; const AName: string = ''): ISendEmail;
    function AddTo(const AEmail: string; const AName: string = ''): ISendEmail;
    function AddReceiptRecipient(const AEmail: string; const AName: string = ''): ISendEmail;
    function AddReplyTo(const AEmail: string; const AName: string = ''): ISendEmail;
    function AddCC(const AEmail: string; const AName: string = ''): ISendEmail;
    function AddBCC(const AEmail: string; const AName: string = ''): ISendEmail;
    function Priority(const APriority: TPriority): ISendEmail;
    function Subject(const ASubject: string): ISendEmail;
    function AddBody(const ABody: string; const IsBodyHTML: Boolean = True): ISendEmail;
    function AddAttachment(const AFileName: string; const ADisposition: TAttachmentDisposition = adInline): ISendEmail;
    function Host(const AHost: string): ISendEmail;
    function Port(const APort: Integer): ISendEmail;
    function Auth(const AValue: Boolean): ISendEmail;
    function UserName(const AUserName: string): ISendEmail;
    function Password(const APassword: string): ISendEmail;
    function SSL(const AValue: Boolean): ISendEmail;
    function TLS(const AValue: Boolean): ISendEmail;
    function Clear: ISendEmail;
    function Connect: ISendEmail;
    function Send(const ADisconnectAfterSending: Boolean = True): ISendEmail;
    function Disconnect: ISendEmail;
    function OnLog(const AStatus: TProc<string>; const ALogMode: TLogMode = lmComponent): ISendEmail;
    function OnWorkBegin(const AExecute: TProc<Int64>): ISendEmail;
    function OnWork(const AExecute: TProc<Int64>): ISendEmail;
    function OnWorkEnd(const AExecute: TProc): ISendEmail;

    constructor Create;
    destructor Destroy; override;

    class function New: ISendEmail;
  end;

implementation

{ TSendEmail }

class function TSendEmail.New: ISendEmail;
begin
  Result := TSendEmail.Create;
end;

constructor TSendEmail.Create;
begin
  FIdSMTP := TIdSMTP.Create(nil);
  FIdSSLOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(FIdSMTP);
  FidMessageBuilderHTML := TIdMessageBuilderHtml.Create;
  FIdMessage := TIdMessage.Create(nil);
  FMessageStream := TMemoryStream.Create;

  FLogExecute := nil;
  FLogMode := lmNone;

  FWorkBegin := nil;
  FWork := nil;
  FWorkEnd := nil;

  FConnectMaxReconnection := 5;
  FConnectCountReconnect := 0;

  FSendMaxReconnection := 5;
  FSendCountReconnect := 0;

  Auth(True);
  SSL(False);
  TLS(False);
  Priority(mpNormal);
  Port(587);

  with FIdSMTP do
  begin
    ConnectTimeout := 60000;
    ReadTimeout := 60000;
    HeloName := 'SendEmail';
    OnStatus := LogSMTPStatus;
    OnWorkBegin := WorkBegin;
    OnWork := Work;
    OnWorkEnd := WorkEnd;
  end;

  with FIdSSLOpenSSL do
  begin
    ConnectTimeout := 100000;
    ReadTimeout := 100000;
    SSLOptions.SSLVersions := [SslvSSLv2, SslvSSLv23, SslvSSLv3, SslvTLSv1, SslvTLSv1_1, SslvTLSv1_2];
    SSLOptions.Mode := SslmBoth;
    SSLOptions.VerifyMode := [];
    SSLOptions.VerifyDepth := 0;
    PassThrough := False;
    OnStatus := LogSMTPStatus;
    OnStatusInfo := LogSSLStatus;
  end;

  with FIdMessage do
  begin
    IsEncoded := True;
    UseNowForDate := True;
  end;
end;

destructor TSendEmail.Destroy;
begin
  FWorkBegin := nil;
  FWork := nil;
  FWorkEnd := nil;
  FLogExecute := nil;

  FreeAndNil(FMessageStream);
  FreeAndNil(FidMessageBuilderHTML);
  FreeAndNil(FIdMessage);
  FreeAndNil(FIdSSLOpenSSL);
  FreeAndNil(FIdSMTP);

  inherited;
end;

function TSendEmail.From(const AEmail, AName: string): ISendEmail;
begin
  Result := Self;

  if AEmail.Trim.IsEmpty then
    Exit;

  FIdMessage.From.Name := AName;
  FIdMessage.From.Address := AEmail;

  Log(Format('From: %s', [AEmail]));
end;

function TSendEmail.AddTo(const AEmail, AName: string): ISendEmail;
begin
  Result := Self;

  if AEmail.Trim.IsEmpty then
    Exit;

  with FIdMessage.Recipients.Add do
  begin
    name := AName;
    Address := AEmail;
  end;

  Log(Format('To(%d): %s', [FIdMessage.Recipients.Count, AEmail]));
end;

function TSendEmail.AddReceiptRecipient(const AEmail, AName: string): ISendEmail;
begin
  Result := Self;

  if AEmail.Trim.IsEmpty then
    Exit;

  FIdMessage.ReceiptRecipient.Name := AName;
  FIdMessage.ReceiptRecipient.Address := AEmail;

  Log(Format('ReceiptRecipient: %s', [AEmail]));
end;

function TSendEmail.AddReplyTo(const AEmail, AName: string): ISendEmail;
begin
  Result := Self;

  if AEmail.Trim.IsEmpty then
    Exit;

  with FIdMessage.ReplyTo.Add do
  begin
    name := AName;
    Address := AEmail;
  end;

  Log(Format('ReplyTo(%d): %s', [FIdMessage.ReplyTo.Count, AEmail]));
end;

function TSendEmail.AddCC(const AEmail, AName: string): ISendEmail;
begin
  Result := Self;

  if AEmail.Trim.IsEmpty then
    Exit;

  with FIdMessage.CCList.Add do
  begin
    name := AName;
    Address := AEmail;
  end;

  Log(Format('CC(%d): %s', [FIdMessage.CCList.Count, AEmail]));
end;

function TSendEmail.AddBCC(const AEmail, AName: string): ISendEmail;
begin
  Result := Self;

  if AEmail.Trim.IsEmpty then
    Exit;

  with FIdMessage.BCCList.Add do
  begin
    name := AName;
    Address := AEmail;
  end;

  Log(Format('BCC(%d): %s', [FIdMessage.BCCList.Count, AEmail]));
end;

function TSendEmail.Priority(const APriority: TPriority): ISendEmail;
begin
  Result := Self;
  FIdMessage.Priority := APriority;

  Log(Format('Priority: %s', [GetEnumName(TypeInfo(TPriority), Integer(APriority))]));
end;

function TSendEmail.Subject(const ASubject: string): ISendEmail;
begin
  Result := Self;
  FIdMessage.Subject := ASubject;

  Log(Format('Subject: %s', [ASubject]));
end;

function TSendEmail.AddBody(const ABody: string; const IsBodyHTML: Boolean = True): ISendEmail;
begin
  Result := Self;

  if IsBodyHTML then
  begin
    FidMessageBuilderHTML.Html.Add(ABody);
    FidMessageBuilderHTML.HtmlCharSet := 'utf-8';
    FidMessageBuilderHTML.HtmlContentTransfer := 'base64'; // quoted-printable
  end
  else
  begin
    FidMessageBuilderHTML.PlainText.Add(ABody);
    FidMessageBuilderHTML.PlainTextCharSet := 'utf-8';
    FidMessageBuilderHTML.PlainTextContentTransfer := 'base64'; // quoted-printable
  end;
end;

function TSendEmail.AddAttachment(const AFileName: string; const ADisposition: TAttachmentDisposition = adInline): ISendEmail;
begin
  Result := Self;

  if AFileName.Trim.IsEmpty then
    Exit;

  if not FileExists(AFileName) then
  begin
    Log('Attachment not found');
    Exit;
  end;

  case ADisposition of
    adAttachment:
      begin
        FidMessageBuilderHTML.Attachments.Add(AFileName);
        Log(Format('Attachment(adAttachment)(%d): %s', [FidMessageBuilderHTML.Attachments.Count, ExtractFileName(AFileName)]));
      end;
    adInline:
      begin
        FidMessageBuilderHTML.HtmlFiles.Add(AFileName);
        Log(Format('Attachment(adInline)(%d): %s', [FidMessageBuilderHTML.HtmlFiles.Count, ExtractFileName(AFileName)]));
      end;
  end;
end;

function TSendEmail.Host(const AHost: string): ISendEmail;
begin
  Result := Self;
  FIdSMTP.Host := AHost;
  Log(Format('Host: %s', [AHost]));
end;

function TSendEmail.Port(const APort: Integer): ISendEmail;
begin
  Result := Self;
  FIdSMTP.Port := APort;
  Log(Format('Port: %d', [APort]));
end;

function TSendEmail.Auth(const AValue: Boolean): ISendEmail;
begin
  Result := Self;

  if AValue then
    FIdSMTP.AuthType := satDefault
  else
    FIdSMTP.AuthType := satNone;

  Log(Format('Auth: %s', [IfThen(AValue, 'True', 'False')]));
end;

function TSendEmail.UserName(const AUserName: string): ISendEmail;
begin
  Result := Self;
  FIdSMTP.UserName := AUserName;
  Log(Format('UserName: %s', [AUserName]));
end;

function TSendEmail.Password(const APassword: string): ISendEmail;
begin
  Result := Self;
  FIdSMTP.Password := APassword;
  Log(Format('Password: %s', ['********']));
end;

function TSendEmail.SSL(const AValue: Boolean): ISendEmail;
begin
  Result := Self;
  FSSL := AValue;
end;

function TSendEmail.TLS(const AValue: Boolean): ISendEmail;
begin
  Result := Self;
  FTLS := AValue;
end;

function TSendEmail.Clear: ISendEmail;
begin
  Result := Self;

  FIdMessage.ClearHeader;
  FIdMessage.Body.Clear;
  FIdMessage.MessageParts.Clear;
  FidMessageBuilderHTML.Clear;
end;

function TSendEmail.Connect: ISendEmail;
var
  LLastResult: string;
begin
  Result := Self;

  if FSSL or FTLS then
  begin
    if FSSL and FTLS then
      Log('Defining encryption: SSL/TLS')
    else
      if FSSL then
        Log('Defining encryption: SSL')
      else
        Log('Defining encryption: TLS');

    Log('Loading DLL');
    if not LoadOpenSSLLibrary then
    begin
      Log('DLL''s not compatible or not found (ssleay32 e libeay32)');
      raise Exception.Create('DLL''s not compatible or not found (ssleay32 e libeay32)');
    end;
    Log('Loaded DLL');

    with FIdSSLOpenSSL do
    begin
      if FSSL then
        SSLOptions.Method := SslvSSLv23;

      if FTLS then
        SSLOptions.Method := SslvTLSv1_2;

      Destination := FIdSMTP.Host + ':' + FIdSMTP.Port.ToString;
      Host := FIdSMTP.Host;
      Port := FIdSMTP.Port;
    end;

    FIdSMTP.IOHandler := FIdSSLOpenSSL;

    if (MatchText(FIdSMTP.Port.ToString, ['25', '587'])) then
      FIdSMTP.UseTLS := utUseExplicitTLS
    else
      FIdSMTP.UseTLS := utUseImplicitTLS;
  end
  else
  begin
    Log('Defining encryption: None');
    FIdSMTP.IOHandler := nil;
    FIdSMTP.UseTLS := UtNoTLSSupport;
  end;

  try
    Log('Connecting');
    FIdSMTP.Connect;
    Log('Connected');

    Log('Authenticating');
    FIdSMTP.Authenticate;
    Log('Authenticated');

    FConnectCountReconnect := 0;
  except
    on E: Exception do
    begin
      if FIdSMTP.LastCmdResult.ReplyExists then
      begin
        LLastResult := Format('Last Result: %s', [FIdSMTP.LastCmdResult.FormattedReply.Text]);

        if LLastResult.ToUpper.Contains('AUTHENTICATION SUCCEEDED') or LLastResult.Contains('250 OK') then
        begin
          Log(LLastResult, True);

          if FConnectCountReconnect < FConnectMaxReconnection then
          begin
            Sleep(100);
            Inc(FConnectCountReconnect);
            Reconnect;
            Exit;
          end
          else
          begin
            FConnectCountReconnect := 0;
            raise Exception.Create(E.Message);
          end;
        end;
      end;

      try
        if E.Message.ToUpper.Contains('INCORRECT AUTHENTICATION DATA') then
          raise Exception.Create('Incorrect authentication!');

        if E.Message.ToUpper.Contains('SOCKET ERROR # 10013') then
          raise Exception.Create('Firewall is blocking access to the internet!');

        if E.Message.ToUpper.Contains('SOCKET ERROR # 10054') then
          raise Exception.Create('Connection terminated!');

        if E.Message.ToUpper.Contains('SOCKET ERROR # 11001') then
          raise Exception.Create('Host not found!');

        if LLastResult.Contains(E.Message) then
          raise Exception.Create(E.Message)
        else
          raise Exception.Create(LLastResult + sLineBreak + 'Message: ' + E.Message);
      except
        on E: Exception do
        begin
          Log(E.Message, True);
          Log('Email not sent');
          raise;
        end;
      end;
    end;
  end;
end;

function TSendEmail.Send(const ADisconnectAfterSending: Boolean = True): ISendEmail;
begin
  Result := Self;

  if not IsConnected then
    Connect;

  try
    try
      Log('Sending email');
      FidMessageBuilderHTML.FillMessage(FIdMessage);
      FIdMessage.SaveToStream(FMessageStream);
      FIdSMTP.Send(FIdMessage);
      Log('Email sent');

      FSendCountReconnect := 0;
    except
      on E: Exception do
      begin
        Log(E.Message, True);

        if E.Message.ToUpper.Contains('CLOSING CONNECTION') or
          E.Message.ToUpper.Contains('TOO MANY MESSAGES IN THIS CONNECTION') or
          E.Message.ToUpper.Contains('SSL3_GET_RECORD')
        then
        begin
          if FSendCountReconnect < FSendMaxReconnection then
          begin
            Sleep(100);
            Inc(FSendCountReconnect);
            Reconnect(True);
            Exit;
          end
          else
          begin
            FSendCountReconnect := 0;
            raise Exception.Create(E.Message);
          end;
        end;

        if E.Message.ToUpper.Contains('NOT CONNECTED') then
          raise Exception.Create('Not Connected to Internet!');

        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if ADisconnectAfterSending and IsConnected then
      Disconnect;
  end;
end;

function TSendEmail.Disconnect: ISendEmail;
begin
  Result := Self;

  try
    Log('Disconnecting');
    FIdSMTP.Disconnect;
    Log('Disconnected');
  except
    Log('Disconnected with error');
  end;

  if FSSL or FTLS then
  begin
    Log('UnLoading DLL');
    UnLoadOpenSSLLibrary;
    Log('UnLoaded DLL');
  end;
end;

function TSendEmail.OnLog(const AStatus: TProc<string>; const ALogMode: TLogMode = lmComponent): ISendEmail;
begin
  Result := Self;

  FLogExecute := AStatus;
  FLogMode := ALogMode;
end;

function TSendEmail.OnWorkBegin(const AExecute: TProc<Int64>): ISendEmail;
begin
  Result := Self;
  FWorkBegin := AExecute;
end;

function TSendEmail.OnWork(const AExecute: TProc<Int64>): ISendEmail;
begin
  Result := Self;
  FWork := AExecute;
end;

function TSendEmail.OnWorkEnd(const AExecute: TProc): ISendEmail;
begin
  Result := Self;
  FWorkEnd := AExecute;
end;

function TSendEmail.IsConnected: Boolean;
begin
  Result := False;

  try
    Result := FIdSMTP.Connected;
  except
    Disconnect;
  end;
end;

procedure TSendEmail.Reconnect(AResend: Boolean = False);
begin
  if AResend then
    Log('Reconnecting: ' + FSendCountReconnect.ToString, True)
  else
    Log('Reconnecting: ' + FConnectCountReconnect.ToString, True);

  Disconnect;
  Connect;
  if AResend then
    Send;
end;

procedure TSendEmail.LogSMTPStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  if Assigned(FLogExecute) and (FLogMode in [lmComponent, lmAll]) then
    FLogExecute(AStatusText);
end;

procedure TSendEmail.LogSSLStatus(const AMsg: string);
begin
  if Assigned(FLogExecute) and (FLogMode in [lmComponent, lmAll]) then
    FLogExecute(AMsg);
end;

procedure TSendEmail.Log(const ALog: string; const AForced: Boolean = False);
begin
  if Assigned(FLogExecute) and ((FLogMode in [lmLib, lmAll]) or AForced) and not(FLogMode = lmNone) then
    FLogExecute(ALog);
end;

procedure TSendEmail.WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  Log('Starting transmission');

  if Assigned(FWorkBegin) and (AWorkMode = wmWrite) then
  begin
    FMessageStream.Position := 0;
    FWorkBegin(FMessageStream.Size);
  end;
end;

procedure TSendEmail.Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if Assigned(FWork) and (AWorkMode = wmWrite) then
    FWork(AWorkCount);
end;

procedure TSendEmail.WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  Log('Transmission completed');

  FMessageStream.Clear;
  if Assigned(FWorkEnd) then
    FWorkEnd;
end;

end.
