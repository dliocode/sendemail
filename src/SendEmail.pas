unit SendEmail;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes, System.TypInfo, System.Threading, System.SyncObjs,
  IdSMTP, IdSSL, IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdExplicitTLSClientServerBase, IdIOHandler, IdMessage, IdMessageBuilder, idGlobal, IdComponent, IdAttachmentFile;

type
  TPriority = IdMessage.TIdMessagePriority;
  TAttachmentDisposition = (adAttachment, adInline);
  TLogMode = (lmComponent, lmLib, lmAll, lmNone);

  TSendEmail = class
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

    class var RW: TMultiReadExclusiveWriteSynchronizer;
    class var FInstance: TSendEmail;

    function IsConnected: Boolean;
    procedure Reconnect(AResend: Boolean = False);

    procedure LogSMTPStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure LogSSLStatus(const AMsg: string);
    procedure Log(const ALog: string; const AForced: Boolean = False);

    procedure WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
  public
    function From(const AEmail: string; const AName: string = ''): TSendEmail;
    function AddTo(const AEmail: string; const AName: string = ''): TSendEmail;
    function AddReceiptRecipient(const AEmail: string; const AName: string = ''): TSendEmail;
    function AddReplyTo(const AEmail: string; const AName: string = ''): TSendEmail;
    function AddCC(const AEmail: string; const AName: string = ''): TSendEmail;
    function AddBCC(const AEmail: string; const AName: string = ''): TSendEmail;
    function Priority(const APriority: TPriority): TSendEmail;
    function Subject(const ASubject: string): TSendEmail;
    function Message(const AMessage: string; const IsBodyHTML: Boolean = True): TSendEmail;
    function AddAttachment(const AFileName: string; const ADisposition: TAttachmentDisposition = adInline): TSendEmail;
    function Host(const AHost: string): TSendEmail;
    function Port(const APort: Integer): TSendEmail;
    function Auth(const AValue: Boolean): TSendEmail;
    function UserName(const AUserName: string): TSendEmail;
    function Password(const APassword: string): TSendEmail;
    function SSL(const AValue: Boolean): TSendEmail;
    function TLS(const AValue: Boolean): TSendEmail;
    function Clear: TSendEmail;
    function ClearRecipient: TSendEmail;
    function Connect: TSendEmail;
    function Send(const ADisconnectAfterSending: Boolean = True): TSendEmail;
    function SendAsync(const ACallBack: TProc<Boolean, string> = nil; const ADisconnectAfterSending: Boolean = True): TSendEmail;
    function Disconnect: TSendEmail;

    function OnLog(const AExecute: TProc<string>; const ALogMode: TLogMode = lmComponent): TSendEmail;
    function OnWorkBegin(const AExecute: TProc<Int64>): TSendEmail;
    function OnWork(const AExecute: TProc<Int64>): TSendEmail;
    function OnWorkEnd(const AExecute: TProc): TSendEmail;

    constructor Create;
    destructor Destroy; override;

    class function New: TSendEmail;
    class destructor UnInitialize;
  end;

implementation

{ TSendEmail }

class function TSendEmail.New: TSendEmail;
begin
  if not Assigned(FInstance) then
    FInstance := TSendEmail.Create;

  Result := FInstance;
end;

class destructor TSendEmail.UnInitialize;
begin
  if Assigned(FInstance) then
    try
      FInstance.OnLog(nil);
      FInstance.Disconnect;
    finally
      FreeAndNil(FInstance);
    end;
end;

constructor TSendEmail.Create;
begin
  FIdSMTP := TIdSMTP.Create;
  FIdSSLOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create;
  FidMessageBuilderHTML := TIdMessageBuilderHtml.Create;
  FIdMessage := TIdMessage.Create;
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
    MailAgent := 'SendEmail';
    ConnectTimeout := 60000;
    ReadTimeout := 60000;
    UseEhlo := True;
    HeloName := 'SendEmail';
    OnStatus := LogSMTPStatus;
    OnWorkBegin := WorkBegin;
    OnWork := Work;
    OnWorkEnd := WorkEnd;
    ManagedIOHandler := True;
    PipeLine := False;
  end;

  with FIdSSLOpenSSL do
  begin
    ConnectTimeout := 100000;
    ReadTimeout := 100000;
    PassThrough := True;
    SSLOptions.SSLVersions := [SslvSSLv2, SslvSSLv23, SslvSSLv3, SslvTLSv1, SslvTLSv1_1, SslvTLSv1_2];
    SSLOptions.Mode := SslmBoth;
    SSLOptions.VerifyMode := [];
    SSLOptions.VerifyDepth := 0;
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

function TSendEmail.From(const AEmail, AName: string): TSendEmail;
begin
  Result := Self;

  if AEmail.Trim.IsEmpty then
    Exit;

  if IsConnected then
    if not FIdMessage.From.Address.Contains(AEmail) then
      Disconnect
    else
      Exit;

  FIdMessage.From.Name := AName;
  FIdMessage.From.Address := AEmail;

  Log(Format('From: %s', [AEmail]));
end;

function TSendEmail.AddTo(const AEmail, AName: string): TSendEmail;
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

function TSendEmail.AddReceiptRecipient(const AEmail, AName: string): TSendEmail;
begin
  Result := Self;

  if AEmail.Trim.IsEmpty then
    Exit;

  FIdMessage.ReceiptRecipient.Name := AName;
  FIdMessage.ReceiptRecipient.Address := AEmail;

  Log(Format('ReceiptRecipient: %s', [AEmail]));
end;

function TSendEmail.AddReplyTo(const AEmail, AName: string): TSendEmail;
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

function TSendEmail.AddCC(const AEmail, AName: string): TSendEmail;
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

function TSendEmail.AddBCC(const AEmail, AName: string): TSendEmail;
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

function TSendEmail.Priority(const APriority: TPriority): TSendEmail;
begin
  Result := Self;

  if FIdMessage.Priority = APriority then
    Exit;

  FIdMessage.Priority := APriority;

  Log(Format('Priority: %s', [GetEnumName(TypeInfo(TPriority), Integer(APriority))]));
end;

function TSendEmail.Subject(const ASubject: string): TSendEmail;
begin
  Result := Self;

  if not FIdMessage.Subject.Contains(ASubject) then
    FIdMessage.Subject := ASubject
  else
    Exit;

  Log(Format('Subject: %s', [ASubject]));
end;

function TSendEmail.Message(const AMessage: string; const IsBodyHTML: Boolean = True): TSendEmail;
begin
  Result := Self;

  if IsBodyHTML then
  begin
    FidMessageBuilderHTML.Html.Text := AMessage;
    FidMessageBuilderHTML.HtmlCharSet := 'utf-8';
    FidMessageBuilderHTML.HtmlContentTransfer := 'base64'; // quoted-printable
  end
  else
  begin
    FidMessageBuilderHTML.PlainText.Text := AMessage;
    FidMessageBuilderHTML.PlainTextCharSet := 'utf-8';
    FidMessageBuilderHTML.PlainTextContentTransfer := 'base64'; // quoted-printable
  end;

  Log(Format('Message: %s', [IfThen(IsBodyHTML, 'HTML', 'PlainText')]));
end;

function TSendEmail.AddAttachment(const AFileName: string; const ADisposition: TAttachmentDisposition = adInline): TSendEmail;
begin
  Result := Self;

  if AFileName.Trim.IsEmpty then
    Exit;

  if not FileExists(AFileName) then
  begin
    Log('Attachment: Not found');
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

function TSendEmail.Host(const AHost: string): TSendEmail;
begin
  Result := Self;

  if IsConnected then
    if not FIdSMTP.Host.Contains(AHost) then
      Disconnect
    else
      Exit;

  FIdSMTP.Host := AHost;

  Log(Format('Host: %s', [AHost]));
end;

function TSendEmail.Port(const APort: Integer): TSendEmail;
begin
  Result := Self;

  if IsConnected then
    if not FIdSMTP.Port = APort then
      Disconnect
    else
      Exit;

  FIdSMTP.Port := APort;

  Log(Format('Port: %d', [APort]));
end;

function TSendEmail.Auth(const AValue: Boolean): TSendEmail;
begin
  Result := Self;

  if AValue then
    FIdSMTP.AuthType := satDefault
  else
    FIdSMTP.AuthType := satNone;

  Log(Format('Auth: %s', [IfThen(AValue, 'True', 'False')]));
end;

function TSendEmail.UserName(const AUserName: string): TSendEmail;
begin
  Result := Self;

  if IsConnected then
    if not FIdSMTP.UserName.Contains(AUserName) then
      Disconnect
    else
      Exit;

  FIdSMTP.UserName := AUserName;

  Log(Format('UserName: %s', [AUserName]));
end;

function TSendEmail.Password(const APassword: string): TSendEmail;
begin
  Result := Self;

  if IsConnected then
    if not FIdSMTP.Password.Contains(APassword) then
      Disconnect
    else
      Exit;

  FIdSMTP.Password := APassword;

  Log(Format('Password: %s', ['********']));
end;

function TSendEmail.SSL(const AValue: Boolean): TSendEmail;
begin
  Result := Self;
  FSSL := AValue;
end;

function TSendEmail.TLS(const AValue: Boolean): TSendEmail;
begin
  Result := Self;
  FTLS := AValue;
end;

function TSendEmail.Clear: TSendEmail;
begin
  Result := Self;

  Priority(mpNormal);
  FIdMessage.ClearHeader;
  FIdMessage.Body.Clear;
  FIdMessage.MessageParts.Clear;
  FidMessageBuilderHTML.Clear;
end;

function TSendEmail.ClearRecipient: TSendEmail;
begin
  Result := Self;

  Priority(mpNormal);
  FIdMessage.Recipients.Clear;
  FIdMessage.ReceiptRecipient.Text := '';
  FIdMessage.ReplyTo.Clear;
  FIdMessage.CCList.Clear;
  FIdMessage.BCCList.Clear;
  FIdMessage.Subject := '';
  FIdMessage.MessageParts.Clear;
  FidMessageBuilderHTML.Clear;
end;

function TSendEmail.Connect: TSendEmail;
var
  LLastResult: string;
begin
  Result := Self;

  if IsConnected then
    Exit;

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

    if MatchText(FIdSMTP.Port.ToString, ['25', '587', '2587']) then
      FIdSMTP.UseTLS := utUseExplicitTLS
    else
      FIdSMTP.UseTLS := utUseImplicitTLS;
  end
  else
  begin
    Log('Defining encryption: None');
    FIdSMTP.IOHandler := TIdIOHandler.MakeDefaultIOHandler(nil);
    FIdSMTP.UseTLS := UtNoTLSSupport;
  end;

  try
    Log('Connecting');
    FIdSMTP.Connect;
    Log('Connected');

    if FIdSMTP.AuthType <> satNone then
    begin
      Log('Authenticating');
      FIdSMTP.Authenticate;
      Log('Authenticated');
    end;

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
            Reconnect(False);
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

        if LLastResult.Contains(E.Message) and not LLastResult.Trim.IsEmpty then
          raise Exception.Create(LLastResult + sLineBreak + 'Message: ' + E.Message)
        else
          raise Exception.Create(E.Message);
      except
        on E: Exception do
        begin
          Log('Except: ' + E.Message, True);
          Log('Email not connected!');
          raise;
        end;
      end;
    end;
  end;
end;

function TSendEmail.Send(const ADisconnectAfterSending: Boolean = True): TSendEmail;
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
        Log('Except: ' + E.Message, True);

        if
          E.Message.ToUpper.Contains('CLOSING CONNECTION') or
          E.Message.ToUpper.Contains('TOO MANY MESSAGES')
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
          raise Exception.Create('Not connected to internet!');

        if
          E.Message.ToUpper.Contains('NO SUCH USER HERE') or
          E.Message.ToUpper.Contains('USER UNKNOWN') or
          E.Message.ToUpper.Contains('MAILBOX UNAVAILABLE')
        then
          raise Exception.Create('The recipient''s mailbox does not exist in the destination domain. It was probably typed incorrectly!');

        if
          E.Message.ToUpper.Contains('MAILBOX IS FULL') or
          E.Message.ToUpper.Contains('MAIL QUOTA EXCEEDED') or
          E.Message.ToUpper.Contains('MAILBOX FULL') or
          E.Message.ToUpper.Contains('DISK QUOTA EXCEEDED') or
          E.Message.ToUpper.Contains('USER IS OVER THE QUOTA')
        then
          raise Exception.Create('It means that the recipient''s inbox is full and cannot receive any more messages!');

        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if ADisconnectAfterSending then
      Disconnect;
  end;
end;

function TSendEmail.SendAsync(const ACallBack: TProc<Boolean, string> = nil; const ADisconnectAfterSending: Boolean = True): TSendEmail;
begin
  Result := Self;

  TThread.CreateAnonymousThread(
    procedure
    var
      LMessage: string;
    begin
      RW.BeginWrite;
      try
        try
          Send(ADisconnectAfterSending);
        except
          on E: Exception do
            LMessage := E.Message;
        end;
      finally
        RW.EndWrite;
      end;

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          if Assigned(ACallBack) then
            ACallBack(not LMessage.Trim.IsEmpty, LMessage)
          else
            raise Exception.Create(LMessage);
        end);
    end).Start;
end;

function TSendEmail.Disconnect: TSendEmail;
begin
  Result := Self;

  if IsConnected then
    try
      Log('Disconnecting');
      FIdSMTP.Disconnect(False);
      Log('Disconnected');
    except
      Log('Except: Disconnected with error');
    end;

  if FSSL or FTLS then
  begin
    Log('UnLoading DLL');
    UnLoadOpenSSLLibrary;
    Log('UnLoaded DLL');
  end;
end;

function TSendEmail.OnLog(const AExecute: TProc<string>; const ALogMode: TLogMode = lmComponent): TSendEmail;
begin
  Result := Self;

  FLogExecute := AExecute;
  FLogMode := ALogMode;
end;

function TSendEmail.OnWorkBegin(const AExecute: TProc<Int64>): TSendEmail;
begin
  Result := Self;
  FWorkBegin := AExecute;
end;

function TSendEmail.OnWork(const AExecute: TProc<Int64>): TSendEmail;
begin
  Result := Self;
  FWork := AExecute;
end;

function TSendEmail.OnWorkEnd(const AExecute: TProc): TSendEmail;
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
    on E: Exception do
    begin
      if E.Message.ToUpper.Contains('CLOSING CONNECTION') or
        E.Message.ToUpper.Contains('SSL3_GET_RECORD')
      then
        try
          Reconnect(False);
          Result := True;
        except
          Exit;
        end;
    end;
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
    FLogExecute('SMTP: ' + AStatusText);
end;

procedure TSendEmail.LogSSLStatus(const AMsg: string);
begin
  if Assigned(FLogExecute) and (FLogMode in [lmComponent, lmAll]) then
    FLogExecute('SSL: ' + AMsg.ToUpper.Replace('SSL STATUS: ', ''));
end;

procedure TSendEmail.Log(const ALog: string; const AForced: Boolean = False);
begin
  if Assigned(FLogExecute) and ((FLogMode in [lmLib, lmAll]) or AForced) and not(FLogMode = lmNone) then
    FLogExecute('LIB: ' + ALog);
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

initialization

TSendEmail.RW := TMultiReadExclusiveWriteSynchronizer.Create;

finalization

FreeAndNil(TSendEmail.RW);

end.