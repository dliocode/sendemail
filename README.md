<p align="center"><br>
  <img src="https://github.com/dliocode/SendEmail/blob/main/SendEmail.jpg"><br>
</p> 

## :mailbox: Send Email

![](https://img.shields.io/github/stars/dliocode/SendEmail.svg) ![](https://img.shields.io/github/forks/dliocode/SendEmail.svg) ![](https://img.shields.io/github/v/tag/dliocode/SendEmail.svg) ![](https://img.shields.io/github/release/dliocode/SendEmail.svg) ![](https://img.shields.io/github/issues/dliocode/SendEmail.svg)

Support: developer.dlio@gmail.com

Function to send email for Delphi.

:fast_forward: Here is an example [Samples](https://github.com/dliocode/SendEmail/tree/main/samples)

## :warning: Usage

### :green_book: Mode: Simple

```delphi
uses
  SendEmail;

begin
  TSendEmail.New
  .From('Email','Name')
  .AddTo('Email','Name')
  .AddReceiptRecipient('Email','Name') // Confirmation Read
  .AddReplyTo('Email','Name') // Answer to
  .AddCC('Email','Name')
  .AddBCC('Email','Name')
  .Priority(TIdMessagePriority.mpNormal)
  .Subject('My Text with SendEmail')
  .Message('<h1>Message</h1>', True) // True is Default = Text in HTML
  .AddAttachment('')
  .Host('email@domain.com')
  .Port(587)
  .Auth(True)
  .UserName('username')
  .Password('password')
  .SSL(False)
  .TLS(False)
  .Send(True); // True is Default = After sending it will be disconnected
end.
```

### :orange_book: Mode: With logger

```delphi
uses
  SendEmail;

begin
  TSendEmail.New
  .OnLog(
   procedure(ALog: string)
      begin
        memoLog.Lines.Add(Format('%s ' + ALog, [FormatDateTime('dd/mm/yyyy hh:MM:ss', Now)]));
      end,
      TLogMode.lmLib) // Options: lmComponent, lmLib, lmAll, lmNone  
  .From('Email','Name')
  .AddTo('Email','Name')
  .AddReceiptRecipient('Email','Name') // Confirmation Read
  .AddReplyTo('Email','Name') // Answer to
  .AddCC('Email','Name')
  .AddBCC('Email','Name')
  .Priority(TIdMessagePriority.mpNormal)
  .Subject('My Text with SendEmail')
  .Message('<h1>Message</h1>', True) // True is Default = Text in HTML
  .AddAttachment('')
  .Host('email@domain.com')
  .Port(587)
  .Auth(True)
  .UserName('username')
  .Password('password')
  .SSL(False)
  .TLS(False)
  .Send(True); // True is Default = After sending it will be disconnected
end. 
```

### :closed_book: Mode: With progress bar

```delphi
uses
  SendEmail;

begin
  TSendEmail.New  
  .OnWorkBegin(
  procedure(ACountMax: Int64)
  begin
	ProgressBar.Position := 0;     
	ProgressBar.Max := ACountMax;
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
  end)    
  .From('Email','Name')
  .AddTo('Email','Name')
  .AddReceiptRecipient('Email','Name') // Confirmation Read
  .AddReplyTo('Email','Name') // Answer to
  .AddCC('Email','Name')
  .AddBCC('Email','Name')
  .Priority(TIdMessagePriority.mpNormal)
  .Subject('My Text with SendEmail')
  .Message('<h1>Message</h1>', True) // True is Default = Text in HTML
  .AddAttachment('')
  .Host('email@domain.com')
  .Port(587)
  .Auth(True)
  .UserName('username')
  .Password('password')
  .SSL(False)
  .TLS(False)
  .Send(True) // True is Default = After sending it will be disconnected
end.
```

### :notebook_with_decorative_cover: Send asynchronous mode

```delphi
uses
  SendEmail;

begin
  TSendEmail.New
  .From('Email','Name')
  .AddTo('Email','Name')
  .AddReceiptRecipient('Email','Name') // Confirmation Read
  .AddReplyTo('Email','Name') // Answer to
  .AddCC('Email','Name')
  .AddBCC('Email','Name')
  .Priority(TIdMessagePriority.mpNormal)
  .Subject('My Text with SendEmail')
  .Message('<h1>Message</h1>', True) // True is Default = Text in HTML
  .AddAttachment('')
  .Host('email@domain.com')
  .Port(587)
  .Auth(True)
  .UserName('username')
  .Password('password')
  .SSL(False)
  .TLS(False)
  .SendAsync( 
    procedure(AErro: Boolean; AMessageErro: string)  // Informed callback to return
    begin
      if AErro then
        ShowMessage(AMessageErro)
      else
        ShowMessage('Message sent!')
    end, True); // True is Default = After sending it will be disconnected
end.
```


## :satellite: Host SMTP

| **Name** | **Host** | **Port** | **Cryptography** | **Auth** |
|  :---: |  :---: |  :---: |  :---: | :---: |
| Gmail  | smtp.gmail.com | 465 | SSL/TLS | Yes |
| Outlook or Office 365  | smtp.office365.com | 587 | TLS | Yes |
| Hotmail | smtp.live.com | 587 | TLS | Yes |
| Yahoo | smtp.mail.yahoo.com.br | 587 | TLS | Yes |
| SendGrid | smtp.sendgrid.net | 465 | TLS | Yes |
| LocalWeb | email-ssl.com.br | 465 | TLS | Yes |
| SparkPost | smtp.sparkpostmail.com | 587 | TLS | Yes |
| Elastic Email | smtp.elasticemail.com | 587 | None | Yes |
| Mail | smtp.mail.ru | 465  | SSL/TLS | Yes |

