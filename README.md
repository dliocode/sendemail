<p align="center">
  <a href="https://user-images.githubusercontent.com/54585337/123355484-2296e300-d53c-11eb-8d87-698741a9def5.png">
    <img alt="datalogger" src="https://user-images.githubusercontent.com/54585337/123355484-2296e300-d53c-11eb-8d87-698741a9def5.png">
  </a>  
</p>
<br>
<p align="center">
  <img src="https://img.shields.io/github/v/release/dliocode/sendemail?style=flat-square">
  <img src="https://img.shields.io/github/stars/dliocode/sendemail?style=flat-square">
  <img src="https://img.shields.io/github/forks/dliocode/sendemail?style=flat-square">
  <img src="https://img.shields.io/github/contributors/dliocode/sendemail?color=orange&style=flat-square">
  <img src="https://tokei.rs/b1/github/dliocode/sendemail?color=red&category=lines">
  <img src="https://tokei.rs/b1/github/dliocode/sendemail?color=green&category=code">
  <img src="https://tokei.rs/b1/github/dliocode/sendemail?color=yellow&category=files">
</p>

## SendEmail

Support: developer.dlio@gmail.com

Function to send email for Delphi.

:fast_forward: Here is an example [Samples](https://github.com/dliocode/SendEmail/tree/main/samples)


### Donation

If this project help you reduce time to develop, you can give me a cup of coffee :) <a href="https://www.paypal.com/donate?hosted_button_id=2T7W4PL7YGJZW" target="_blank" rel="noopener noreferrer"><img width="150" src="https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif"></a>


## :warning: Usage

### :green_book: Mode: Simple

```delphi
uses
  SendEmail;

begin
  TSendEmail.New
    .From('Email', 'Name')
    .AddTo('Email', 'Name')
    .AddReceiptRecipient('Email', 'Name') // Confirmation Read
    .AddReplyTo('Email', 'Name')          // Answer to
    .AddCC('Email', 'Name')
    .AddBCC('Email', 'Name')
    .Priority(TPriority.mpNormal)
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
      Writeln(Format('%s ' + ALog, [FormatDateTime('dd/mm/yyyy hh:MM:ss', Now)]));
    end,
    TLogMode.lmLib) // Options: lmComponent, lmLib, lmAll, lmNone
    .From('Email', 'Name')
    .AddTo('Email', 'Name')
    .AddReceiptRecipient('Email', 'Name') // Confirmation Read
    .AddReplyTo('Email', 'Name')          // Answer to
    .AddCC('Email', 'Name')
    .AddBCC('Email', 'Name')
    .Priority(TPriority.mpNormal)
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
    .From('Email', 'Name')
    .AddTo('Email', 'Name')
    .AddReceiptRecipient('Email', 'Name') // Confirmation Read
    .AddReplyTo('Email', 'Name')          // Answer to
    .AddCC('Email', 'Name')
    .AddBCC('Email', 'Name')
    .Priority(TPriority.mpNormal)
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

### :notebook_with_decorative_cover: Send asynchronous mode

```delphi
uses
  SendEmail;

begin
  TSendEmail.New
    .From('Email', 'Name')
    .AddTo('Email', 'Name')
    .AddReceiptRecipient('Email', 'Name') // Confirmation Read
    .AddReplyTo('Email', 'Name')          // Answer to
    .AddCC('Email', 'Name')
    .AddBCC('Email', 'Name')
    .Priority(TPriority.mpNormal)
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
    procedure(AErro: Boolean; AMessageErro: string) // Informed callback to return
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

