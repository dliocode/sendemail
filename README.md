<p align="center"><br>
  <img src="https://github.com/dliocode/SendEmail/blob/master/SendEmail.jpg"><br>
</p>

## Send Email

![](https://img.shields.io/github/stars/dliocode/SendEmail.svg) ![](https://img.shields.io/github/forks/dliocode/SendEmail.svg) ![](https://img.shields.io/github/tag/dliocode/SendEmail.svg) ![](https://img.shields.io/github/release/dliocode/SendEmail.svg) ![](https://img.shields.io/github/issues/dliocode/SendEmail.svg)

Support: developer.dlio@gmail.com

Function to send email for Delphi.


Here is an example ![Samples](https://github.com/dliocode/SendEmail/tree/master/samples)

```delphi
uses
  SendEmail in '..\src\SendEmail.pas';

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
  .AddBody('<h1>Message</h1>', True) // True is Default = Text in HTML
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


With add logger.

```delphi
uses
  SendEmail in '..\src\SendEmail.pas';

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
  .AddBody('<h1>Message</h1>', True) // True is Default = Text in HTML
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

With add progressbar

```delphi
uses
  SendEmail in '..\src\SendEmail.pas';

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
  .AddBody('<h1>Message</h1>', True) // True is Default = Text in HTML
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