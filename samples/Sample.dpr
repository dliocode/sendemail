program Sample;

uses
  Vcl.Forms,
  USendEmail in 'USendEmail.pas' {FormSendEmail},
  SendEmail in '..\src\SendEmail.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSendEmail, FormSendEmail);
  Application.Run;
end.
