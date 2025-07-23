program FluentUXDemo;

uses
  Vcl.Forms,
  CFX.ThemeManager,
  CFXDemo in 'CFXDemo.pas' {Form1};

{$R *.res}
begin
  Application.Initialize;

  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
