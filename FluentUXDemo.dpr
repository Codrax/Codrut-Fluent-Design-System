program FluentUXDemo;

uses
  Vcl.Forms,
  CFX.ThemeManager,
  CFXDemo in 'Demo\CFXDemo.pas' {Form1},
  CFX.Template in 'Source\CFX.Template.pas';

{$R *.res}
begin
  Application.Initialize;

  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
