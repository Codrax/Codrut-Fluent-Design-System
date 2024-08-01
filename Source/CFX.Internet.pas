unit CFX.Internet;

interface

uses
  Winapi.Windows, Winapi.Messages, Classes, System.SysUtils, System.UITypes,
  Types, Vcl.Forms, Vcl.Graphics, CFX.Colors, CFX.Registry, ShellAPI,
  CFX.Types, IOUTils, IdHTTP, UrlMon;

function DownloadFile(Source, Destination: string): Boolean;

implementation

function DownloadFile(Source, Destination: string): Boolean;
var
  IdHTTP1: TIdHTTP;
  FileStream: TFileStream;
begin
  try
    // Attempt 1 - IDHTTP
    IdHTTP1 := TIdHTTP.Create(nil);
    FileStream := TFileStream.Create(Destination, fmCreate);
    try
      IdHTTP1.Get(Source, FileStream);

      Result := TFile.Exists(Destination);
    finally
      IdHTTP1.Free;
      FileStream.Free;
    end;
  except
    // Attempt 2 - UrlMon
    try
      Result := UrlDownloadToFile( nil, PChar(source), PChar( Destination ) , 0, nil ) = 0;
    except
      // Failure
      Result := False;
    end;
  end;
end;

end.
