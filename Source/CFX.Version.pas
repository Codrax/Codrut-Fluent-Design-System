{ Imported from Cod Library Pack }

unit CFX.Version;

interface
  uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Types, IdSNTP, Registry, DateUtils, IdHTTP, Math, JSON, IdSSLOpenSSL,
  CFX.Math, CFX.Constants;

type
  FXVersion = record
    Major,
    Minor,
    Maintenance,
    Build: cardinal;

    APIResponse: TJsonObject;

    // Main
    constructor Create(AMajor, AMinor, AMaintenance: cardinal; ABuild: cardinal=0);
    procedure Clear;

    // Load
    procedure Parse(From: string);
    procedure NetworkLoad(URL: string);
    procedure HtmlLoad(URL: string);
    procedure APILoad(AppName: string; Endpoint: string = DEFAULT_API); overload;
    procedure APILoad(AppName: string; Current: FXVersion; Endpoint: string = DEFAULT_API); overload;

    // Comparation
    function Empty: boolean;
    function CompareTo(Version: FXVersion): TValueRelationship;
    function NewerThan(Version: FXVersion): boolean;

    // Utils
    function GetDownloadLink(JSONValue: string = DEFAULT_UPDATE_NAME): string;

    // Conversion
    function ToString: string; overload;
    function ToString(IncludeBuild: boolean): string; overload;
    function ToString(Separator: char; IncludeBuild: boolean = false): string; overload;
  end;

  function MakeVersion(Major, Minor, Maintenance: cardinal; Build: cardinal = 0): FXVersion;

const
  VERSION_EMPTY: FXVersion = (Major:0; Minor:0; Maintenance:0; Build:0);

implementation

function MakeVersion(Major, Minor, Maintenance: cardinal; Build: cardinal = 0): FXVersion;
begin
  Result.Major := Major;
  Result.Minor := Minor;
  Result.Maintenance := Maintenance;
  Result.Build := Build;
end;


{ TVersionRec }

procedure FXVersion.NetworkLoad(URL: string);
var
  IdHttp: TIdHTTP;
  HTML: string;
begin
  IdHttp := TIdHTTP.Create(nil);
  try
    HTML := IdHttp.Get(URL);

    Parse(HTML);
  finally
    IdHttp.Free;
  end;
end;


function FXVersion.NewerThan(Version: FXVersion): boolean;
begin
  Result := CompareTo(Version) = GreaterThanValue;
end;

procedure FXVersion.APILoad(AppName, Endpoint: string);
begin
  APILoad(AppName, VERSION_EMPTY, EndPoint);
end;

procedure FXVersion.APILoad(AppName: string; Current: FXVersion;
  Endpoint: string);
var
  HTTP: TIdHTTP;
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  Request: TJSONObject;
  RequestStream: TStringStream;
  Result: string;
begin
  // Create HTTP and SSLIOHandler components
  HTTP := TIdHTTP.Create(nil);
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(HTTP);
  Request := TJSONObject.Create;

  // Build Request
  Request.AddPair('mode', 'getversion');
  Request.AddPair('app', AppName);
  if not Current.Empty then
    Request.AddPair('client-version', Current.ToString(true));

  // Request
  RequestStream := TStringStream.Create(Request.ToJSON, TEncoding.UTF8);
  try
    // Set SSL/TLS options
    SSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    HTTP.IOHandler := SSLIOHandler;

    // Set headers
    HTTP.Request.ContentType := 'application/json';

    // Send POST
    Result := HTTP.Post(Endpoint, RequestStream);

    // Parse
    APIResponse := TJSONObject.ParseJSONValue( Result ) as TJSONObject;

    // Parse response
    Parse(APIResponse.GetValue<string>('version'));
  finally
    // Free
    (* dont free APIResponse *)
    HTTP.Free;
    Request.Free;
    RequestStream.Free;
  end;
end;

procedure FXVersion.Clear;
begin
  Major := 0;
  Minor := 0;
  Maintenance := 0;
  Build := 0;
end;

function FXVersion.CompareTo(Version: FXVersion): TValueRelationship;
begin
  Result := CompareValue(Major, Version.Major);
  if Result <> EqualsValue then
    Exit;

  Result := CompareValue(Minor, Version.Minor);
  if Result <> EqualsValue then
    Exit;

  Result := CompareValue(Maintenance, Version.Maintenance);
  if Result <> EqualsValue then
    Exit;

  Result := CompareValue(Build, Version.Build);
end;

constructor FXVersion.Create(AMajor, AMinor, AMaintenance, ABuild: cardinal);
begin
  Major := AMajor;
  Minor := AMinor;
  Maintenance := AMaintenance;
  Build := ABuild;
end;

function FXVersion.Empty: boolean;
begin
  Result := CompareTo(VERSION_EMPTY) = EqualsValue;
end;

function FXVersion.GetDownloadLink(JSONValue: string): string;
begin
  if not APIResponse.TryGetValue<string>(JSONValue, Result) then
    Result := '';
end;

procedure FXVersion.HtmlLoad(URL: string);
var
  IdHttp: TIdHTTP;
  HTML: string;
begin
  IdHttp := TIdHTTP.Create(nil);
  try
    IdHttp.Request.CacheControl := 'no-cache';
    HTML := IdHttp.Get(URL);

    HTML := Trim(HTML).Replace(#13, '').DeQuotedString;

    Parse(HTML);
  finally
    IdHttp.Free;
  end;
end;

procedure FXVersion.Parse(From: string);
var
  Separator: char;
  Splitted: TArray<string>;
  I: Integer;
  Value: cardinal;
  AVersions: integer;
begin
  // Separator
  if From.IndexOf('.') <> -1 then
    Separator := '.'
  else
  if From.IndexOf(',') <> -1 then
    Separator := ','
  else
  if From.IndexOf('-') <> -1 then
    Separator := '-'
  else
    Separator := #0;

  // Values
  Splitted := From.Split(Separator);

  AVersions := Length(Splitted);
  if AVersions < 0 then
    Exit;

  // Write
  Clear;

  for I := 0 to AVersions-1 do
    begin
      Value := Splitted[I].ToInteger;
      case I of
        0: Major := Value;
        1: Minor := Value;
        2: Maintenance := Value;
        3: Build := Value;
      end;
    end;
end;

function FXVersion.ToString: string;
begin
  Result := ToString(false);
end;

function FXVersion.ToString(IncludeBuild: boolean): string;
begin
  Result := ToString('.', IncludeBuild);
end;

function FXVersion.ToString(Separator: char; IncludeBuild: boolean): string;
begin
  Result := Major.ToString + Separator + Minor.ToString + Separator + Maintenance.ToString;

  if IncludeBuild then
    Result := Result + Separator + Build.ToString;
end;

end.
