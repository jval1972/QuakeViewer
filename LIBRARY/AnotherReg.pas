(*
   AnotherReg
   Registry-aware Components Collection
   (C) Glen Why, 1996
   Version 01.04.00

Version History

   01.00.00 - first
   01.00.01 - Small bug in TVarinatProfile.SetValue fixed
   01.00.02 - TCustomKey.SetParentKey moved to the protected
              stub and made vitrual;

   01.01.00 - + TSoundEvent
   01.01.01 - Automatic search for the AppConfigKey added
              to the TSoundEvent.Create;
   01.01.02 - TSoundEvent.EventLabel property added
   01.01.03 - TSoundEvent.OnSoundEvent event added
   01.01.04 - 01.01.03's bugs fixed

   01.02.00 - + TStringProfile

   01.03.00 - + TComponentProfile

   01.04.00 - + TFormRestorer

notes:

* According to Win32 Developer's reference, NT's version
  of RegDeleteKey will fail if specified key has subkeys,
  as a Result ERegistryError will be raised on call to
  the Delete method of key that has subkey(s). Nothing
  to worry about in Windows 95.

*)
unit AnotherReg;

interface

uses
  Windows, MMSystem, Classes, ExtCtrls, SysUtils, Forms, Variants;

const
  SndEvent_DefAutoRegister = false;
  restorer_ValueName = 'SizeAndPosition';

type
  ERegistryError = class(Exception);

  TRegistryEntry = class(TComponent)
  protected
    function GetExist: Boolean; virtual; abstract;
    procedure DoDelete; virtual; abstract;
  public
    procedure Delete;
    property Exist: Boolean read GetExist;
  end;

  TCustomKey = class(TRegistryEntry)
  private
    FParentKey: TCustomKey;
    FAutoCloseTimer: TTimer;
    FHandle: hKey;
    procedure SetActive(V: Boolean);
    function GetActive: Boolean;
    function GetHandle: THandle;
    procedure SetAutoCloseTimer;
    procedure KillAutoCloseTimer;
    procedure AutoClose(Sender: TObject);
    function GetFullPath: string;
  protected
    procedure SetParentKey(V: TCustomKey); virtual;
    function GetExist: Boolean; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DoDelete; override;
    function GetPath: string; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    property ParentKey: TCustomKey read FParentKey write SetParentKey;
  public
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
    property Active: Boolean read GetActive write SetActive stored false;
    property Handle: THandle read GetHandle;
    property Path: string read GetPath;
    property FullPath: string read GetFullPath;
  end;

  TRegKey = class(TCustomKey)
  published
    property ParentKey;
  end;

  TAppConfigKey = class(TCustomKey)
  private
    FCompanyName: string;
    FApplicationName: string;
    FApplicationVersion: string;
    procedure SetApplicationName(V: string);
    procedure SetCompanyName(V: string);
    procedure SetApplicationVersion(V: string);
  protected
    function GetPath: string; override;
  public
    constructor Create(anOwner: TComponent); override;
  published
    property ApplicationName: string
      read FApplicationName write SetApplicationName;
    property ApplicationVersion: string
      read FApplicationVersion write SetApplicationVersion;
    property CompanyName: string
      read FCompanyName write SetCompanyName;
  end;

  TCustomProfile = class(TRegistryEntry)
  protected
    FKey: TCustomKey;
    function GetExist: boolean; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DoDelete; override;
  public
    constructor Create(anOwner: TComponent); override;
    property Key: TCustomKey
      read FKey write FKey;
  end;

  TVariantProfile = class(TCustomProfile)
  protected
    function GetValue: Variant; virtual;
    procedure SetValue(V: Variant); virtual;
  public
    property Value: Variant read GetValue write SetValue;
  published
    property Key;
  end;

  TSoundEventOption = (seResetSchemeOnUpdate, sePlaySync,
    seNoDefault, seNoStop);

  TSoundEventOptions = set of TSoundEventOption;

  TSoundEvent = class(TCustomKey)
  private
    FEventLabel: string;
    FAutoRegister: Boolean;
    FDefaultSoundName: string;
    FAppConfigKey: TAppConfigKey;
    FOptions: TSoundEventOptions;
    FOnSoundEvent: TNotifyEvent;
    function GetSOundName: string;
    procedure SetSoundName(const NewName: string);
    function GetDefaultSoundName: string;
    procedure SetDefaultSoundName(const NewName: string);
    function ReadSoundName: string;
    procedure WriteSoundName(const NewName: string);
    function ReadDefaultSoundName: string;
    procedure WriteDefaultSoundName(const NewName: string);
    procedure SetEventLabel(const NewLabel: string);
  protected
    function GetPath: string; override;
    procedure Loaded; override;
    procedure SoundEvent; dynamic;
    procedure DoDelete; override;
  public
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterSoundEvent;
    procedure UnregisterSoundEvent;
    procedure Play;
    property SoundName: string
      read GetSoundName write SetSoundName stored false;
  published
    property AppConfigKey: TAppCOnfigKey
      read FAppConfigKey write FAppConfigKey;
    property AutoRegister: boolean
      read FAutoRegister write FAutoRegister default SndEvent_DefAutoRegister;
    property Options: TSoundEventOptions
      read FOptions write FOptions;
    property DefaultSoundName: string
      read GetDefaultSoundName write SetDefaultSoundName;
    property EventLabel: string
      read FEventLabel write SetEventLabel;
    property OnSoundEvent: TNotifyEvent
      read FOnSoundEvent write FOnSoundEvent;
  end;

  TStringProfile = class(TCustomProfile)
  private
    procedure SetValue(NewValue: string);
    function GetValue: string;
  public
    property Value: string
      read GetValue write SetValue;
  published
    property Key;
  end;

  TComponentProfile = class(TCustomProfile)
  private
    procedure SetValue(NewValue: TComponent);
    function GetValue: TComponent;
  public
    property Value: TComponent
      read GetValue write SetValue;
  published
    property Key: TCustomKey
      read FKey write FKey;
  end;

  TFormRestorerOption = (frSizeAndPosition, frSizeOnly, frPositionOnly);

  TFormRestorer = class(TCustomKey)
  private
    FRestoring: TFormRestorerOption;
    FOldOnClose: TCloseEvent;
    procedure OurOnClose(Sender: TObject; var Action: TCloseAction);
  protected
    function GetPath: string; override;
    procedure Loaded; override;
  public
    constructor Create(anOwner: TComponent); override;
    procedure Store;
    procedure Restore;
  published
    property Restoring: TFormRestorerOption read FRestoring write FRestoring
      default frSizeAndPosition;
    property ParentKey;
  end;

const
  SInvalidInteger = 65408;
  SInvalidFloat = 65409;
  SInvalidDate = 65410;
  SInvalidTime = 65411;
  SInvalidDateTime = 65412;
  STimeEncodeError = 65413;
  SDateEncodeError = 65414;
  SOutOfMemory = 65415;
  SInOutError = 65416;
  SFileNotFound = 65417;
  SInvalidFilename = 65418;
  STooManyOpenFiles = 65419;
  SAccessDenied = 65420;
  SEndOfFile = 65421;
  SDiskFull = 65422;
  SInvalidInput = 65423;
  SDivByZero = 65424;
  SRangeError = 65425;
  SIntOverflow = 65426;
  SInvalidOp = 65427;
  SZeroDivide = 65428;
  SOverflow = 65429;
  SUnderflow = 65430;
  SInvalidPointer = 65431;
  SInvalidCast = 65432;
  SAccessViolation = 65433;
  SStackOverflow = 65434;
  SControlC = 65435;
  SPrivilege = 65436;
  SOperationAborted = 65437;
  SException = 65438;
  SExceptTitle = 65439;
  SInvalidFormat = 65440;
  SArgumentMissing = 65441;
  SInvalidVarCast = 65442;
  SInvalidVarOp = 65443;
  SDispatchError = 65444;
  SReadAccess = 65445;
  SWriteAccess = 65446;
  SResultTooLong = 65447;
  SFormatTooLong = 65448;
  SVarArrayCreate = 65449;
  SVarNotArray = 65450;
  SVarArrayBounds = 65451;
  SExternalException = 65452;

  SShortMonthNames = 65472;
  SLongMonthNames = 65488;
  SShortDayNames = 65504;
  SLongDayNames = 65511;

const
  SFirst = 54567;

  SCrossRef = SFirst + 0;
  SOpenKeyError = SFirst + 1;
  SCloseKeyError = SFirst + 2;
  SLostKeyPath = SFirst + 3;
  SInvalidValue = SFirst + 4;
  SDefApplicationName = SFirst + 5;
  SDefApplicationVersion = SFirst + 6;
  SDefCompanyName = SFirst + 7;
  SNoKey = SFirst + 8;
  SRegReadError = SFirst + 9;
  SInvalidType = SFirst + 10;
  SRegWriteError = SFirst + 11;
  SDeleteKeyError = SFirst + 12;
  SDeleteValueError = SFirst + 13;
  SSndEventParentKey = SFirst + 14;
  SLostSoundEventReg = SFirst + 15;
  SSoundEventLabel = SFirst + 16;
  SDefSoundEventLabel = SFirst + 17;

procedure Register;

implementation

{$R ANOTHERREG.RES}

const
  AutoCloseInterval = 5000;

function FindOwnerForm(C: TComponent): TForm;
begin
  Result := nil;
  if c = nil then
    Exit;
  Result := TForm(c.Owner);
  while Result <> nil do
    if Result is TForm then
      break
    else
      Result := TForm(Result.Owner);
end;

function StrToIdent(const S: string): string;
var
  i: integer;
begin
  if IsValidIdent(S) then
    Result := S
  else
  begin
    if (S[1] in ['0'..'9']) then
      Result := '_' + Trim(S)
    else
      Result := Trim(S);
    for i := 2 to Length(Result) - 1 do
      if Result[i] = ' ' then
        Result[i] := '_';
  end;
end;

function FindCompByClass(Root: TComponent; CompClass: TClass): TComponent;
var
  i: integer;
begin
  Result := nil;
  if (root = nil) then
    Exit;
  for i := 0 to root.componentCount - 1 do
  begin
    if root.components[i] is CompClass then
      Result := root.components[i]
    else
      Result := FindCompByClass(root.components[i], CompClass);
    if Result <> nil then
      break;
  end;
end;

function CheckCrossRef(Root, Item: TCustomKey): boolean;
var
  K: TCustomKey;
begin
  Result := false;
  if (Item = nil) or (root = nil) then
    Exit;
  K := Item;
  while K <> nil do
  begin
    if K = Root then
    begin
      Result := true;
      Exit;
    end
    else
      K := K.FParentKey;
  end;
end;

{ TCustomLey }

procedure TCustomKey.DoDelete;
var
  H: HKey;
begin
  close;
  if FParentKey <> nil then
    H := FParentKey.Handle
  else
    H := HKEY_CURRENT_USER;
  if RegDeleteKey(H, PChar(Path)) <> ERROR_SUCCESS then
    raise ERegistryError.CreateResFmt(SDeleteKeyError, [FullPath]);
end;

function TCustomKey.GetExist: Boolean;
var
  H: hKey;
begin
  Result := RegOpenKey(HKEY_CURRENT_USER, PChar(FullPath), H) = ERROR_SUCCESS;
  if Result then
    RegCloseKey(H);
end;

procedure TCustomKey.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FParentKey) then
    FParentKey := nil;
end;

procedure TCustomKey.AutoClose(Sender: TObject);
begin
  Close;
end;

procedure TCustomKey.SetParentKey(V: TCustomKey);
begin
  if V = FParentKey then
    Exit;
  if (csLoading in ComponentSTate) then
  begin
    FParentKey := V;
    Exit;
  end;
  if CheckCrossRef(Self, V) then
    raise ERegistryError.CreateRes(SCrossRef);
  close;
  FParentKey := V;
end;

procedure TCustomKey.SetActive(V: Boolean);
begin
  if v then
    Open
  else
    Close;
end;

function TCustomKey.GetActive: Boolean;
begin
  Result := FHandle <> 0;
end;

procedure TCustomKey.Open;
var
  H: hKey;
begin
  if (FHandle = 0) then
  begin
    if (FParentKey <> nil) then
      H := FParentKey.Handle
    else
      H := HKEY_CURRENT_USER;
    if RegCreateKey(H, PChar(Path), FHandle) <> ERROR_SUCCESS then
      raise ERegistryError.CreateResFmt(SOpenKeyError, [Path]);
  end;
  if (FParentKey = nil) then
    SetAutoCloseTimer;
end;

procedure TCustomKey.Close;
begin
  if FHandle = 0 then
    Exit;
  if RegCloseKey(FHandle) <> ERROR_SUCCESS then
    raise ERegistryError.CreateResFmt(SCloseKeyError, [Path]);
  FHandle := 0;
  if (FParentKey = nil) then
    KillAutoCloseTimer;
end;

constructor TCustomKey.Create(anOwner: TComponent);
begin
  inherited Create(anOwner);
  FAutoCloseTimer := nil;
  FHandle := 0;
  FParentKey := nil;
end;

destructor TCustomKey.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TCustomKey.GetHandle;
begin
  open;
  Result := FHandle;
end;

procedure TCustomKey.SetAutoCloseTimer;
begin
  if FAutoCloseTimer = nil then
    FAutoCloseTimer := TTimer.Create(Self);
  with FAutoCloseTimer do
  begin
    OnTimer := AutoClose;
    Interval := AutoCloseInterval;
  end;
end;

procedure TCustomKey.KillAutoCloseTimer;
begin
  if FAutoCloseTimer <> nil then
  begin
    FAutoCloseTimer.Enabled := false;
    FAutoCloseTimer.Free;
    FAutoCloseTimer := nil;
  end;
end;

function TCustomKey.GetPath: string;
begin
  if (FParentKey = nil) then
    Result := LoadStr(SLostKeyPath) + '\' + Name
  else
    Result := Name;
end;

function TCustomKey.GetFullPath: string;
begin
  if (FParentKey <> nil) then
    Result := FParentKey.FullPath + '\' + Path
  else
    Result := path;
end;

{ TAppConfigKey }

procedure TAppConfigKey.SetApplicationName(V: string);
begin
  if V = '' then
    raise ERegistryError.CreateRes(SInvalidValue);
  close;
  FApplicationName := V;
end;

procedure TAppConfigKey.SetApplicationVersion(V: string);
begin
  if V = '' then
    raise ERegistryError.CreateRes(SInvalidValue);
  close;
  FApplicationVersion := V;
end;

constructor TAppConfigKey.Create(anOwner: TComponent);
begin
  inherited Create(anOwner);
  FCompanyName := LoadStr(SDefCompanyName);
  FApplicationName := LoadStr(SDefApplicationName);
  FApplicationVersion := LoadStr(SDefApplicationVersion);
end;

function TAppConfigKey.GetPath: string;
begin
  Result := 'Software\' + CompanyName + '\' + ApplicationName + '\' +
    ApplicationVersion;
end;

procedure TAppConfigKey.SetCompanyName(V: string);
begin
  if V = '' then
    raise ERegistryError.CreateRes(SInvalidValue);
  close;
  FCompanyName := V;
end;

{ TCustomProfile }

procedure TCustomProfile.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FKey) and (Operation = opRemove) then
    FKey := nil;
  inherited Notification(AComponent, Operation);
end;

constructor TCustomProfile.Create(anOwner: TComponent);
begin
  inherited Create(anOwner);
  FKey := nil;
end;

function TCustomProfile.GetExist: boolean;
var
  T, S: Longint;
begin
  if Key = nil then
    raise ERegistryError.CreateRes(SNoKey);
  T := REG_BINARY;
  Result := RegQueryValueEx(Key.Handle, PChar(Name),
    nil, @T, nil, @S) = ERROR_SUCCESS;
end;

procedure TCustomProfile.DoDelete;
begin
  if Key = nil then
    raise ERegistryError.CreateRes(SNoKey);
  if RegDeleteValue(Key.Handle, PChar(Name)) <> ERROR_SUCCESS then
    raise ERegistryError.CreateResFmt(SDeleteValueError, [Name, Key.Path]);
end;

{ TVariantProfile }

{ The code looks bizzare, but I had no time and information
 to do something more professional.
 By the way, can anybody make me a present
 called "recent version of Turbo Debugger?" }

function TVariantProfile.GetValue: Variant;
var
  S, R, T: Longint;
  B: Pointer;
begin
  if Key = nil then
    raise ERegistryError.CreateRes(SNoKey);
  T := REG_BINARY;
  R := RegQueryValueEx(Key.Handle, PChar(Name), nil, @T, nil, @S);
  if (R <> ERROR_SUCCESS) then
  begin
    VarClear(Result);
    Exit;
  end;
  GetMem(B, S + 32);
  if (B = nil) then
    raise EOutOfMemory.CreateRes(SOutOfMemory);
  try
    R := RegQueryValueEx(Key.Handle, PChar(Name), nil, @T, B, @S);
    if (R <> ERROR_SUCCESS) then
      raise ERegistryError.CreateRes(SRegReadError);
    if (PVarData(B)^.VType = varString) then
      Result := string(PChar(Longint(B) + 2))
    else
      move(b^, Result, sizeof(variant));
  finally
    FreeMem(B);
  end;
end;

procedure TVariantProfile.SetValue(V: Variant);
var
  B: Pointer;
  R, S: Longint;
  vvtyp: word;
begin
  if Key = nil then
    raise ERegistryError.CreateRes(SNoKey);
  vvtyp := VarType(V);
  case vvtyp of
    varString: S := 2 + Length(V) + 1;
    varByte, varWord, varShortInt, varSmallint, varInteger, varLongWord,
      varInt64,
      varSingle, varDouble, varCurrency,
      varBoolean, varDate: S := SizeOf(Variant);
  else
    raise ERegistryError.CreateRes(SInvalidType);
  end;
  GetMem(B, S + 32);
  if (B = nil) then
    raise EOutOfMemory.CreateRes(SOutOfMemory);
  try
    move(V, B^, SizeOf(Variant));
    if ((VarType(V) = varString) and (Length(V) > 0)) then
      move(TVarData(V).VString^, Pointer(Longint(B) + 2)^, S - 2);
    R := RegSetValueEx(Key.Handle, PChar(Name), 0, REG_BINARY, B, S);
    if (R <> ERROR_SUCCESS) then
      raise ERegistryError.CreateRes(SRegWriteError);
  finally
    FreeMem(B);
  end;
end;

{ TRegistryEntry }

procedure TRegistryEntry.Delete;
begin
  if exist then
    DoDelete;
end;

{ Predefined keys used by TSoundEvent }

type
  TSchemesKey = class(TCustomKey)
  protected
    function GetPath: string; override;
  end;

  TEventLabelsKey = class(TCustomKey)
  protected
    function GetPath: string; override;
  end;

  TSchemesAppsKey = class(TCustomKey)
  protected
    function GetPath: string; override;
  end;

  TAppEventsKey = class(TCustomKey)
  protected
    function GetPath: string; override;
  end;

function TAppEventsKey.GetPath: string;
begin
  Result := 'AppEvents';
end;

function TSchemesKey.GetPath: string;
begin
  Result := 'AppEvents\Schemes';
end;

function TSchemesAppsKey.GetPath: string;
begin
  Result := 'AppEvents\Schemes\Apps';
end;

function TEventLabelsKey.GetPath: string;
begin
  Result := 'AppEvents\EventLabels';
end;

{ TSoundEvent }

constructor TSoundEvent.Create(anOwner: TComponent);
var
  i: integer;
begin
  inherited Create(anOwner);
  FAutoRegister := SndEvent_DefAutoRegister;
  FDefaultSoundName := '';
  FOptions := [seResetSchemeOnUpdate];
  FAppConfigKey := nil;
  FEventLabel := LoadStr(SDefSoundEventLabel);
  if (csDesigning in ComponentState) then
    for i := 0 to Screen.FormCount - 1 do
    begin
      FAppConfigKey := TAppConfigKey(FindCompByClass(
        Screen.Forms[i], TAppConfigKey));
      if (FAppConfigKey <> nil) then
        break;
    end;
  FParentKey := TSchemesAppsKey.Create(Self);
end;

function TSoundEvent.GetSoundName: string;
begin
  Result := ReadSoundName;
end;

procedure TSoundEvent.SetSoundName(const NewName: string);
begin
  WriteSoundName(NewName);
end;

{ DefaultSoundName property read accessor. At design-time
returns value of FDefaultSoundName. At run-time returns
default value of ".Default" subkey }

function TSoundEvent.GetDefaultSoundName: string;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Result := FDefaultSoundName
  else
    Result := ReadDefaultSoundName;
end;

{ DefaultSoundName property write accessor. At design-time stores
 new name in FDefaultSoundName field. At run-time updates
 default value of ".Default" subkey.)}

procedure TSoundEvent.SetDefaultSoundName(const NewName: string);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    FDefaultSoundName := NewName
  else
    WriteDefaultSoundName(NewName);
end;

{ Changes parent key. Accepts only TAppConfigKey }

function TSoundEvent.GetPath: string;
begin
  if (FParentKey = nil) then
    Result := 'LostEvents'
  else
    Result := StrToIdent(FAppCOnfigKey.ApplicationName);
  Result := Result + '\' + Name;
end;

{ At run-time registers event if AutoRegister is true }

procedure TSoundEvent.Loaded;
begin
  inherited Loaded;
  if (not (csDesigning in ComponentState)) and AutoRegister then
    RegisterSoundEvent;
end;

function RegCreateSubKey(Key: hKey; const SubKeyName: string): HKey;
var
  R, D: Longint;
  H: hKey;
begin
  R := RegCreateKeyEx(Key, PChar(SubKeyName), 0, nil, 0, KEY_ALL_ACCESS, nil, H,
    @D);
  if (R <> ERROR_SUCCESS) then
    raise ERegistryError.CreateResFmt(SOpenKeyError, [SubKeyName]);
  Result := H;
end;

function RegReadSubKeyString(Key: hKey; const SubKeyName: string; const
  ValueName: string): string;
var
  S, R, T: Longint;
  B: Pointer;
  H: THandle;
begin
  H := RegCreateSubKey(Key, SubKeyName);
  try
    T := REG_SZ;
    R := RegQueryValueEx(H, PChar(ValueName), nil, @T, nil, @S);
    if (R <> ERROR_SUCCESS) or (S = 0) then
      Result := ''
    else
    begin
      GetMem(B, S + 32);
      if (B = nil) then
        raise EoutOfMemory.CReateRes(SOutOfMemory);
      try
        R := RegQueryValueEx(H, PChar(ValueName), nil, @T, B, @S);
        if (R <> ERROR_SUCCESS) then
          raise ERegistryError.CreateRes(SRegReadError);
        Result := PChar(B);
      finally
        FreeMem(B);
      end;
    end;
  finally
    RegCloseKey(H);
  end;
end;

procedure RegWriteSubKeyString(
  Key: hKey;
  const SubKeyName: string;
  const ValueName: string;
  const Value: string);
var
  R: Longint;
  H: THandle;
begin
  H := RegCreateSubKey(Key, SubKeyName);
  try
    R := RegSetValueEx(H, PChar(ValueName), 0, REG_SZ,
      PChar(Value), Length(Value) + 1);
    if (R <> ERROR_SUCCESS) then
      raise ERegistryError.CreateRes(SRegWriteError);
  finally
    RegCloseKey(H);
  end;
end;

const

  path_Default = '.Default';
  path_Current = '.current';
  path_Schemes = 'Schemes';

function TSoundEvent.ReadDefaultSoundName: string;
begin
  Result := RegReadSubKeyString(Handle, path_Default, '');
end;

procedure TSoundEvent.WriteDefaultSoundName(const NewName: string);
begin
  RegWriteSubKeyString(Handle, path_Default, '', NewName);
end;

function TSoundEvent.ReadSoundName: string;
begin
  Result := RegReadSubKeyString(Handle, path_Current, '');
end;

procedure TSoundEvent.WriteSoundName(const NewName: string);
var
  SchemeName: string;
begin
  with TAppEventsKey.Create(Self) do
    try
      if (seResetSchemeOnUpdate in Options) then
        RegWriteSubKeyString(Handle, path_Schemes, '', path_Current)
      else
      begin
        SchemeName := RegReadSubKeyString(Handle, path_Schemes, '');
        RegWriteSubKeyString(Self.Handle, SchemeName, '', NewName);
      end;
    finally
      free;
    end;
  RegWriteSubKeyString(Handle, path_Current, '', NewName);
end;

procedure TSoundEvent.RegisterSoundEvent;
begin
  if AppConfigKey = nil then
    raise ERegistryError.CreateRes(SLostSoundEventReg);
  { Setting \AppEvents\Schemes\Apps\<AppID> key }
  with TSchemesAppsKey.Create(Self) do
    try
      RegWriteSubKeyString(Handle, StrToIdent(AppConfigKey.ApplicationName),
        '', AppConfigKey.ApplicationName);
    finally
      Free;
    end;
  { Setting \AppEvents\EventLabels\<Name> key }
  with TEventLabelsKey.Create(Self) do
    try
      RegWriteSubKeyString(Handle, Self.Name, '', EventLabel);
    finally
      free;
    end;
  { Setting up \AppEvents\Schemes\Apps\<AppID>\<Name>\.Default key }
  if FDefaultSoundName <> '' then
    RegWriteSubKeyString(Handle, path_Default, '', FDefaultSoundName);
  RegCreateSubKey(Handle, path_Current);
end;

procedure TSoundEvent.UnregisterSoundEvent;
begin
  Delete;
end;

procedure TSoundEvent.SetEventLabel(const NewLabel: string);
begin
  if EventLabel = NewLabel then
    Exit;
  if (NewLabel = '') then
    raise ERegistryError.CreateRes(SSoundEventLabel);
  FEventLabel := NewLabel;
end;

procedure TSoundEvent.SoundEvent;
begin
  if Assigned(FOnSoundEvent) then
    FOnSOundEvent(Self);
end;

procedure TSoundEvent.Play;
var
  F: Longint;
  S: string;
begin
  SoundEvent;
  F := 0;
  if (sePlaySync in Options) then
    F := F or SND_SYNC
  else
    F := F or SND_ASYNC;
  if (seNoDefault in Options) then
    F := F or SND_NODEFAULT;
  if (seNoStop in Options) then
    F := F or SND_NOSTOP;
  S := SoundName;
  if (S = '') and (not (seNoDefault in Options)) then
    S := DefaultSoundName;
  PlaySound(PChar(S), 0, SND_FILENAME or F);
end;

destructor TSoundEvent.Destroy;
begin
  PlaySound(nil, 0, SND_PURGE);
  inherited Destroy;
end;

procedure TSoundEvent.DoDelete;
begin
  inherited DoDelete;
  with TEventLabelsKey.Create(Self) do
    try
      RegDeleteKey(Handle, PChar(Self.Name));
    finally
      free;
    end;
end;

{ TStringProfile }

procedure TStringProfile.SetValue(NewValue: string);
var
  R: Longint;
begin
  if Key = nil then
    raise ERegistryError.CreateRes(SNoKey);
  R := RegSetValueEx(Key.Handle, PChar(Name), 0, REG_SZ,
    PChar(NewValue), Length(NewValue) + 1);
  if (R <> ERROR_SUCCESS) then
    raise ERegistryError.CreateRes(SRegWriteError);
end;

function TStringProfile.GetValue: string;
var
  S, R, T: Longint;
begin
  if Key = nil then
    raise ERegistryError.CreateRes(SNoKey);
  T := REG_SZ;
  R := RegQueryValueEx(Key.Handle, PChar(Name), nil, @T, nil, @S);
  if (R <> ERROR_SUCCESS) then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Result, S);
  try
    R := RegQueryValueEx(Key.Handle, PChar(Name), nil, @T, pointer(Result), @S);
    if (R <> ERROR_SUCCESS) then
      raise ERegistryError.CreateRes(SRegReadError);
  except
    Result := '';
    raise;
  end;
end;

{ TComponentProfile }

procedure TComponentProfile.SetValue(NewValue: TComponent);
var
  R: Longint;
  Stream: TMemoryStream;
begin
  if Key = nil then
    raise ERegistryError.CreateRes(SNoKey);
  if NewValue = nil then
  begin
    R := RegSetValueEx(Key.Handle, PChar(Name), 0, REG_BINARY, nil, 0);
    if (R <> ERROR_SUCCESS) then
      raise ERegistryError.CreateRes(SRegWriteError);
    Exit;
  end;
  Stream := TMemoryStream.Create;
  try
    Stream.WriteComponent(NewValue);
    R := RegSetValueEx(Key.Handle, PChar(Name), 0, REG_BINARY, Stream.Memory,
      Stream.Size);
    if (R <> ERROR_SUCCESS) then
      raise ERegistryError.CreateRes(SRegWriteError);
  finally
    Stream.Free;
  end;
end;

function TComponentProfile.GetValue: TComponent;
var
  S, R, T: Longint;
  Stream: TMemoryStream;
begin
  Result := nil;
  if Key = nil then
    raise ERegistryError.CreateRes(SNoKey);
  T := REG_BINARY;
  R := RegQueryValueEx(Key.Handle, PChar(Name), nil, @T, nil, @S);
  if (R <> ERROR_SUCCESS) or (S = 0) then
    Exit;
  Stream := TMemoryStream.Create;
  try
    Stream.SetSize(S + 32);
    R := RegQueryValueEx(Key.Handle, PChar(Name), nil, @T, Stream.Memory, @S);
    if (R <> ERROR_SUCCESS) then
      raise ERegistryError.CreateRes(SRegReadError);
    Result := Stream.ReadComponent(nil);
  finally
    Stream.Free;
  end;
end;

{ TFormRestorer }

function TFormRestorer.GetPath: string;
begin
  if ParentKey = nil then
    Result := inherited GetPath
  else
    Result := FindOwnerForm(Self).Name;
end;

constructor TFormRestorer.Create(anOwner: TComponent);
var
  i: integer;
begin
  inherited Create(anOwner);
  FRestoring := frSizeAndPosition;
  FOldOnClose := nil;
  if csDesigning in ComponentState then
    for i := 0 to Screen.FormCount - 1 do
    begin
      FParentKey := TCustomKey(FindCompByClass(Screen.Forms[i], TAppConfigKey));
      if (FParentKey <> nil) then
        Break;
    end;
end;

procedure TFormRestorer.Loaded;
begin
  inherited Loaded;
  if (not (csDesigning in componentState)) then
  begin
    with FindOwnerForm(self) do
    begin
      FOldOnClose := OnClose;
      OnClose := OurOnClose;
    end;
    if exist then
      restore;
  end;
end;

type
  TRestoringParams = record
    R: TRect;
    S: TWindowState;
  end;

procedure TFormRestorer.Store;
var
  F: TForm;
  R: Longint;
  P: TRestoringParams;
begin
  F := FindOwnerForm(self);
  P.R := Rect(F.Left, F.Top, F.Width, F.Height);
  P.S := F.WindowState;
  R := RegSetValueEx(Handle, Restorer_ValueName, 0, REG_BINARY, @P, SizeOf(P));
  if (R <> ERROR_SUCCESS) then
    raise ERegistryError.CreateRes(SRegWriteError);
end;

procedure TFormRestorer.Restore;
var
  S, R, T: Longint;
  P: TRestoringParams;
begin
  T := REG_BINARY;
  S := SizeOf(P);
  R := RegQueryValueEx(Handle, Restorer_ValueName, nil, @T, @P, @S);
  if (R <> ERROR_SUCCESS) or (S <> SizeOf(P)) then
    Exit;

  with FindOwnerForm(self) do
  begin
    if Restoring in [frSizeAndPosition, frPositionOnly] then
    begin
      Left := P.R.Left;
      Top := P.R.Top;
    end;
    if Restoring in [frSizeAndPosition, frSizeOnly] then
    begin
      Width := P.R.Right;
      Height := P.R.Bottom;
    end;
    if Restoring = frSizeAndPosition then
      WindowState := P.S;
    if WindowState = wsMinimized then
      WindowState := wsNormal;
  end;
end;

procedure TFormRestorer.OurOnClose(Sender: TObject; var Action: TCloseAction);
begin
  Store;
  if assigned(FOldOnClose) then
    FOldOnClose(Sender, Action);
end;

procedure Register;
begin
  RegisterComponents('TombViewer Components', [
    TAppConfigKey, TRegKey, TVariantProfile, TStringProfile, TComponentProfile,
    TFormRestorer, TSoundEvent
  ]);
end;

end.

