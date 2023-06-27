unit DaJson.Handler;

interface

uses
  DWL.Server.Handler.DLL.Classes, System.Generics.Collections, System.Classes,
  DWL.Server.Types, DWL.MySQL, System.JSON;

type
  TResourceDef = class
  strict private
    FTableName: string;
    FSQLWhere: string;
    FAttributes: TStringList;
  private
    property Attributes: TStringList read FAttributes;
    property SQLWhere: string read FSQLWhere;
    property Tablename: string read FTableName;
  public
    constructor Create(const TableName, SQLWhere: string; var Attributes: TStringList);
    destructor Destroy; override;
  end;

  THandler_DaJson = class(TdwlDLLHandling_OpenID)
  strict private
    class procedure Config_LoadResources;
    class procedure Config_LoadResource(Session: IdwlMySQLSession; Resource: TJSONObject);
    class function GetFieldsFromTable(Session: IdwlMySQLSession; const TableName: string; Fields: TStringList): boolean;
  class var
    FTables: TObjectDictionary<string, TResourceDef>;
    class function Get_Resource(const State: PdwlHTTPHandlingState): boolean;
  public
    class procedure Configure(const Params: string); override;
    class procedure WrapUp(const State: PdwlHTTPHandlingState); override;
  end;

implementation

uses
  System.SysUtils, DWL.Logging, DWL.HTTP.Consts, DWL.Server.Utils,
  DWL.Params.Consts, System.Math, System.NetEncoding, DWL.StrUtils;

const
  Param_Resources = 'resources';

{ THandler_DaJson }

class procedure THandler_DaJson.Configure(const Params: string);
begin
  inherited Configure(Params);
  FTables := TObjectDictionary<string, TResourceDef>.Create([doOwnsValues]);
  Config_LoadResources;
end;

class procedure THandler_DaJson.Config_LoadResource(Session: IdwlMySQLSession;
  Resource: TJSONObject);
begin
  var ResourceName := Resource.GetValue<string>('resource_name');
  var TableName := Resource.GetValue<string>('table_name', ResourceName);
  var SQlWhere := Resource.GetValue<string>('sql_where', '');
  var ExposedFields := TStringList.Create;
  var SQLFields := TStringList.Create;
  try
    // Now we have the tablename we can get the fields
    GetFieldsFromTable(Session, TableName, SQLFields);
    var JsonAttributes := Resource.GetValue<TJSONArray>('attributes');
    if JsonAttributes<>nil then
    begin
      for var JsonAttribute in JsonAttributes do
      if JsonAttribute is TJSONString then
        ExposedFields.Add(TJSONString(JsonAttribute).Value)
      else
        raise Exception.Create('Only strings allowed in attributes array');
      // primary field not to be present in exposed fields array
      var PrimId := ExposedFields.IndexOf(SQLFields[0]);
      if PrimId>=0 then
        ExposedFields.Delete(PrimId);
      // remove not exposed SQL fields not found in table
      for var SQLIdx := SQLFields.Count-1 downto 1 do
      begin
        var ExpIdx :=  ExposedFields.IndexOf(SQLFields[SQLIdx]);
        if ExpIdx>=0 then
          ExposedFields.Delete(ExpIdx)
        else
          SQLFields.Delete(SQLIdx);
      end;
      if ExposedFields.Count>0 then
        TdwlLogger.Log('Field(s) '+ExposedFields.DelimitedText+' from '+ResourceName+' not found in SQL Table', lsWarning);
    end; // else keep all fields
    FTables.Add(ResourceName, TResourceDef.Create(TableName, SQlWhere, SQLFields));
    // add handlings for every resource served
    RegisterHandling(dwlhttpGET, '/'+ResourceName, Get_Resource, []);
  finally
    SQLFields.Free;  // doesnt harm if nil
    ExposedFields.Free;
  end;
end;

class procedure THandler_DaJson.Config_LoadResources;
begin
  try
    var Session := New_MySQLSession(FConfigParams);
    var JsonResources := TJSONValue.ParseJSONValue(FConfigParams.StrValue(Param_Resources));
    try
      if (JsonResources is TJSONObject)  then
        Config_LoadResource(Session, TJSONObject(JsonResources))
      else
      begin
        if JsonResources is TJSONArray then
        begin
          for var JsonResource in TJSONArray(JsonResources) do
          begin
            if not (JsonResource is TJSONObject) then
              raise Exception.Create('Definition is not an array of objects');
            Config_LoadResource(Session, TJSONObject(JsonResource));
          end;
        end
        else
          raise Exception.Create('Definition is empty, invalid or not an (array of) object');
      end;
    finally
      JsonResources.Free;
    end;
  except
    on E: Exception do
      TdwlLogger.Log('Error loading Resources: '+E.Message, lsError);
  end;
end;

class function THandler_DaJson.GetFieldsFromTable(Session: IdwlMySQLSession; const TableName: string; Fields: TStringList): boolean;
begin
  Result := false;
  try
    Fields.Clear;
    var DbName := FConfigParams.StrValue(Param_Db);
    // first get the primary index field
    var Cmd := Session.CreateCommand('SELECT COLUMN_NAME from information_schema.STATISTICS WHERE TABLE_SCHEMA="' +
      DbName+'" AND TABLE_NAME="'+TableName+'" AND INDEX_NAME="PRIMARY"');
    Cmd.Execute;
    if not Cmd.Reader.Read then
    begin
      TdwlLogger.Log('Unable to retrieve primary index from table '+TableName+', skipped table', lsWarning);
      Exit;
    end;
    var PrimFieldName := Cmd.Reader.GetString(0);
    if Cmd.Reader.Read then
    begin
      TdwlLogger.Log('Primary index from table '+TableName+' has multiple fields, skipped table', lsWarning);
      Exit;
    end;
    // Add primary Field to result
    Fields.AddObject(PrimFieldName, pointer(1));
    // then retrieve all fields
    Cmd := Session.CreateCommand('SELECT COLUMN_NAME, DATA_TYPE from information_schema.COLUMNS WHERE TABLE_SCHEMA="'+ DbName+'" AND TABLE_NAME="'+TableName+'"');
    Cmd.Execute;
    while Cmd.Reader.Read do
    begin
      var FieldName := Cmd.Reader.GetString(0);
      var IsInteger := Min(1, Pos('int', Cmd.Reader.GetString(1).ToLower));
      if PrimFieldName=FieldName then
      begin
        if IsInteger=0 then
        begin
          TdwlLogger.Log('Primary field from table '+TableName+' is not an integer, skipped table', lsWarning);
          Exit;
        end;
        Continue;
      end;
      Fields.AddObject(FieldName.ToLower, pointer(IsInteger));
    end;
    Result := true;
  except
    on E: Exception do
      TdwlLogger.Log('Unable to get fields from '+TableName+': '+E.Message, lsError);
    // we dit not succeed
  end;
end;

class function THandler_DaJson.Get_Resource(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := false;
  var ResourceDef: TResourceDef;
  var ResourceName := Copy(State.URI, 2, MaxInt);
  if not FTables.TryGetValue(ResourceName, ResourceDef) then
    Exit;
  // get totalcount
  var SQL := 'SELECT `'+ResourceDef.Attributes[0]+'`';
  for var Idx := 1 to ResourceDef.Attributes.Count-1 do
    SQL := SQL+',`'+ResourceDef.Attributes[Idx]+'`';
  SQL := SQL + ' FROM `'+ResourceDef.Tablename+'`';
  var SQLWhere := ResourceDef.SQLWhere;
  if SQLWhere<>'' then
    SQLWhere := ' ('+SQLWhere+')';


  var Filter: string;
  var AllRequestParams: string;
  if State.TryGetRequestParamStr('*', AllRequestParams) then
  begin
    var RequestParams := TStringList.Create;
    try
      RequestParams.Text := AllRequestParams;
      for var i := 0 to RequestParams.Count-1 do
      begin
        var Key := RequestParams.Names[i];
        if Key.StartsWith('filter[') and Key.EndsWith(']') then
        begin
          var Value := TNetEncoding.URL.Decode(RequestParams.Values[Key]);
          if SQLWhere<>'' then
            SQLWhere := SQLWhere+' AND';
          Key := Copy(Key, 8, Key.Length-8);
          if Key='*' then
          begin
            SQLWhere := SQLWhere+' (';
            for var AttNo := 1 to ResourceDef.Attributes.Count-1 do
            begin
              if AttNo>1 then
                SQLWhere := SQLWhere+' OR';
              SQLWhere := SQLWhere+' (`'+ResourceDef.Attributes[AttNo]+'` LIKE "%'+Value+'%")';
            end;
            SQLWhere := SQLWhere+')';
          end
          else
          begin
            if ResourceDef.Attributes.IndexOf(Key)>=0 then
              SQLWhere := SQLWhere+' (`'+Key+'` LIKE "%'+Value+'%")';
          end;

        end;
      end;
    finally
      RequestParams.Free;
    end;
  end;
  if SQLWhere<>'' then
    SQLWhere := ' WHERE'+SQLWhere;
  var Cmd := MySQLCommand(State, 'SELECT count(0) FROM `'+ResourceDef.Tablename+'`'+SQLWhere);
  Cmd.Execute;
  var TotalCount := 0;
  if Cmd.Reader.Read then
    TotalCount := Cmd.Reader.GetInteger(0);
  SQL := 'SELECT `'+ResourceDef.Attributes[0]+'`';
  for var Idx := 1 to ResourceDef.Attributes.Count-1 do
    SQL := SQL+',`'+ResourceDef.Attributes[Idx]+'`';
  SQL := SQL + ' FROM `'+ResourceDef.Tablename+'`';
  if SQLWhere<>'' then
    SQl := SQl+SQLWhere;
  var SortField: string;
  if TryGetRequestParamStr(State, 'sort', SortField) then
  begin
    var CleanSortField := SortField.TrimLeft(['-']);
    if ResourceDef.Attributes.IndexOf(CleanSortField)>=0 then
    begin
      SQL := SQL + ' ORDER BY '+CleanSortField;
      if CleanSortField.Length<>SortField.Length then
        SQL := SQL + ' DESC';
    end;
  end;
  var Arr := JSON_Data_Array(State);
  var PageNo: integer;
  var PageSize: integer;
  if TryGetRequestParamInt(State, 'page[number]', PageNo) and TryGetRequestParamInt(State, 'page[size]', PageSize) then
    SQL := SQL + ' LIMIT '+((PageNo-1)*PageSize).ToString+','+PageSize.ToString;
  Cmd := MySQLCommand(State, SQL);
  Cmd.Execute;
  var Reader := Cmd.Reader;
  while Reader.Read do
  begin
    var Data := TJSONObject.Create;
    Arr.Add(Data);
    // https://jsonapi.org/format/
    // The values of the id, type, and lid members MUST be strings.
    Data.AddPair('id', Cmd.Reader.GetString(0));
    Data.AddPair('type', ResourceName);
    var JsonAttr := TJSONObject.Create;
    Data.AddPair('attributes', JsonAttr);
    for var i := 1 to Reader.Fields.Count-1 do
    begin
      if ResourceDef.Attributes.Objects[i]=pointer(1) then
        JsonAttr.AddPair(Reader.Fields[i].Name, TJSONNumber.Create(Reader.GetInteger(i, true)))
      else
        JsonAttr.AddPair(Reader.Fields[i].Name, Reader.GetString(i, true));
    end;
  end;
  JSON_Meta_Object(State).AddPair('total', TJSONNumber.Create(TotalCount));
  State.SetContentType(CONTENT_TYPE_JSONAPI, CHARSET_UTF8);
  Result := true;
end;

class procedure THandler_DaJson.WrapUp(const State: PdwlHTTPHandlingState);
begin
  if State=nil then
    FTables.Free;
  inherited WrapUp(State);
end;

{ TResourceDef }

constructor TResourceDef.Create(const TableName, SQLWhere: string; var Attributes: TStringList);
begin
  inherited Create;
  FTableName := TableName;
  FSQLWhere := SQLWhere;
  FAttributes := Attributes; // take ownership
  Attributes := nil;
end;

destructor TResourceDef.Destroy;
begin
  FAttributes.Free;
  inherited Destroy;
end;

end.
