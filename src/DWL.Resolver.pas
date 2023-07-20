unit DWL.Resolver;

interface

uses
  System.Classes;

type
  TResolveKeyFunc = function(const Params: string): string;

  TdwlResolver = record
  strict private
  class var
    FResolvableReferences: TStringList;
    FKeyLead: string;
    FKeyTrail: string;
    FL_KeyLead: integer;
    FL_KeyTrail: integer;
  public
    /// <summary>
    ///   registers a function that replaces the key with the resolved value
    /// </summary>
    /// <param name="Key">
    ///   Key this function will resolve
    /// </param>
    /// <param name="ResolverFunc">
    ///   Callback function to be called when resolving
    /// </param>
    class procedure Register(const Key: string; ResolverFunc: TResolveKeyFunc); static;
    /// <summary>
    ///   resolve the text to be resolved using all the registered resolvers
    /// </summary>
    /// <param name="TextToBeResolved">
    ///   Text that will be replaced by its resolved value
    /// </param>
    class procedure Resolve(var TextToBeResolved: string); static;
    /// <summary>
    ///  Will resolve any reference to ${ until the next closing curly bracket
    ///  If the contents between the curly brackets is a number between
    ///  0 and 255, an ASCII-character is inserted instead of the reference.
    /// </summary>
    class procedure ResolveChars(var S: string); static;
    /// <summary>
    ///   Defines the global lead/trail combination to be used when enclosing
    ///   keys for resolving
    /// </summary>
    /// <param name="KeyLead">
    ///   the leading characters indicating a key to be resolved
    /// </param>
    /// <param name="KeyTrail">
    ///   the trailing characters indicating a key to be resolved <br />
    /// </param>
    /// <remarks>
    ///   The default lead is $( and the trail ) <br />For example: $(key)
    /// </remarks>
    class procedure SetKeyEnvelope(const KeyLead, KeyTrail: string); static;
    class function KeyLead: string; static;
    class function KeyTrail: string; static;
    class constructor Create;
    class destructor Destroy;
  end;

implementation

uses
  System.SysUtils;

{ TdwlResolver }

class constructor TdwlResolver.Create;
begin
  inherited;
  FResolvableReferences := TStringList.Create;
  FResolvableReferences.Duplicates := dupIgnore;
  FResolvableReferences.Sorted := true;
  SetKeyEnvelope('$(', ')');
end;

class destructor TdwlResolver.Destroy;
begin
  FResolvableReferences.Free;
  inherited;
end;

class function TdwlResolver.KeyLead: string;
begin
  Result := FKeyLead;
end;

class function TdwlResolver.KeyTrail: string;
begin
  Result := FKeyTrail;
end;

class procedure TdwlResolver.Register(const Key: string; ResolverFunc: TResolveKeyFunc);
begin
  if FResolvableReferences.IndexOf(LowerCase(Key))<0 then
    FResolvableReferences.AddObject(LowerCase(Key), pointer(@Resolverfunc));
end;

class procedure TdwlResolver.Resolve(var TextToBeResolved: string);
begin
  var Walker := 1;
  var L_S := length(TextToBeResolved);
  while Walker<L_S do
  begin
    if (TextToBeResolved[Walker]=FKeyLead[1]) and (Copy(TextToBeResolved, Walker, FL_KeyLead)=FKeyLead) then
    begin  // KeyLead found
      var ReferenceStart := Walker;
      var Param_Semicolon := 0;
      inc(Walker, FL_KeyLead);
      while (Walker<L_S) and ((TextToBeResolved[Walker]<>FKeyTrail[1]) or (Copy(TextToBeResolved, Walker, FL_KeyTrail)<>FKeyTrail)) do
      begin
        if (Param_Semicolon=0) and (TextToBeResolved[Walker]=';') then
          Param_Semicolon := Walker;
        inc(Walker);
      end;
      if Walker<=L_S then //KeyTrail found
      begin
        var Reference: string;
        var Params: string;
        if Param_Semicolon>0 then
        begin
          Reference := Copy(TextToBeResolved, ReferenceStart+FL_KeyLead, Param_Semicolon-ReferenceStart-FL_KeyLead);
          Params := Copy(TextToBeResolved, Param_Semicolon+1, Walker-Param_Semicolon-1);
        end
        else
        begin
          Reference := Copy(TextToBeResolved, ReferenceStart+FL_KeyLead, Walker-ReferenceStart-FL_KeyLead);
          Params := '';
        end;
        var RefIndex := FResolvableReferences.IndexOf(LowerCase(Reference));
        if RefIndex>=0 then //resolvable found
        begin
          var ReplaceStr := TResolveKeyFunc(FResolvableReferences.Objects[RefIndex])(Params);
          TextToBeResolved := Copy(TextToBeResolved, 1, ReferenceStart-1)+ReplaceStr+Copy(TextToBeResolved, Walker+FL_KeyTrail, MaxInt);
          L_S := length(TextToBeResolved);
          Walker := ReferenceStart-1; // evaluate ReplaceStr again
        end
        else
          inc(Walker, FL_KeyTrail-1);
      end;
    end;
    inc(Walker);
  end;
end;

class procedure TdwlResolver.ResolveChars(var S: string);
const
  dlCharRefStart='${';
  dlCharRefEnd='}';
  dlCharRefStartLength=2;
  dlCharRefEndLength=1;
begin
  var P := Pos(dlCharRefStart, S);
  while (P>0) do
  begin
    var Q := P-1+Pos(dlCharRefEnd, Copy(S, P, MaxInt));
    if Q>0 then
    begin
      var i := StrToIntDef(Copy(S, P+dlCharRefStartLength, Q-P+1-dlCharRefStartLength-dlCharRefEndLength), -1);
      if (i>0) and (i<256) then
        S := Copy(S, 1, P-1)+chr(i)+Copy(S, Q+dlCharRefEndLength, MaxInt)
      else
        S := Copy(S, 1, P-1)+Copy(S, Q+dlCharRefEndLength, MaxInt);
      P := Pos(dlCharRefStart, S);
    end
    else
      P := 0;
  end;
end;

class procedure TdwlResolver.SetKeyEnvelope(const KeyLead, KeyTrail: string);
begin
  FKeyLead := KeyLead;
  FKeyTrail := KeyTrail;
  FL_KeyLead := length(FKeyLead);
  FL_KeyTrail := length(FKeyTrail);
end;

end.



