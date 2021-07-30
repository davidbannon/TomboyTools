unit TT_utils;


{ A collection of small and unrelated utilites that are for use by TomBoyTools
  only, tomboy-ng has them already.

  Possible conside moving some of thes into tb_utils if it seeems to make sense.

}

{$mode ObjFPC}{$H+}

interface
uses
    Classes, SysUtils;

(*                        // Passed  FFN, thats <path><ID><.note> and returns the Title, munge indicates
                        // make it suitable for use as a file name, an empty ret string indicates error
function GetTitleFromFFN(FFN: string; Munge : boolean{; out LenTitle : integer}): string;

                        // Returns the default place for tomboy-ng notes.
//function GetDefaultNoteDir : string;

                        // Remove all content up to and including <note-content ...> and all content
                        // including and after </note-content>.  Because we cannot guarantee that these
                        // lines are on their own, we will need to poke into individual lines.
                        // Maybe tolerant of gnote format.
procedure RemoveNoteMetaData(STL : TStringList);

procedure SayDebugSafe(st: string);                       *)

implementation

uses LCLProc, laz2_DOM, laz2_XMLRead, LazFileUtils, tb_utils;

(*
function GetDefaultNoteDir : string;
begin
    {$IFDEF UNIX}
    Result := GetEnvironmentVariable('HOME') + '/.local/share/tomboy-ng/';
    {$ENDIF}
    {$IFDEF DARWIN}
    // try the correct place first, if not there, lets try the old, wrong place
    // if at neither, we go back to correct place.
    Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/Tomboy-ng/Notes/';
    if DirectoryExistsUTF8(Result) then exit;
    Result := GetEnvironmentVariable('HOME') + '/.local/share/tomboy-ng/';
    if not DirectoryExistsUTF8(Result) then
        Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/Tomboy-ng/Notes/';
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := GetEnvironmentVariable('APPDATA') + '\tomboy-ng\notes\';
    // %APPDATA%\Tomboy\notes\
    {$ENDIF}
end;            *)


(*
procedure SayDebugSafe(st: string);
begin
    {$ifdef LCL}Debugln{$else}writeln{$endif}(St);
end;

function GetTitleFromFFN(FFN: string; Munge : boolean{; out LenTitle : integer}): string;
var
    Doc : TXMLDocument;
    Node : TDOMNode;
//    Index : integer = 1;
begin
    if not FileExists(FFN) then begin
        debugln('ERROR : File does not exist = '  + FFN);
        exit('');
	end;
	ReadXMLFile(Doc, FFN);
    try
        Node := Doc.DocumentElement.FindNode('title');
        result := Node.FirstChild.NodeValue;
    finally
        Doc.free;
    end;
    if Munge then
        Result := TB_MakeFileName(Result);

{    begin
        // remove char that don't belong in a file name
        while Index <= length(Result) do begin
                if Result[Index] in [ ' ', ':', '.', '/', '\', '|', '"', '''' ] then begin
                    Result[Index] := '_';
                end;

                inc(Index);
        end;
        Result := copy(Result, 1, 42);      // Because 42 is the meaning of life
	end;  }
    if Result = '' then Debugln('Title not found' + FFN);
    //LenTitle := length(Result);
end;


// Remove all content up to and including <note-content ...> and all content
// including and after </note-content>.  Because we cannot guarantee that these
// lines are on their own, we will need to poke into individual lines.
procedure RemoveNoteMetaData(STL : TStringList);
var
    Index, CutOff : integer;
    St : string;
begin
    // First, the trailing end.
    Index := FindInStringList(StL, '</note-content>');       // this is the line its on but we may have content on the same line
    St := Stl[Index];
    CutOff := pos('</note-content>', St);
    if CutOff <> 1 then begin
        delete(St, CutOff, 1000);
        STL.Delete(Index);
        STL.Insert(Index, St);
    end;
    inc(Index);     // Get rid of the remainder.
    while Index < StL.Count do StL.Delete(Index);
    // OK, now the start of the list
    Index := FindInStringList(StL, '<note-content');
    while Index > 0 do begin
        STL.Delete(0);
        dec(Index);
    end;
    St := STL[0];
    CutOff := St.IndexOf('>', St.IndexOf('<note-content')) +1;      // Zero based index !
    delete(St, 1, CutOff);
    STL.Delete(0);
    STL.Insert(0, St);
end;                         *)

end.

