unit export2man;

{$mode objfpc}{$H+}

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    Exports a note in a subset of man page, the new file always get its name
    from the Note Title. Any text content that is marked as Italic is ignored.
    The only other significent markup is Huge which marks Section Headers, some
    Section Headers have especially defined behaviour (OPTIONS,  SYNOPSIS) and
    remainder, including any the user dreams up, are treated as plain text.

    The Exporter (in TomboyTools) does not require any particular Section
    Headers but a good man page does.

    A associated Tomboy template is essencial, it also provides a measure of
    documentation on what to put under the various Section Headings.

    Create the object, give it a directory to look in, a FFN file name and a
    directory to write to.

    Note that unlike CommnoMark, this unit saves its own files and does not get
    passed a StringList, so, output cannot be used programatically.

    COMMAND LINE to see the resulting .1 file as a man page -
    groff -man -Tascii tomboy-ng_1.1

    HISTORY
    2021-08-04  First Commit.


}

interface

uses
        Classes, SysUtils;


type                { TExport2Man }

TExport2Man = class

    private
        STL : TStringList;
        Index : integer;
        OutFileName : string;   // Gets the FFName of output file.

        procedure DoGeneric();
        procedure DoOptions();
        procedure DoSynopsis();
        procedure EscapeChar();
        function GetMancontent(InFFN: string): boolean;
                        // Finds and returns the next heading, removes the huge tags and adds Section Header tags
        function NextHeading: string;
        procedure RemoveAllMarkUp();
        procedure RemoveItalic();
                        // Replaces the indicated line in STL with he passed one
        procedure SwapLines(Line : Integer; NewLn: string);


    public
        DebugMode : boolean;
        ErrorString : string;   // Set to something if an error occured
        NotesDir : string;      // dir were we expect to find our TB notes, must have trailing path delim
        OutDir   : string;      // a dir to write our html files to, must have trailing path delim

        function ExportFile(InFile: string): boolean;
        // constructor Create();

end;




implementation

uses tb_utils, LazFileUtils{$ifdef LCL}, lazlogger {$endif}, laz2_DOM, laz2_XMLRead, notenormal, FileUtil;

{ TNoteList }

// ================= E X P O R T   M A N  ====================================

procedure  TExport2Man.RemoveItalic();
var
    St : string;
    i : integer = 0;
    Start, Finish : integer;
    ContinuingTag : boolean = false;
begin
    while i < STL.count do begin
        if (STL[i] <> '') and (ContinuingTag or (pos('<italic>', STL[i]) > 0)) then begin
            // We cannot assume that opening and closing tags exist in same line !
            ST := STL[i];
            if ContinuingTag then
                Start := 1
            else Start := pos('<italic>', ST);
            Finish := pos('</italic>', ST);
            if Finish < 1 then begin
                ContinuingTag := True;
                Finish := length(St);
            end else ContinuingTag := False;
            delete(St, Start, Finish - Start +9);
            Start := 1;                 // warning, using Start for another purpose !
            while length(St) > 0 do     // remove leading whitespace
                 if St[1] = ' ' then delete(St, 1, 1)
                 else break;
            STL.delete(i);
            if St <> '' then begin      // OK, is there anything left there ?
                StL.Insert(i, St);
                inc(i);
            end;
        end else
            inc(i);
    end;
end;

procedure  TExport2Man.RemoveAllMarkUp();
var
    St : string;
    i : integer = 0;
begin
    while i < STL.count do begin
        if STL[i] <> '' then begin
            ST := STL[i];
            ST := St.Replace('<bold>', '', [rfReplaceAll]);
            ST := St.Replace('</bold>', '', [rfReplaceAll]);
            //ST := St.Replace('<italic>', '', [rfReplaceAll]);
            //ST := St.Replace('</italic>', '', [rfReplaceAll]);
            ST := St.Replace('<size:small>', '', [rfReplaceAll]);
            ST := St.Replace('</size:small>', '', [rfReplaceAll]);
            ST := St.Replace('<strikeout>', '', [rfReplaceAll]);
            ST := St.Replace('</strikeout>', '', [rfReplaceAll]);
            ST := St.Replace('<highlight>', '', [rfReplaceAll]);
            ST := St.Replace('</highlight>', '', [rfReplaceAll]);
            ST := St.Replace('<monospace>', '', [rfReplaceAll]);
            ST := St.Replace('</monospace>', '', [rfReplaceAll]);
            ST := St.Replace('<list><list-item dir="ltr">', '', [rfReplaceAll]);
            ST := St.Replace('</list></list-item dir="ltr">', '', [rfReplaceAll]);
            ST := St.Replace('<size:large>', '', [rfReplaceAll]);
            ST := St.Replace('</size:large>', '', [rfReplaceAll]);
            ST := St.Replace('<underline>', '', [rfReplaceAll]);
            ST := St.Replace('</underline>', '', [rfReplaceAll]);
            St := RestoreBadXMLChar(St);
            SwapLines(i, St);
        end;
        inc(i);
    end;
end;

// I am aware we must escape both '-' and '\', are there more ?
procedure TExport2Man.EscapeChar();
var
    i : integer = 1;
    St : String;
begin
    while i < STL.count do begin
        St := STL[i];
        St := St.Replace('\', '\\', [rfReplaceAll]);
        St := St.Replace('-', '\-', [rfReplaceAll]);
        if St <> STL[i] then begin
            Stl.Delete(i);
            STL.Insert(i, ST);
        end;
        inc(i);
    end;
end;

function TExport2Man.ExportFile(InFile : string) : boolean;
var
    //FFN : string;
    BaseName : string;
    ExtName : char;
begin
    // if the title ends with a space and a digit, we use that as part of file name, else assume '1'
    // trouble is, we will have already munged spaces to underscore.
    BaseName := lowercase(GetTitleFromFFN(InFile, True));
    if (length(BaseName) > 2)
            and (BaseName[length(BaseName)-1] = '_')
            and  (BaseName[length(BaseName)] in ['0'..'9']) then begin
                    ExtName := BaseName[length(BaseName)];
                    delete(BaseName, length(BaseName) -1, 2);
                    BaseName := BaseName + '.' + ExtName;
    end else BaseName := BaseName + '.1';
    OutFileName := OutDir + BaseName;
    STL := TStringList.create;                  // Used elsewhere but we create and free here.
    try
        Result := GetMancontent(InFile);
    finally
        STL.Free;
    end;
end;


procedure TExport2Man.SwapLines(Line : Integer; NewLn : string);
begin
    StL.Delete(Line);
    STL.Insert(Line, NewLn);
end;

// Returns True if the characters match for the length of shorter string.
function MyStrCmp(const Shorter, Longer : string) : boolean;
var
    I : integer;
begin
    I := length(Shorter);
    if I > length(Longer) then exit(false);
    while i <> 0 do begin
        if Shorter[i] <> Longer[i] then exit(false);
        dec(i);
    end;
    Result := True;
end;


function TExport2Man.NextHeading : string;
begin
    while Index < STL.Count do begin
        if MyStrCmp('<size:huge>', STL[Index]) then begin
            Result := STL[Index];
            Result := Result.Replace('<size:huge>', '');
            Result := Result.Replace('</size:huge>', '');
            SwapLines(Index, '.SH ' + Result);
            Inc(Index);                         // ToDo : what if last line contains a header ?
            exit;
        end;
        inc(Index);
    end;
    Result := '';
end;

procedure TExport2Man.DoSynopsis();             // seems we don't need this ?
{var
    St : string;    }
begin
    while Index < STL.count do begin
        if MyStrCmp('<size:huge>', STL[Index]) then exit;   // Thats next header;
        inc(Index);
    end;
end;

procedure TExport2Man.DoGeneric();
var
    St : string;
begin
    while Index < STL.count do begin
        if MyStrCmp('<size:huge>', STL[Index]) then exit;   // Thats next header;
        St := STL[Index];
        // do something with it  ??
        inc(Index);
    end;
end;

// if we have text here that is not the next Section Header, we assume its pairs of
// lines, an option and then a description. Only one line would be an error.
//

procedure TExport2Man.DoOptions();
var
    St : string;
begin
    while Index < STL.count do begin
        if MyStrCmp('<size:huge>', STL[Index]) then exit;   // Thats next header;
        St := STL[Index];
        if St <> '' then begin
            STL.Insert(Index, '.TP');
            inc(Index);                                     // point to option, we know its content anyway.
            inc(Index);                                     // Now point to desc
            if Stl[Index] = ''  then begin                          //Hmm, that should have content
                StL.insert(Index, '***ERROR*** Option [' + StL[Index-1] +'] does not have a description');
                ErrorString := 'Error Occured, see output file';
                inc(Index);
            end else begin
                if MyStrCmp('<size:huge>', STL[Index]) then begin
                    StL.insert(Index, '***ERROR*** Option [' + StL[Index-1] +'] does not have a description');
                    ErrorString := 'Error Occured, see output file';
                    exit;
                end;
                inc(Index);                                 // Assume valid content, its something and not a heading.
            end;
            if STL[Index] <> '' then begin                  // Now to either a blank line or next item
                STL.Insert(Index, '');
                inc(index);
            end
        end else
            inc(Index);
    end;
end;




function TExport2Man.GetManContent(InFFN : string): boolean;
var
    Normaliser : TNoteNormaliser;
    Head : string;
    //St : string;
begin
        if not FileExists(InFFN) then begin
            SayDebugSafe('ERROR, could not find ' + InFFN);
            exit(False);
        end;
        StL.LoadFromFile(InFFN);
        Normaliser := TNoteNormaliser.Create;
        Normaliser.NormaliseList(StL);
        Normaliser.Free;
        RemoveNoteMetaData(STL);// tt_utils
        // That should leave us with the first line being Title
        SwapLines(0, '.TH ' + STL[0]);
        RemoveItalic();
        RemoveAllMarkUp();
        Index := 1;
        while Index < STL.Count do begin
            Head := NextHeading();
            case Head of
                '' : break;
                'SYNOPSIS' : DoSynopsis();
                'OPTIONS'  : DoOptions();
            otherwise DoGeneric();
            end;
        end;
        // All Section Headings done.

        EscapeChar();

        if (Stl.Count < 2) then begin
            SayDebugSafe('Not saving ' + OutFileName + ' because its less than two lines');
            exit(False);
        end;
        if FileExistsUTF8(OutFileName) then
            DeleteFileUTF8(OutFileName);
        if FileExistsUTF8(OutFileName) then exit(False)
        else StL.SaveToFile(OutFileName);
        Result := True;
end;


end.

