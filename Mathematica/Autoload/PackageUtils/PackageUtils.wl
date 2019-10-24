(* ::Package:: *)

(* ::Title:: *)
(*PackageUtils*)


(* ::Author:: *)
(*author: Michal Mandrysz*)


(* ::Affiliation:: *)
(*Marian Smoluchowski Institute of Physics, Jagiellonian University, Krakow, Poland*)


(* ::Abstract:: *)
(*This package is a collection of function used for notebook formatting, pdf exporting etc.*)


(* ::Text:: *)
(*Version: 1.0.0*)


BeginPackage["PackageUtils`"];


(* ::Section:: *)
(*Public messages*)


(* ::Text:: *)
(*It is useful to see more recently opened notebooks*)


SetOptions[$FrontEnd,"NotebooksMenuHistoryLength"->20]


ShowStatus::usage="Prints the message in the status bar";
EmbedNote::usage="Embedes cells with tag from a notebook located at path";
DuplicateNotebook::usage="Makes a copy of the notebook";
PrintToConsole::usage="Send to console";
HideOutput::usage="Hides output cells";
MergeStyle::usage ="Merges stylesheet with the notebook and saves in the same directory with the postfix _sm";
CloseCollapsed::usage="Closes collapsed Section group cells \[Dash] good for retaining numbering during export of single sections";
CodeVisible::usage = "Shows/Hides code and cell tags";
PublishToPDF::usage="Saves a publishing ready version, optional argument for copy (pendrive)";
ExportToPDF::usage="Option for ExportToTeX";
EmbedRefrencesBeforeExport::usage="Option for ExportToTeX";
WriteTOC::usage="Option for ExportToTeX";
BibFile::usage="Option for ExportToTeX";
WriteAuthors::usage="Option for ExportToTeX";
WriteDate::usage="Option for ExportToTeX";
WriteTitle::usage="Option for ExportToTeX";
SetTeXMargin::usage="Option for ExportToTeX";
CustomTeXCommands::usage="Option for ExportToTeX";
TeXLanguage::usage="Option for ExportToTeX";
ShowTeXLabels::usage="Option for ExportToTeX";
TeXLineSpread::usage="Option for ExportToTeX";
TeXPlotScale::usage="Option for ExportToTeX";
FitEquations::usage="Option for ExportToTeX";
ExportToTeX::usage="Generates a draft in Tex with an option to compile to PDF";
CreateTOC::usage="Create table of contents";
CellStrip::usage="Simple cell stripper, removes BoxData and Cell";
StyleButton::usage="Creates a button to create a style Cell of a specific name";
WordStats::usage="Prints current notebook word/character stats in Status area";
GetReal::usage="Returns items with real coefs";
SearchBar::usage="Search bar";
Outline::usage ="Displays document outline";
WorkingEnv::usage="Show all cell metadata and Input cells";
PrintoutEnv::usage="Hides all cell metadata and Input cells";
Recent::usage="Shows recently opened notebooks";
TranslateSpecialCellStyleNames::usage="For internal usage";
RefCellTooltip::usage="Generates a tooltip for referenced cells, takes tag and subtag";
ContentsByCellStyle::usage="Extracts (assumed)text contents of a cells by style";
Board::usage="Creates reusable Table with caption from input";
BoardColumn::usage="The same as board but with vertical split";
Figure::usage="Creates reusable Figure with caption from input";
IncludePath::usage="Includes a path to $Path variable, for easy loading";
FullDerivativesForm::usage="Prints eqs in TraditionalForm transforming derivatives to standard, LaTeX readable form";
FieldTheoryForm::usage="Prints eqs in TraditionalForm and moves derivatives to subscripts, best used on not-already-subscripted symbols";
ReferenceBox::usage="Used for printing dynamic references in text. Takes tag, subtag";


(* ::Section:: *)
(*Implementations*)


Begin["`Private`"];
FullDerivativesForm[f_]:=TraditionalForm[f/.Derivative[inds__][g_][vars__]:>Apply[Defer[D[g[vars],##]]&,Transpose[{{vars},{inds}}]/.{{var_,0}:>Sequence[],{var_,1}:>{var}}]];
FieldTheoryForm[f_,parameterVars_List:{}]:=TraditionalForm[f//.{Derivative[inds__][g_Subscript][vars__Symbol]:>(Subscript[g[[1]](*,ToString/@*),##2&@@g,(*ToExpression@RowBox@*)Symbol@StringJoin[ToString/@Riffle[MapThread[ConstantArray[#1,#2]//.{List->Sequence}&,{{vars},{inds}}],"\[Null]"] ]]),Derivative[inds__][g_][vars__Symbol]:>Subscript[g,(*ToExpression@RowBox@*)Symbol@StringJoin[ToString/@Riffle[MapThread[ConstantArray[#1,#2]//.{List->Sequence}&,{{vars},{inds}}],"\[Null]"]]]}/.{(F_Symbol|F_Subscript)[a__]/;(ToString[Quiet[Check[Context[F],"SubscriptedSymbol",General::ssle],General::ssle]]=!="System`"):>F},ParameterVariables->parameterVars];
ReferenceBox[t_,ysubt_,subt_]:=Block[{style,secID},
 style=(If[#==="Board","Table",#])&@AbsoluteCurrentValue[First[Cells[EvaluationNotebook[],CellTags->{t}]],"CellStyleName"];
 secID=AbsoluteCurrentValue[First[Cells[EvaluationNotebook[],CellTags->{t}]],{"CounterValue","Section"}];
 RowBox[
   If[style==="Section",
     {"(",secID,")"},
     If[secID===0,
       If[ysubt,{"(",AbsoluteCurrentValue[First[Cells[EvaluationNotebook[],CellTags->{t}]],{"CounterValue",style}],".",subt,")"},{"(",AbsoluteCurrentValue[First[Cells[EvaluationNotebook[],CellTags->{t}]],{"CounterValue",style}],")"}],
       If[ysubt,{"(",secID,".",AbsoluteCurrentValue[First[Cells[EvaluationNotebook[],CellTags->{t}]],{"CounterValue",style}],".",subt,")"},{"(",secID,".",AbsoluteCurrentValue[First[Cells[EvaluationNotebook[],CellTags->{t}]],{"CounterValue",style}],")"}]
     ]
   ]]];
 RefCellTooltip[x_,y_:None]:=Block[{ce,cs,secID,capt},
 ce=First@Cells[EvaluationNotebook[], CellTags -> {x}];
 cs=AbsoluteCurrentValue[ce,"CellStyleName"]; 
 secID=AbsoluteCurrentValue[ce,{"CounterValue","Section"}];
 capt=If[secID===0,{If[y=!=None,y,TranslateSpecialCellStyleNames[cs]], " ", AbsoluteCurrentValue[ce,{"CounterValue", If[cs==="Board","Table",cs]}], "\n"},
 {If[y=!=None,y,TranslateSpecialCellStyleNames[cs]], " ", secID, ".", AbsoluteCurrentValue[ce,{"CounterValue", If[cs==="Board","Table",cs]}], "\n"}];
 RowBox[Prepend[Flatten[{CellStrip[If[y=!=None,Cases[NotebookRead[ce], Cell[Pattern[A, BlankSequence[]], CellTags->y] -> Cell[A], Infinity], NotebookRead[ce]]]}], 
 StyleBox[RowBox[capt], "Subsubsection"]]]];


ObjCaption[style_String]:=RowBox[{TranslateSpecialCellStyleNames[style],CurrentValue[{"CounterValue","Section"}],".",CurrentValue[{"CounterValue",If[style==="Board","Table",style]}]}];
TagInput[base_String,tag_:Automatic]:=Block[{tagt,baseTag,tmp},
baseTag="@"<>base;
t=(TaggingRules/.Options[EvaluationCell[],TaggingRules]);
tmp=If[ListQ@t,baseTag/.t,baseTag];
tagt=If[tag=!=Automatic,tag,
(*If[Head@Unevaluated@content===Symbol,SymbolName[Unevaluated[content]],*)
If[tmp=!=baseTag,tmp,
tmp=StringJoin[Capitalize@RandomWord[#,IncludeInflections->True,Language->"English"]&/@{"Adjective","Noun"}];
SetOptions[EvaluationCell[],TaggingRules->If[ListQ[t],Join[t,{baseTag->tmp}],If[StringQ[t],Join[{t},{baseTag->tmp}],{baseTag->tmp}]]];
tmp
]
]
];
CreateObj[style_String,capstyle_String,tag_:Automatic,captionFirst_:False]:=Block[{c,tagt,tmp},
tagt=TagInput[style,tag];
c=NotebookFind[EvaluationNotebook[],tagt,All,CellTags];
tmp=c===$Failed;
If[tmp,FrontEnd`NotebookWrite[EvaluationNotebook[],
CellPrint[Cell[BoxData[FormBox["\[Placeholder]",TraditionalForm]],style,CellAutoOverwrite->False,CellTags->tagt]
],All,AutoScroll->False];FrontEnd`NotebookWrite[EvaluationNotebook[],CellPrint[TextCell["\[Placeholder]",capstyle,CellAutoOverwrite->False]
],All,AutoScroll->False]];
c=NotebookFind[EvaluationNotebook[],tagt,All,CellTags];
tmp
];
SetAttributes[Figure, HoldFirst];
SetAttributes[Board, HoldFirst];
SetAttributes[BoardColumn, HoldFirst];
Figure[content_,tag_:Automatic]:=Block[{createdNew},
createdNew=CreateObj["Figure","FigureCaption",If[tag===Automatic&&Head@Unevaluated@content===Symbol,SymbolName[Unevaluated[content]],tag]];
Paste[EvaluationNotebook[],content];
SelectionMove[EvaluationNotebook[],Next,Cell,1,AutoScroll->False];
SelectionMove[EvaluationNotebook[],After,CellContents,1,AutoScroll->False];
];
(*Board[content_,tag_:Automatic,row_:True,opts___]:=Block[{createdNew},
createdNew=CreateObj["Board",If[tag===Automatic&&Head@Unevaluated@content===Symbol,SymbolName[Unevaluated[content]],tag],True];
SelectionMove[EvaluationNotebook[],After,CellContents,AutoScroll->False];
SelectionMove[EvaluationNotebook[],Previous,Line,1,AutoScroll->False];
If[!createdNew,SelectionMove[EvaluationNotebook[],All,Expression,AutoScroll->False],Unevaluated[Sequence[]]];
NotebookWrite[EvaluationNotebook[],GridBox[content,opts,GridBoxAlignment->{"Columns"->{{Left}},"ColumnsIndexed"->{},"Rows"->{{Center}},"RowsIndexed"->{}},GridBoxDividers->{"Columns"->{False,If[!row,AbsoluteThickness[1],Nothing],{False},False},"ColumnsIndexed"->{},"Rows"->{AbsoluteThickness[2],If[row,AbsoluteThickness[1],Nothing],{False},AbsoluteThickness[2]},"RowsIndexed"->{}},GridBoxItemSize->{"Columns"->{{All}},"ColumnsIndexed"->{},"Rows"->{{1.2}},"RowsIndexed"->{}},GridDefaultElement:>"\[Placeholder]"]];
SelectionMove[EvaluationNotebook[],Previous,Expression,2,AutoScroll->False];
];
ExtFT[cs_,cont_]:=If[cs==="Board",Replace[cont,{FormBox[GridBox[{_,{C___},{D___}},E___],F___]:>FormBox[GridBox[{{C},{D}},E],F]},Infinity],If[cs==="Figure",Replace[cont,{FormBox[GridBox[{{C___},_,{D___}},E___],F___]:>FormBox[GridBox[{{C},{D}},E],F]},Infinity],cont]];
BoardColumn[content_,tag_:Automatic,opts___]:=Board[content,tag,False,opts];
*)

DuplicateNotebook[]:=NotebookPut@NotebookGet[EvaluationNotebook[]];
TranslateSpecialCellStyleNames[name_]:=If[AbsoluteCurrentValue["Language"] == "Polish",Switch[name,"Example","Przyk\[LSlash]ad","Exercise","Zadanie","Solution","Rozwi\:0105zanie","Question","Pytanie","Remark","Uwaga","Comment","Komentarz",
"Theorem","Twierdzenie","Proof","Dow\[OAcute]d","Axiom","Aksjomat","Definition","Definicja","Lemma","Lemat","Corollary","Wniosek","Title","Tytu\[LSlash]","Subtitle","Podtytu\[LSlash]","Author","Autor","Section","Sekcja","Subsection","Podsekcja","Subsubsection","Podpodsekcja","Text","Tekst","Item1","Pozycja","Equation","R\[OAcute]wnanie","EquationNumbered","R\[OAcute]wnanie numerowane","Figure","Rysunek","Table","Tabela","Board","Tabela",_,name],Switch[name,"Board","Tabela",_,name]];
CloseCollapsed[]:=(SetOptions[#[[1]],CellOpen->!(CellOpen/.Options[#[[1]],CellOpen])];&/@Select[{#,CurrentValue[#,"CellGroupOpen"]}&/@Cells[EvaluationNotebook[], CellStyle -> "Section"],#[[2]]==$Failed||#[[2]]==Closed&];)

Outline[]:=CreatePalette[
Dynamic[
Refresh[
Column[
First/@
DeleteCases[
Cases[
NotebookRead@{#},
Cell[name_,style:"Title"|"Section"|"Subsection"|"Subsubsection"|"Subsubsubsection",___,CellID->id_,___]:>
Button[DisplayForm[
If[style=="Title",Cell[Style[name,TextAlignment->Axis],"Text",(*FontWeight\[Rule]Bold,*) FontSize->16,TextAlignment->Center,CellSize->{Full,Automatic}],
Short@Row[{Style[(StringPadLeft["     ",IntegerPart[(StringLength[style]-7)/3]*2]<>ToString[CurrentValue[#,{"CounterValue",style}]]<>". "),Bold,13],Cell[name,"Text",FontSize->13,CellSize->{Full,Automatic}]},ImageSize->Full,BaseStyle->"Text"]
]
],(
NotebookFind[SelectedNotebook[],id,All,CellID];
SelectionMove[SelectedNotebook[],All,CellGroup];
FrontEndTokenExecute["OpenCloseGroup"];
), Appearance->"Frameless", Alignment->Center, ImageMargins->0, FrameMargins->{{5,0},{0,0}},Background->If[TrueQ[CurrentValue[#,"CellGroupOpen"]==Open],White,Lighter@LightGray],ImageSize->{Full,Full}]]
&/@Cells[SelectedNotebook[],CellStyle->{"Title","Section","Subsection","Subsubsection","Subsubsubsection"}],{}],ItemSize->{30,Full},Alignment->{Center,Left}],UpdateInterval->0.5],"Temporary"],WindowSize->{Fit,650},WindowFloating->True,WindowMargins->{{Automatic,50},{Automatic,58}},WindowElements->{"VerticalScrollBar"},Saveable->False,WindowTitle->Dynamic[AbsoluteCurrentValue[SelectedNotebook[],"WindowTitle"],"Temporary",SynchronousUpdating->True]
];

SearchBar=ExpressionCell[Row@{InputField[Dynamic[search],String,ContinuousAction->True],"  ",Button["search",sdm],"  ",Button["show all",sa]}];

WorkingEnv[]:=(DynamicModule[{nb},nb=SelectedNotebook[];
SetOptions[#,CellOpen->True,ShowCellTags->True, ShowCellLabel->True,ShowCellBracket->True]&/@Cells[nb];SetOptions[EvaluationNotebook[],ScreenStyleEnvironment->"Working"]]);

Recent[]:=Column[NotebooksMenu/.Options[$FrontEnd]//MapAt[ToFileName@@#[[1, ;;2]]&,{All,2}]];

IncludePath[path_]:=If[Not[MemberQ[$Path,path]],$Path=Flatten[{$Path,path}]];

PrintoutEnv[]:=DynamicModule[{nb},nb=SelectedNotebook[];
SetOptions[#,ShowCellLabel->False,ShowCellTags->False,ShowCellBracket->False]&/@Cells[nb];
SetOptions[#,CellOpen->False]&/@Cells[nb,CellStyle->"Input"];
SetOptions[EvaluationNotebook[],ScreenStyleEnvironment->"Printout"]];

sa:=DynamicModule[{nb},nb=EvaluationNotebook[];
SetOptions[#,CellOpen->True,ShowCellLabel->True, ShowCellBracket->True]&/@Cells[nb]]

sdm:=DynamicModule[{nb},nb=EvaluationNotebook[];
NotebookFind[nb,search,All];
SetOptions[#,CellOpen->False,ShowCellBracket->False]&/@Cells[nb];
SetOptions[#,CellOpen->True,ShowCellBracket->True]&/@SelectedCells[nb];]

PrintToConsole[expr_]:=(SetSelectedNotebook[MessagesNotebook[]];
NotebookWrite[SelectedNotebook[],Cell[BoxData[ToBoxes[expr]],"Print"]]);

ShowStatus[status_]:=LinkWrite[$ParentLink,SetNotebookStatusLine[FrontEnd`EvaluationNotebook[],ToString[status]]];

StyleButton[name_]:=Button[name,SelectionMove[SelectedNotebook[],All,Cell];FrontEndExecute@FrontEndToken[SelectedNotebook[],"Style",name]];

GetReal[sols_]:=Module[{thrd,rule},
thrd=Thread[Variables[sols]->1];
Pick[sols,Element[#,Reals]&/@(sols/.thrd)]
];

CellStrip[data_]:=ReplaceRepeated[data,{Cell[c_,___]:>c,BoxData[d__]:>d,TextData[ff_]:>ff}]

EmbedNote[notebookpath_,tag_]:=Block[{m},
Catch[
NotebookWrite[
EvaluationNotebook[],
Cell[CellGroupData[
Cases[First@Get[If[(m=FindFile[notebookpath])=!=$Failed,m,Throw["File \""<> notebookpath <>"\" not found"]]],Cell[__,CellTags->(tag|{___,tag,___}),___],\[Infinity]]],CellFrame->{{0,0},{0,0}},CellDingbat->ButtonBox[StyleBox["\[LeftAngleBracket]","Dingbat"],Appearance->"Frameless",Tooltip->notebookpath,ButtonFunction->(NotebookLocate[#]&),ButtonData->{m,tag}]
]
]
]
];

WordStats[]:=
Module[{cells,dat,data,characters,words},
cells=Cells[EvaluationNotebook[],CellStyle->{"Title","Section","Subsection","Subsubsection","Subsubsubection","Text","EquationNumbered","Equation","Item1","Item2","Item3","Item1Numbered","Item2Numbered","Item3Numbered"}];
data=NotebookRead@cells;
dat=ReplaceRepeated[data,
{
RowBox[{C__String}]:>StringJoin@C,
StyleBox[D__,Background->None]:>StyleBox[D],
StyleBox[D_String]:>D,
Cell[TextData[data_],___]:> data,
Cell[BoxData[data_],___]:> data,
Cell[data_,___]:> data,
Cell[BoxData[data_],___]:> data,
FormBox[C_String,TextForm|TraditionalForm]:> C
}];
dat=Block[{n=1},ReplaceAll[dat,{
(FormBox[RowBox[{C__String}],TextForm]|FormBox[RowBox[{C__String}],TraditionalForm]):>StringJoin@C,
(FormBox[C__, TraditionalForm]|FormBox[C__, TextForm]):>"[$]"<>Convert`TeX`BoxesToTeX[C, "BoxRules"->{StyleBox[D_,Background->LightGreen]:> "[/$]{{c"<>ToString[n]<>"::[$]" <>StringReplace[Convert`TeX`BoxesToTeX [D],{"{{":> " { { ","}}":> " } } "}] <>"[/$]}}[$]"}]<>"[/$] ",

StyleBox[D_, Background->LightGreen]:>("{{c"<>ToString[n]<>"::"<>D<>"}}"),
StyleBox[D_String,___,FontWeight->"Bold",___]:> D,
StyleBox[D_String,___,FontSlant->"Italic",___]:> D,
StyleBox[D_String,___,FontWeight->"Plain",___]:> D,
StyleBox[D_String,___,FontVariations->{___,"Underline"->True,___},___]:> (D),
StyleBox[D_String,___,FontVariations->__,___]:> D,
StyleBox[D_, Background->_]:>ToString[n],
ButtonBox[RowBox[{_,CounterBox[_,N_],_}],___]:> (Convert`TeX`BoxesToTeX@First@Cases[data,Cell[name_,___,CellTags->N,___]:>name,Infinity]),
ButtonBox[RowBox[{_,CounterBox[_],_}],___]:> "",
CounterBox["FigureCaptionNumbered",N_]:> "",
CounterBox["FigureCaptionNumbered"]:> "",
ButtonBox[___]:> ""
}]];
words=ToString@Total[WordCount[#]&/@Flatten[dat]];
characters=ToString@Total[StringLength[#]&/@Flatten[dat]];
ShowStatus["Counted: "<>characters<>" characters (with spaces) and " <> words<> " words"];
];

MergeStyle[]:=Module[{path,child,childstyles,parent,nb,tmp,parentparent,parentstyles, old},(*find the parent stylesheet from the private stylesheet,a.k.a child*)
If[NotebookDirectory[]===$Failed, Abort[]];
path=ToFileName[{$UserBaseDirectory,"SystemFiles","FrontEnd","StyleSheets"}];
child=Options[EvaluationNotebook[],StyleDefinitions];
parent=First@Cases[Options[EvaluationNotebook[],StyleDefinitions],(StyleDefinitions-> x_):>x,\[Infinity]] ;
childstyles=Cases[child,Cell[StyleData[_,___],__],\[Infinity]];
(*get the parent*)
nb=NotebookOpen[path<>parent];
tmp=NotebookGet[nb];
NotebookClose[nb];
(*find the parent definition*)parentparent=Cases[tmp,Cell[StyleData[StyleDefinitions->x_]],\[Infinity]];
(*scrape the styles*)parentstyles=Cases[tmp,Cell[StyleData[x_,y___],z__],\[Infinity]];
(*merge parents parent,child styles,parent styles*)SetOptions[EvaluationNotebook[],StyleDefinitions->Notebook[Join[parentparent,childstyles,parentstyles],StyleDefinitions->"PrivateStylesheetFormatting.nb"],DockedCells->None];

old=FileBaseName[NotebookFileName[]];
NotebookSave[EvaluationNotebook[],NotebookDirectory[EvaluationNotebook[]]<>old<>"_sm.nb"];
NotebookOpen[NotebookDirectory[EvaluationNotebook[]]<>old<>".nb"];
];

CodeVisible[] := Module[{cells,show},
   cells = Cells[EvaluationNotebook[], CellStyle -> "Input"];
show=Last@First@Options[EvaluationNotebook[],ShowCellTags];

SetOptions[EvaluationNotebook[],ShowCellLabel->!show];
SetOptions[EvaluationNotebook[],ShowCellTags->!show];
   Map[SetOptions[#, CellOpen -> !show] &, cells];
   ];

CodeVisible[flag_] := Module[{cells,show},
   cells = Cells[EvaluationNotebook[], CellStyle -> "Input"];
SetOptions[EvaluationNotebook[],ShowCellLabel->flag];
SetOptions[EvaluationNotebook[],ShowCellTags->flag];
   Map[SetOptions[#, CellOpen -> flag] &, cells];
   ];

CreateTOC[typeList_]:=Module[{toc,book,createCell,counter,cell,type,tag,tocreate,sel},
sel=False;
If[SelectedCells[]=={},
NotebookWrite[EvaluationNotebook[],Cell["Temp","Subtitle",CellTags->"TempIndex"]];
sel=True;
];
If[sel==False, Return];
tocreate={};
book=EvaluationNotebook[];
SetOptions[book,ShowPageBreaks->True];
(*helper file for creating cell*)createCell[text_,tag_,level_]:={"",DynamicBox@Refresh[StringJoin[Table["\t",{i,1,level-1}]]<>(NotebookRead[Cells[EvaluationNotebook[],CellTags->tag]][[1]]/.{Cell[t_,___]:>t}),UpdateInterval->1],DynamicBox@CounterBox["Page",tag]};
(*iterate over cells to set tags and write lines to TOC*)Scan[(counter[#]=0)&,typeList];
SelectionMove[book,Next,Cell];
While[(cell=NotebookRead[book])=!={},If[Length[cell]>=2,type=cell[[2]];
If[MemberQ[typeList,type],counter[type]+=1;
tag=type<>ToString[counter[type]];
SetOptions[NotebookSelection[book],CellTags->tag];
SelectionMove[book,All,CellContents];
AppendTo[tocreate, createCell[SelectedCells[],tag,Position[typeList,type][[1,1]]]]];
SelectionMove[book,Next,Cell]];
];
If[sel,
NotebookLocate["Index"];
If[SelectedCells[]!={},
NotebookWrite[EvaluationNotebook[],Cell[BoxData[StyleBox[DynamicBox@GridBox[tocreate,GridBoxAlignment->{"Columns"->{Left,Left,Right}},GridBoxItemSize->{"Columns"->{1,32,-5}}],"Text"]],CellTags->"Index"]];
NotebookLocate["TempIndex"];
NotebookDelete[];,

NotebookLocate["TempIndex"];
NotebookDelete[];
NotebookWrite[EvaluationNotebook[],Cell[BoxData[StyleBox[DynamicBox@GridBox[tocreate,GridBoxAlignment->{"Columns"->{Left,Left,Right}},GridBoxItemSize->{"Columns"->{2 -1,35-2,-5}}],"Text"]],CellTags->"Index"]];
];
];
];

ContentsByCellStyle[style_]:=Block[{cs},cs=Cells[EvaluationNotebook[],CellStyle->style];
If[Length[cs]>0,CellStrip/@NotebookRead[cs],{""}]];

PublishToPDF[copyPath_: "",codeVisible_:False] := Module[{nb, nb2},
ShowStatus["Exprting PDF..."];
  nb = ToFileName[{NotebookDirectory[EvaluationNotebook[]]}, "WindowTitle" /. NotebookInformation[SelectedNotebook[]]];
  CodeVisible[codeVisible];
  Export[ nb <> ".pdf", SelectedNotebook[]];
nb2 = copyPath <> ToString["WindowTitle" /. NotebookInformation[SelectedNotebook[]]] <> ".pdf";
  If[copyPath != "", CopyFile[nb <> ".pdf", nb2]];
  CodeVisible[True];
SystemOpen[nb<>".pdf"];
ShowStatus["PDF exported successfully"];
{nb, nb2};
  ];
  
Options[ExportToTeX]={ExportToPDF->False,EmbedRefrencesBeforeExport->False,WriteTOC->False,BibFile->"",
WriteAuthors->False,WriteDate->False,WriteTitle->True,CustomTeXCommands->"",TeXLanguage->None,TeXLineSpread->"1.0",
ShowTeXLabels->False,SetTeXMargin->"0.8in",TeXPlotScale->"0.7",FitEquations->True};
ExportToTeX[opt:OptionsPattern[]]:=Module[{cells,base,prolog,epilog,styles,title, author, abstract, affil},
ShowStatus["Initializing TeX export..."];
PrintToConsole["ExportToPDF is " <> ToString[OptionValue[ExportToPDF]]];
PrintToConsole["EmbedRefrencesBeforeExport is "<>ToString[OptionValue[EmbedRefrencesBeforeExport]]];
PrintToConsole["WriteTOC is " <> ToString[OptionValue[WriteTOC]]];
PrintToConsole["WriteAuthors is " <> ToString[OptionValue[WriteAuthors]]];
PrintToConsole["WriteDate is " <> ToString[OptionValue[WriteDate]]];
PrintToConsole["WriteTitle is " <> ToString[OptionValue[WriteTitle]]];
PrintToConsole["BibFile is " <> ToString[OptionValue[BibFile]]];
PrintToConsole["CustomTeXCommands is " <> ToString[OptionValue[CustomTeXCommands]]];
PrintToConsole["TeXLanguage is " <> ToString[OptionValue[TeXLanguage]]];
PrintToConsole["ShowTeXLabels is " <> ToString[OptionValue[ShowTeXLabels]]];
PrintToConsole["TeXPlotScale is " <> ToString[OptionValue[TeXPlotScale]]];
PrintToConsole["FitEquations is " <> ToString[OptionValue[FitEquations]]];
Quiet@ExportString["\[Lambda]","TeXFragment"];
System`Convert`TeXFormDump`maketex["\[OAcute]"]="\[OAcute]";
System`Convert`TeXFormDump`maketex["\[CloseCurlyQuote]"]="'";
System`Convert`TeXFormDump`maketex["\:015b"]="\:015b";
System`Convert`TeXFormDump`maketex["\[CAcute]"]="\[CAcute]";
System`Convert`TeXFormDump`maketex["\:0119"]="\:0119";
System`Convert`TeXFormDump`maketex["\:0105"]="\:0105";
System`Convert`TeXFormDump`maketex["\[LSlash]"]="\[LSlash]";
System`Convert`TeXFormDump`maketex["\:0144"]="\:0144";
System`Convert`TeXFormDump`maketex["\:017c"]="\:017c";
System`Convert`TeXFormDump`maketex["\:017a"]="\:017a";
(*System`Convert`TeXFormDump`maketex["&"]="\\$ ";*)
System`Convert`TeXFormDump`maketex["~"]="\\sim ";
System`Convert`TeXFormDump`maketex["\[Perpendicular]"]="\\perp ";
System`Convert`TeXFormDump`maketex["\[TensorWedge]"]="\\wedge ";
System`Convert`TeXFormDump`maketex["\[Wedge]"]="\\wedge ";
System`Convert`TeXFormDump`maketex["\[TensorProduct]"]="\\otimes ";
System`Convert`TeXFormDump`maketex["\[TensorProduct]"]="\\otimes ";
System`Convert`TeXFormDump`maketex["\[LineSeparator]"]="\n";
System`Convert`TeXFormDump`maketex[":="]="\\coloneqq\,";

(*nie zamieniaj zwyk\[LSlash]ego tekstu*)
System`Convert`CommonDump`ConvertTextData[contents_String,toFormat_,toFormatStream_,conversionRules_,opts___?OptionQ]:=Module[{fpre,frule,fpost,pstyle,popts,str=contents},System`Convert`CommonDump`DebugPrint["CONVERTCOMMON:ConvertTextData-general content: ",contents];
pstyle=System`Convert`CommonDump`ParentCellStyle/.{opts}/.System`Convert`CommonDump`ParentCellStyle->"";
popts=Flatten[System`Convert`CommonDump`ParentOptions/.List/@{opts}/.System`Convert`CommonDump`ParentOptions->{}];
System`Convert`CommonDump`DebugPrint["pstyle: ",pstyle];
{fpre,frule,fpost}=System`Convert`CommonDump`ConvertFormatRule[pstyle/.conversionRules,False];
System`Convert`CommonDump`DebugPrint["{fpre, frule, fpost}: ","InputForm"[{fpre,frule,fpost}]];
If[frule===Automatic,frule=System`Convert`CommonDump`ConvertText[#1,toFormat,opts]&];
If[!(System`Convert`CommonDump`ShowQuotesQ[pstyle]||TrueQ[System`Convert`CommonDump`ShowQuotes/.Flatten[{opts}]]||TrueQ[ShowStringCharacters/.popts]),str=System`Convert`CommonDump`RemoveQuotes[str]];
System`Convert`CommonDump`DebugPrint["str: ",frule];
(*If[!TrueQ[System`Convert`CommonDump`ConvertText/.Flatten[{opts}]],str=frule[str]];*)
System`Convert`CommonDump`DebugPrint["str: ",str];
System`Convert`CommonDump`DebugPrint["CONVERTCOMMON-ConvertTextData.  Writing the string. HIJACK"];
System`Convert`CommonDump`DebugPrint["------------------------------------------"];
WriteString[toFormatStream,str];];
System`Convert`TeXFormDump`$TeXDelimiterReplacements = System`Convert`TeXFormDump`$TeXDelimiterReplacements /. {"\\left| " | "\\right| " -> "|","\\left\\| " | "\\right\\| " -> "\\| "};
deb=Convert`TeX`BoxesToTeX[""];
System`Convert`TeXFormDump`$TeXDelimiterReplacements = System`Convert`TeXFormDump`$TeXDelimiterReplacements /. {"\\left| " | "\\right| " -> "|","\\left\\| " | "\\right\\| " -> "\\| "};


EmbedEq[what_]:=Replace[Replace[what,{ButtonBox[___,Tooltip->DynamicBox[c__,UpdateInterval->\[Infinity]],___]:>Temp[c]},Infinity],Temp[RowBox[{_,d___}]]:>d,Infinity];
RefEq[what_]:=Replace[
Replace[what,{Cell[BoxData[ButtonBox[___,TaggingRules->{___,"deeptag"->_,"TeXtag"->c_,___},___],___],___]:>("~\\ref{"<>ToString[c]<>"}"), Cell[BoxData[ButtonBox[___,TaggingRules->{___,"citekey"->c_,___},___],___],___]:>("~\\cite{"<>ToString[c]<>"}"), Cell[C_,D___,FormatType->"TraditionalForm",E___,CellTags->t_]:>Cell[C,"InlineCell",D,FormatType->"TraditionalForm",E,CellTags->t],
Cell[C_,D___,FormatType->"TraditionalForm",E___]:>Cell[C,"InlineCell",D,FormatType->"TraditionalForm",E],Cell[C_,D___]:>Cell[C,"InlineCell",D]},Infinity]
,{Cell[C_,CellTags->t_]:>Cell[C,"InlineCell",CellTags->t]},Infinity];

FixFigures[what_]:=Replace[what,{BoxData[GraphicsBox[C___]]:>BoxData[FormBox[GraphicsBox[C],TraditionalForm]],StyleBox[C_String,Background->RGBColor[0.88, 1, 0.88],D___]:>(C)},Infinity];

NameThisFigure[i_]:=Block[{labels}, labels=(CellTags/.Options[i]); 
If[StringQ[labels]||(ListQ[labels]&&Length[labels]==1),ToString[CellTags/.Options[i]],ToString[CellID/.Options[i]]]];

NameAndExport[i_,form_]:=Block[{name(*,boxs,capt*)},
name=NameThisFigure[i];
(*{boxs,capt}=First@Replace[{form},{FormBox[GridBox[{{C___},_,{D___}},___],___]:>{C,D}},Infinity];
capt=FixedPoint[StringReplace[#,{"\\text{"~~Shortest[t__]~~"}"~~Whitespace~~"\\text{"~~Shortest[s__]~~"}":>"\\text{"<>t<> " "<>s<>" }"}]&,EqBoxToTeX[capt]];
capt=If[StringQ[capt],If[capt==="\\square","",StringJoin["\\caption{\((",capt,"\))}"]],""];
*)
StringJoin["{",FileNameTake[Export[NotebookDirectory[EvaluationNotebook[]]<>name<>".pdf",StyleBox[FormBox[form,TraditionalForm],Magnification->5/8],"PDF",ImageSize->Scaled[1.0],Magnification->5/8]],"}"]
];
CaptionTable[i_,form_]:=Block[{boxs,capt},
{capt,boxs}=First@Replace[{form},{FormBox[GridBox[{_,{C___},{D___}},___],___]:>{C,D}},Infinity];
capt=FixedPoint[StringReplace[#,{"\\text{"~~Shortest[t__]~~"}"~~Whitespace~~"\\text{"~~Shortest[s__]~~"}":>"\\text{"<>t<> " "<>s<>" }"}]&,EqBoxToTeX[capt]];
capt=If[StringQ[capt],If[capt==="\\square","",StringJoin["\\caption{\((",capt,"\))}\n"]],""];
StringReplace[StringJoin[capt,EqBoxToTeX@FormBox[boxs,TraditionalForm]],"{array}"->"{tabular}"]
];
SL[x_]:=If[StringQ[x],x,First@x];
CheckLabels[labels_,sublabels_:None,inlineQ_:False]:=Block[{hasLabels,hasSublabels},
hasLabels=(StringQ[labels]||(ListQ[labels]&&Length[labels]>0));
hasSublabels=(StringQ[sublabels]||(ListQ[sublabels]&&Length[sublabels]>0));
If[hasLabels && Length[labels]>1,
PrintToConsole["One of the cells has more than one label "<>ToString[labels]<>" using first"];];
If[hasSublabels && Length[sublabels]>1,
PrintToConsole["One of the cells has more than one label "<>ToString[sublabels]<>" using first"];];

If[hasLabels && hasSublabels,"\\label{"<>SL@labels<>SL@sublabels<>"}",
If[hasLabels &&!inlineQ,"\\label{"<>SL@labels<>"}",If[hasSublabels,"\\label{"<>SL@sublabels<>"}",""]]]
];
LeftB[i_]:=If[(StringQ[#]||(ListQ[#]&&Length[#]>0))& @(CellTags/.Options[i]),"{",""];
RightB[i_]:=If[(StringQ[#]||(ListQ[#]&&Length[#]>0))& @(CellTags/.Options[i]),"}",""];
AddLabel[i_]:=Block[{labels},
labels=(CellTags/.Options[i]);
CheckLabels[labels]];

AddInlineLabels[i_,form_]:=Block[{parentLabels,labels,formattedLabel},
formattedLabel=None;
parentLabels=(CellTags/.Options[i]);
labels=Replace[Cases[NotebookRead[i],Cell[___,_[form],___,CellTags->_,___],Infinity],Cell[___,_[form],___,CellTags->c_,___]:>c,Infinity];
formattedLabel=CheckLabels[parentLabels,labels,True];
(*PrintToConsole[form];
PrintToConsole[TooltipToFootnote[form]];*)
If[StringQ[TooltipToFootnote[form]],TooltipToFootnote[form],
("\\("<>(StringTrim@EqBoxToTeX[form])<>formattedLabel<>"\\)")]];
(*use defaults*)
myBoxRule[TemplateBox[boxes_,rule_]]:=System`Convert`TeXFormDump`maketex[TemplateBox[boxes,rule]];
myBoxRule[TemplateBox[boxes_,rule_,ruleb_]]:=System`Convert`TeXFormDump`maketex[TemplateBox[boxes,rule,ruleb]];

myBoxRule[TemplateBox[{boxes_,_,lima_,limb_},___]]:=StringJoin["\\left."<>System`Convert`TeXFormDump`maketex[boxes],"\\right|_{",System`Convert`TeXFormDump`maketex[lima],"}^{",System`Convert`TeXFormDump`maketex[limb],"}"];

myBoxRule[FormBox[TemplateBox[{boxes_,_,lima_,limb_},___],___]]:=StringJoin["\\left."<>System`Convert`TeXFormDump`maketex[boxes],"\\right|_{",System`Convert`TeXFormDump`maketex[lima],"}^{",System`Convert`TeXFormDump`maketex[limb],"}"];

myBoxRule[FormBox[C__]]:=(System`Convert`TeXFormDump`maketex[FormBox[C]]);
TooltipToFootnote[a_]:=First@Replace[{a},{FormBox[TagBox[TooltipBox[C:(_String|_Symbol),D:(_String|_Symbol),___],___],___]:>(ToString[C]<>"\\footnote{"<>If[StringQ[D],StringDrop[StringDrop[D,1],-1], ToString[D]]<>"}")},Infinity];
EqBoxToTeX[c_]:=(Convert`TeX`BoxesToTeX[c,"BoxRules"->{box:(_TemplateBox|_FormBox):>(myBoxRule[box])}]);
title=ContentsByCellStyle["Title"];
author=ContentsByCellStyle["Author"];
abstract=ContentsByCellStyle["Abstract"];
affil=ContentsByCellStyle["Affiliation"];
prolog="% \\documentclass[titlepage, a4paper]{mwart}
%\\documentclass[aps,prl,showpacs,10pt,superscriptaddress,nidanfloat,twocolumn,% draft]{revtex4-1}
\\documentclass{article}
\\usepackage[margin="<>OptionValue[SetTeXMargin]<>"]{geometry}
\\usepackage{amsmath,amsfonts,amssymb,amsthm}
\\usepackage{graphicx, setspace}
\\usepackage{authblk}
\\usepackage{mathtools}
\\usepackage[export]{adjustbox}
\\usepackage{placeins}
\\let\\Oldsection\\section
\\renewcommand{\\section}{\\FloatBarrier\\Oldsection}
\\let\\Oldsubsection\\subsection
\\renewcommand{\\subsection}{\\FloatBarrier\\Oldsubsection}
\\let\\Oldsubsubsection\\subsubsection
\\renewcommand{\\subsubsection}{\\FloatBarrier\\Oldsubsubsection}

\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage[english,polish]{babel}
\\usepackage{breqn} %break long equations, use dmath instead of equation
\\newtheorem{theorem}{"<>TranslateSpecialCellStyleNames["Theorem"]<>"}[section]
\\newtheorem{example}{"<>TranslateSpecialCellStyleNames["Example"]<>"}[section]
\\newtheorem{exercise}{"<>TranslateSpecialCellStyleNames["Exercise"]<>"}[section]
\\newtheorem{solution}[exercise]{"<>TranslateSpecialCellStyleNames["Solution"]<>"}
\\newtheorem{corollary}{"<>TranslateSpecialCellStyleNames["Corollary"]<>"}[theorem]
\\newtheorem{lemma}[theorem]{"<>TranslateSpecialCellStyleNames["Lemma"]<>"}
\\newtheorem{definition}{"<>TranslateSpecialCellStyleNames["Definition"]<>"}[section]
\\newtheorem{axiom}{"<>TranslateSpecialCellStyleNames["Axiom"]<>"}[section]
\\newtheorem*{remark}{"<>TranslateSpecialCellStyleNames["Remark"]<>"}
\\newtheorem*{comment}{"<>TranslateSpecialCellStyleNames["Comment"]<>"}
\\newtheorem{question}{"<>TranslateSpecialCellStyleNames["Question"]<>"}[section]
% \\usepackage{polski}
\\linespread{"<>ToString[OptionValue[TeXLineSpread]]<>"}
\\usepackage[pdftex,colorlinks=true,citecolor=blue,linkcolor=magenta]{hyperref}\n"
<>If[OptionValue[ShowTeXLabels],"\\usepackage{showlabels} %Comment this in production",""]<>
"\\usepackage[backend=bibtex,sorting=none]{biblatex}
\\bibliography{"<>OptionValue[BibFile]<>"}\n"<>
OptionValue[CustomTeXCommands]<>"
% \\usepackage[backend=bibtex]{biblatex}
% \\addbibresource[location=remote]{http://127.0.0.1:23119/better-bibtex/collection?/1/.biblatex}
% \\makeatletter
% \\newcommand{\\labeltext}[3][]{%
%     \\@bsphack%
%     \\csname phantomsection\\endcsname% in case hyperref is used
%     \\def\\tst{#1}%
%     \\def\\labelmarkup{\\emph}% How to markup the label itself
% %\\def\\refmarkup{\\labelmarkup}% How to markup the reference
%     \\def\\refmarkup{}%
%     \\ifx\\tst\\empty\\def\\@currentlabel{\\refmarkup{#2}}{\\label{#3}}%
%     \\else\\def\\@currentlabel{\\refmarkup{#1}}{\\label{#3}}\\fi%
%     \\@esphack%
%     \\labelmarkup{#2}% visible printed text.
% }
% \\makeatother
\\title{"<>First[title]<>"}\n"<>
If[OptionValue[WriteAuthors],
StringRiffle[MapIndexed["\\author["<>ToString[First[#2]]<>"]{"<>#1<>"}"&,author],"\n"]<>
StringRiffle[MapIndexed["\\affil["<>ToString[First[#2]]<>"]{"<>#1<>"}\n"&,affil],"\n"]<>
"\n\\renewcommand\\Affilfont{\\itshape\\small}",""]<>
If[OptionValue[WriteDate],"\\date{}",""]<>
"
\\begin{document}
\\selectlanguage{"<>ToLowerCase[If[OptionValue[TeXLanguage]==None,AbsoluteCurrentValue["Language"],OptionValue[TeXLanguage]]]<>"}\n"
<>If[OptionValue[WriteTitle],"\\maketitle\n","\n"]<>
If[OptionValue[WriteTOC],"\\tableofcontents{}",""]
<>"\n";
epilog="\\appto{\\bibsetup}{\\raggedright} %For keeping the bib within margins
\\printbibliography
\\end{document}";
ShowStatus["Gathering cells..."];
(*styles=DeleteCases[FEPrivate`GetPopupList[EvaluationNotebook[],"MenuListStyles"]//FE`Evaluate//Cases[_[s_String,_]:>s],"Input"|"Output"];*)
cells=Cells[EvaluationNotebook[]];
cells=Select[cells,!MemberQ[{{"Input"},{"Print"},{"Output"},{"Title"},{"Affiliation"},{"Author"},{"Message"},{"Message","MSG"}},CurrentValue[#,"CellStyle"]]&];
ShowStatus["Processing cells..."];
(*line splitter extended to 5000 characters*)
System`Convert`TeXDump`cleanUpFile[fileName_String] := 
 Module[{streamIn, streamOut, insideComment = False, charNum = 0, 
   scratchFileName, charIn, charLast = ""}, 
   	 {streamIn, streamOut} = {OpenRead[fileName], System`ConvertersDump`Utilities`OpenTempFile[CharacterEncoding -> {}]};
  	While[(charIn = Read[streamIn, Character]) =!= EndOfFile, 
   If[charLast =!= "\\" && charIn === "%", insideComment = True];
   	If[charIn === "\n", insideComment = False;
    	charNum = -1];
   	If[charNum >= 5000 && Or @@ (charIn === #1 &) /@ {"\t", " "}, 
    WriteString[streamOut, If[! insideComment, "\n", "\n%"]];
    	charNum = 0, WriteString[streamOut, charIn];
    	++charNum];
   	charLast = charIn;];
  	Scan[Close, {streamIn, streamOut}];
  	DeleteFile[fileName];
  	CopyFile[scratchFileName = First[streamOut], fileName];
  	DeleteFile[scratchFileName];];
  	Quiet@ExportString["\[Lambda]","TeXFragment"];
  	
base=StringJoin@Riffle[#,"\n"]&@Table[ImportString[ExportString[If[OptionValue[EmbedRefrencesBeforeExport],EmbedEq,RefEq]@FixFigures[NotebookRead[i]],"TeXFragment",
"BoxRules"->{box:(_FormBox):>(myBoxRule[box]),
"\[Transpose]":>"^{\\mathsf{T}}",
"\[ConjugateTranspose]":>"^{\\dagger} ",
"\[HermitianConjugate]":>"^{\\dagger} "},
"ConversionRules"->{
"Text"->{LeftB[i],Automatic,StringJoin[AddLabel[i],RightB[i]]},
"Chapter"->{"\\part{",Automatic,AddLabel[i]<>"}"},
"Section"->{"\\section{",Automatic,AddLabel[i]<>"}"},
"Subsection"->{"\\subsection{",Automatic,AddLabel[i]<>"}"},
"Subsubsection"->{"\\subsubsection{",Automatic,AddLabel[i]<>"}"},
"Subsubsubsection"->{"\\paragraph{",Automatic,AddLabel[i]<>"}"},
"Subsubsubsubsection"->{"\\subparagraph{",Automatic,AddLabel[i]<>"}"},
"Example"->{"\\begin{example}",Automatic,AddLabel[i]<>"\\end{example}"},
"Exercise"->{"\\begin{exercise}",Automatic,AddLabel[i]<>"\\end{exercise}"},
"Solution"->{"\\begin{solution}",Automatic,AddLabel[i]<>"\\end{solution}"},
"Question"->{"\\begin{question}",Automatic,AddLabel[i]<>"\\end{question}"},
"Remark"->{"\\begin{remark}",Automatic,AddLabel[i]<>"\\end{remark}"},
"Comment"->{"\\begin{comment}",Automatic,AddLabel[i]<>"\\end{comment}"},
"Theorem"->{"\\begin{theorem}",Automatic,AddLabel[i]<>"\\end{theorem}"},
"Proof"->{"\\begin{proof}",Automatic,AddLabel[i]<>"\\end{proof}"},
"Axiom"->{"\\begin{axiom}",Automatic,AddLabel[i]<>"\\end{axiom}"},
"Definition"->{"\\begin{definition}",Automatic,AddLabel[i]<>"\\end{definition}"},
"Lemma"->{"\\begin{lemma}",Automatic,AddLabel[i]<>"\\end{lemma}"},
"Corollary"->{"\\begin{corollary}",Automatic,AddLabel[i]<>"\\end{corollary}"},
"Abstract"->{"\\begin{abstract}",Automatic,"\\end{abstract}"},
"Figure"->{"\\begin{figure}[!htb]\\centering\\includegraphics[scale="<>OptionValue[TeXPlotScale]<>",max width=\\textwidth]",NameAndExport[i,#]&,AddLabel[i]<>"\\end{figure}"},
"FigureCaption"->{"\\caption{",Automatic,"}"},
"Board"->{"\\begin{table}[ht!]\\centering",CaptionTable[i,#]&,AddLabel[i]<>"\\end{table}"},
"EquationNumbered"->{"\\begin{equation}"<>If[OptionValue[FitEquations],"\\adjustbox{max width=.95\\textwidth}{$",""],EqBoxToTeX[#]&,If[OptionValue[FitEquations],"$}",""]<>AddLabel[i]<>"\\end{equation}"},"EquationNumbered"->{"\\begin{equation}",EqBoxToTeX[#]&,AddLabel[i]<>"\\end{equation}"},
"Equation"->{"\\begin{equation*}"<>If[OptionValue[FitEquations],"\\adjustbox{max width=.95\\textwidth}{$",""],EqBoxToTeX[#]&,If[OptionValue[FitEquations],"$}",""]<>AddLabel[i]<>"\\end{equation*}"},
"InlineCell"->{"",AddInlineLabels[i,#]&,""}
}
],"Text"],{i,cells}];
(*line splitter restored to 144 characters*)
System`Convert`TeXDump`cleanUpFile[fileName_String] := 
 Module[{streamIn, streamOut, insideComment = False, charNum = 0, 
   scratchFileName, charIn, charLast = ""}, 
   	 {streamIn, streamOut} = {OpenRead[fileName], System`ConvertersDump`Utilities`OpenTempFile[CharacterEncoding -> {}]};
  	While[(charIn = Read[streamIn, Character]) =!= EndOfFile, 
   If[charLast =!= "\\" && charIn === "%", insideComment = True];
   	If[charIn === "\n", insideComment = False;
    	charNum = -1];
   	If[charNum >= 5000 && Or @@ (charIn === #1 &) /@ {"\t", " "}, 
    WriteString[streamOut, If[! insideComment, "\n", "\n%"]];
    	charNum = 0, WriteString[streamOut, charIn];
    	++charNum];
   	charLast = charIn;];
  	Scan[Close, {streamIn, streamOut}];
  	DeleteFile[fileName];
  	CopyFile[scratchFileName = First[streamOut], fileName];
  	DeleteFile[scratchFileName];];
ShowStatus["Postprocessing cells..."];
(*fix figure captions*)

base=StringReplace[base,"\\label{"~~Shortest[t___]~~"}\\end{figure}"~~Whitespace~~"\\caption{"~~Shortest[c___]~~"}"/; (StringCount[c,"\\("]==StringCount[c,"\\)"]&&StringCount[c,"{"]==StringCount[c,"}"]&&StringFreeQ[t,"}"](*&&(!StringContainsQ[c,"\\ref"]||((StringCount[c,"{"]==StringCount[c,"\\ref"])&&(StringCount[c,"}"]==StringCount[c,"\\ref"])))*)(*&&StringFreeQ[t,"}"]*)) :>"\\caption{ "<>c<>" }\\label{"<>t<>"}\\end{figure} "];
base=StringReplace[base,{"\\end{equation*}"~~Whitespace~~"\\begin{equation*}"->"\\end{equation*}\n\\begin{equation*}","\\end{equation}"~~Whitespace~~"\\begin{equation}"->"\\end{equation}\n\\begin{equation}","\\end{equation}"~~Whitespace~~"\\begin{equation*}"->"\\end{equation}\n\\begin{equation*}"}];
(*remove spacing for glueing TeX ref*)
base=StringReplace[base,{Whitespace~~"~\\"->"~\\"}];
base=StringReplace[base,{"\\(\\("->"\\(","\\)\\)"->"\\)"}];
base=StringReplace[StringReplace[base,{"\\)"~~Shortest[C__]~~"\\(":>StringJoin["\\)",StringReplace[C,"\\pmb{"->"\\textbf{"],"\\("],
StartOfString~~Shortest[C__]~~"\\(":>StringJoin[StringReplace[C,"\\pmb{"->"\\textbf{"],"\\("],
"\\)"~~Shortest[C__]~~EndOfString:>StringJoin["\\)",StringReplace[C,"\\pmb{"->"\\textbf{"]]
}],{"\\pmb{"->"\\bf{"}];
base=If[StringContainsQ[base,"\\("],base,StringReplace[base,{"\\pmb{"->"\\textbf{"}]];
(*make inline matrixes small*)
base=StringReplace[base,"\\("~~Shortest[C__]~~"\\)":>("\\("<>StringReplace[C,{"{array}{c"~~Repeated["c"]..~~"}":>"{smallmatrix}","{array}":>"{smallmatrix}"}]<>"\\)")];
(*base=StringReplace[base,{"\\&"\[Rule]"&","{array}" -> "{aligned}"}];*)
ShowStatus["Exporting TeX file..."];
Export[StringDrop[NotebookFileName[],-2]<>"tex",prolog<>base<>epilog,"Text"];
ShowStatus["Trying to build the PDF from TeX (might fail)..."];

If[OptionValue[ExportToPDF],
PrintToConsole["Running command: "<>"!cd "<>NotebookDirectory[EvaluationNotebook[]]<>"; ls; pdflatex "<>StringDrop[FileNameTake[NotebookFileName[EvaluationNotebook[]]],-2]<>"tex"];
(*SetEnvironment["PATH"\[Rule]Import["!source ~/.bash_profile; echo $PATH","Text"]];*)
Quiet@ReadList@OpenRead["!cd "<>NotebookDirectory[EvaluationNotebook[]]<>"; /Library/TeX/texbin/pdflatex "<>StringDrop[FileNameTake[NotebookFileName[EvaluationNotebook[]]],-2]<>"tex"];
(*RunProcess@{"pdflatex.exe",StringDrop[FileNameTake[NotebookFileName[EvaluationNotebook[]]],-2]<>"tex"}*)];
ShowStatus["Done."];
];



End[];
EndPackage[];
