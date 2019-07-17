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
CodeVisible::usage = "Shows/Hides code and cell tags";PublishToPDF::usage="Saves a publishing ready version, optional argument for copy (pendrive)";
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
IncludePath::usage="Includes a path to $Path variable, for easy loading";


(* ::Section:: *)
(*Implementations*)


Begin["`Private`"];

DuplicateNotebook[]:=NotebookPut@NotebookGet[EvaluationNotebook[]];

CloseCollapsed[]:=(SetOptions[#[[1]],CellOpen->!(CellOpen/.Options[#[[1]],CellOpen])];&/@Select[{#,CurrentValue[#,"CellGroupOpen"]}&/@Cells[EvaluationNotebook[], CellStyle -> "Section"],#[[2]]==$Failed||#[[2]]==Closed&];)

Outline[]:=
pal=CreatePalette[
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


End[];
EndPackage[];
