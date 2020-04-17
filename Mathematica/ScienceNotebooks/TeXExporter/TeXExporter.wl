(* ::Package:: *)

(* ::Title:: *)
(*TeXExporter*)


(* ::Author:: *)
(*author: Michal Mandrysz*)


(* ::Affiliation:: *)
(*Marian Smoluchowski Institute of Physics, Jagiellonian University, Krakow, Poland*)


(* ::Abstract:: *)
(*Export Mathematica notebooks to TeX files.*)


BeginPackage["TeXExporter`",{"ScienceNotebooks`"}];


TeXExportToPDF::usage="Option for ExportToTeX";
TeXNumberedSections::usage="Option for ExportToTeX";
TeXEmbedRefrencesBeforeExport::usage="Option for ExportToTeX";
TeXWriteTOC::usage="Option for ExportToTeX";
TeXBibFile::usage="Option for ExportToTeX";
TeXWriteAuthors::usage="Option for ExportToTeX";
TeXWriteAffil::usage="Option for ExportToTeX";
TeXWriteDate::usage="Option for ExportToTeX";
TeXWriteTitle::usage="Option for ExportToTeX";
TeXSetMargin::usage="Option for ExportToTeX";
TeXCustomCommands::usage="Option for ExportToTeX";
TeXLanguage::usage="Option for ExportToTeX";
TeXShowLabels::usage="Option for ExportToTeX";
TeXLineSpread::usage="Option for ExportToTeX";
TeXPlotScale::usage="Option for ExportToTeX";
TeXExportSections::usage="Export only one section";
TeXOmmitStyles::usage="Export only one section";
TeXFitEquations::usage="Option for ExportToTeX";
TeXAlignEquations::usage="Option for ExportToTeX";
TeXOutputName::usage="Option for ExportToTeX";
ExportToTeX::usage="Generates a draft in Tex with an option to compile to PDF";


Begin["`Private`"];


Options[ExportToTeX]={TeXExportToPDF->False,TeXNumberedSections->True,TeXEmbedRefrencesBeforeExport->False,TeXWriteTOC->False,TeXBibFile->"",
TeXWriteAuthors->False,TeXWriteAffil->False,TeXWriteDate->False,TeXWriteTitle->True,TeXCustomCommands->"",TeXLanguage->None,TeXLineSpread->"1.0",
TeXShowLabels->False,TeXSetMargin->"0.8in",TeXPlotScale->"0.7",TeXExportSections->All,TeXOmmitStyles->{},TeXFitEquations->False,TeXAlignEquations->True,TeXOutputName->""};
ExportToTeX[opt:OptionsPattern[]]:=Module[{cells,cells2,base,prolog,epilog,styles,title, author, abstract, affil,tmps,ommit,outname},
ShowStatus["Initializing TeX export..."];
If[NotebookDirectory[]===$Failed,ShowStatus["ExportToTeX failed. Notebook must be saved first!"];Abort[]];
PrintToConsole["TeXExportToPDF is " <> ToString[OptionValue[TeXExportToPDF]]];
PrintToConsole["TeXNumberedSections is" <> ToString[OptionValue[TeXNumberedSections]]];
PrintToConsole["TeXEmbedRefrencesBeforeExport is "<>ToString[OptionValue[TeXEmbedRefrencesBeforeExport]]];
PrintToConsole["TeXWriteTOC is " <> ToString[OptionValue[TeXWriteTOC]]];
PrintToConsole["TeXWriteAuthors is " <> ToString[OptionValue[TeXWriteAuthors]]];
PrintToConsole["TeXWriteAffil is " <> ToString[OptionValue[TeXWriteAffil]]];
PrintToConsole["TeXWriteDate is " <> ToString[OptionValue[TeXWriteDate]]];
PrintToConsole["TeXWriteTitle is " <> ToString[OptionValue[TeXWriteTitle]]];
PrintToConsole["TeXBibFile is " <> ToString[OptionValue[TeXBibFile]]];
PrintToConsole["TeXCustomCommands is " <> ToString[OptionValue[TeXCustomCommands]]];
PrintToConsole["TeXLanguage is " <> ToString[OptionValue[TeXLanguage]]];
PrintToConsole["TeXShowLabels is " <> ToString[OptionValue[TeXShowLabels]]];
PrintToConsole["TeXPlotScale is " <> ToString[OptionValue[TeXPlotScale]]];
PrintToConsole["TeXExportSections is " <> ToString[OptionValue[TeXExportSections]]];
PrintToConsole["TeXOmmitStyles is " <> ToString[OptionValue[TeXOmmitStyles]]];
PrintToConsole["TeXLineSpread is " <> ToString[OptionValue[TeXLineSpread]]];
PrintToConsole["TeXFitEquations is " <> ToString[OptionValue[TeXFitEquations]]];
PrintToConsole["TeXAlignEquations is " <> ToString[OptionValue[TeXAlignEquations]]];
PrintToConsole["TeXOutputName is " <> ToString[OptionValue[TeXOutputName]]];
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
System`Convert`TeXFormDump`maketex["\[GreaterTilde]"]="\\gtrsim ";
System`Convert`TeXFormDump`maketex["c.c."]="\text{c.c.}";
System`Convert`TeXFormDump`maketex["c.\[VeryThinSpace]c."]="\text{c.c.}";
System`Convert`TeXFormDump`maketex["\[LineSeparator]"]="\n";
System`Convert`TeXFormDump`maketex[":="]="\\coloneqq\,";

(*don't change regular text [hack]*)
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
(*prevent double brackets*)
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
EqBoxToTeX[c_]:=Convert`TeX`BoxesToTeX[c,"BoxRules"->{box:(_TemplateBox|_FormBox):>(myBoxRule[box])}];
EqBoxToTeXDisplay[c_]:=(EqBoxToTeX[c]<>"\\\\");
title=ContentsByCellStyle["Title"];
author=ContentsByCellStyle["Author"];
abstract=ContentsByCellStyle["Abstract"];
affil=ContentsByCellStyle["Affiliation"];
prolog="% \\documentclass[titlepage, a4paper]{mwart}
%\\documentclass[aps,prl,showpacs,10pt,superscriptaddress,nidanfloat,twocolumn,% draft]{revtex4-1}
\\documentclass{article}
\\usepackage[margin="<>OptionValue[TeXSetMargin]<>"]{geometry}
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
<>If[OptionValue[TeXShowLabels],"\\usepackage{showlabels} %Comment this in production",""]<>
"\\usepackage[backend=bibtex,sorting=none]{biblatex}
\\bibliography{"<>OptionValue[TeXBibFile]<>"}\n"<>
OptionValue[TeXCustomCommands]<>"
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
If[OptionValue[TeXWriteAuthors],
StringRiffle[MapIndexed["\\author["<>ToString[First[#2]]<>"]{"<>#1<>"}"&,author],"\n"]<>
If[OptionValue[TeXWriteAffil],StringRiffle[MapIndexed["\\affil["<>ToString[First[#2]]<>"]{"<>#1<>"}\n"&,affil],"\n"],""]<>
"\n\\renewcommand\\Affilfont{\\itshape\\small}",""]<>
If[OptionValue[TeXWriteDate],"","\\date{\\vspace{-5ex}}"]<>
"
\\begin{document}
\\selectlanguage{"<>ToLowerCase[If[OptionValue[TeXLanguage]==None,AbsoluteCurrentValue["Language"],OptionValue[TeXLanguage]]]<>"}\n"
<>If[OptionValue[TeXWriteTitle],"\\maketitle\n","\n"]<>
If[OptionValue[TeXWriteTOC],"\\tableofcontents{}",""]
<>"\n";
epilog="\\appto{\\bibsetup}{\\raggedright} %For keeping the bib within margins
\\printbibliography
\\end{document}";
ShowStatus["Gathering cells..."];
(*styles=DeleteCases[FEPrivate`GetPopupList[EvaluationNotebook[],"MenuListStyles"]//FE`Evaluate//Cases[_[s_String,_]:>s],"Input"|"Output"];*)
cells=Cells[EvaluationNotebook[]];
ommit=Join[{{"Input"},{"Print"},{"Output"},{"Title"},{"Affiliation"},{"Author"},{"Message"},{"Message","MSG"}},Partition[OptionValue[TeXOmmitStyles],1]];
cells=Select[cells,!MemberQ[ommit,CurrentValue[#,"CellStyle"]]&];
ShowStatus["Processing cells..."];
(*PrintToConsole[cells];*)
cells=If[OptionValue[TeXExportSections]=!=All,
cells2=Split[cells,(*(CurrentValue[#2,"CellStyle"]=!={"Section"})||*)(CurrentValue[#2,"CellStyle"]=!={"Section"})&];
If[Length[cells2]<Max[OptionValue[TeXExportSections]],ShowStatus["Error: Incorrect TeXExportSections option value - there's not that many sections in your notebook"];  Abort[];]; 
cells2=If[CurrentValue[cells2[[1,1]],"CellStyle"]=!={"Section"},cells2[[2;;]],cells2];
Flatten[cells2[[OptionValue[TeXExportSections]]],1],cells];
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
  	
base=StringJoin@Riffle[#,"\n"]&@Table[ImportString[ExportString[If[OptionValue[TeXEmbedRefrencesBeforeExport],EmbedEq,RefEq]@FixFigures[NotebookRead[i]],"TeXFragment",
"BoxRules"->{box:(_FormBox):>(myBoxRule[box]),
"\[Transpose]":>"^{\\mathsf{T}}",
"\[ConjugateTranspose]":>"^{\\dagger} ",
"\[HermitianConjugate]":>"^{\\dagger} "},
"ConversionRules"->{
"Text"->{LeftB[i],Automatic,StringJoin[AddLabel[i],RightB[i]]},
"Chapter"->{"\\part{",Automatic,AddLabel[i]<>"}"},
"Section"->{"\\section"<>If[OptionValue[TeXNumberedSections],"","*"]<>"{",Automatic,AddLabel[i]<>"}"},
"Subsection"->{"\\subsection"<>If[OptionValue[TeXNumberedSections],"","*"]<>"{",Automatic,AddLabel[i]<>"}"},
"Subsubsection"->{"\\subsubsection"<>If[OptionValue[TeXNumberedSections],"","*"]<>"{",Automatic,AddLabel[i]<>"}"},
"Subsubsubsection"->{"\\paragraph"<>If[OptionValue[TeXNumberedSections],"","*"]<>"{",Automatic,AddLabel[i]<>"}"},
"Subsubsubsubsection"->{"\\subparagraph"<>If[OptionValue[TeXNumberedSections],"","*"]<>"{",Automatic,AddLabel[i]<>"}"},
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
"EquationNumbered"->{"\\begin{equation}"<>If[OptionValue[TeXFitEquations],"\\adjustbox{max width=.95\\textwidth}{$",""]<>If[OptionValue[TeXAlignEquations],"\\begin{aligned}",""],EqBoxToTeXDisplay[#]&,"\n"<>If[OptionValue[TeXAlignEquations],"\\end{aligned}",""]<>If[OptionValue[TeXFitEquations],"$}",""]<>AddLabel[i]<>"\\end{equation}"},
"Equation"->{"\\begin{equation*}"<>If[OptionValue[TeXFitEquations],"\\adjustbox{max width=.95\\textwidth}{$",""]<>If[OptionValue[TeXAlignEquations],"\\begin{aligned}",""],EqBoxToTeXDisplay[#]&,If[OptionValue[TeXAlignEquations],"\\end{aligned}",""]<>If[OptionValue[TeXFitEquations],"$}",""]<>AddLabel[i]<>"\\end{equation*}"},
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
(*alignment*)
(*PutAlignMarkOnEqual[st_]:=(StringReplace[st,StartOfString ~~Shortest[t___]~~"="~~u___~~EndOfString/; (StringCount[t,"{"]==StringCount[t,"}"]):> t<>" &="<>u]);
PutAlignMarkOnPropto[st_]:=StringReplace[st,StartOfString ~~Shortest[t___]~~"\\propto"~~u___~~EndOfString/; (StringCount[t,"{"]==StringCount[t,"}"]):> t<>" &\\propto"<>u];
PutAlignMarkOnApprox[st_]:=StringReplace[st,StartOfString ~~Shortest[t___]~~"\\approx"~~u___~~EndOfString/; (StringCount[t,"{"]==StringCount[t,"}"]):> t<>" &\\approx"<>u];
PutAlignMarkOnPlusMinus[st_]:=StringReplace[st,StartOfString ~~Shortest[t___]~~Pattern[z,Blank[]]~~u___~~EndOfString  /; ((z==="+"||z==="-")&&StringCount[t,"{"]==StringCount[t,"}"]) :> t<>" &"<>z<>u];
PutAlignMark[st_String]:=If[Length[StringSplit[st,"\\\\"]]>1,
(StringRiffle[(tmps=If[StringContainsQ[#,"="],PutAlignMarkOnEqual[#],
If[StringContainsQ[#,"\\propto"],PutAlignMarkOnPropto[#],If[StringContainsQ[#,"\\approx"],PutAlignMarkOnApprox[#], PutAlignMarkOnPlusMinus[#]]]
]; If[StringContainsQ[tmps,"&"],tmps," &"<>tmps]) &/@StringSplit[st,"\\\\"],"\\\\\n"]),st];*)
PutAlignMarkSpecial[st_]:=(StringReplace[st,StartOfString ~~Shortest[t___]~~a:("="|"\\approx"| "\\propto"(*|"+"|"-"*))~~u___~~EndOfString/; (StringCount[t,"{"]==StringCount[t,"}"]&&StringCount[t,"("]==StringCount[t,")"]):> t<>" &"<>a<>u]);
(*PutAlignMarkOnPropto[st_]:=StringReplace[st,StartOfString ~~Shortest[t___]~~"\\propto"~~u___~~EndOfString/; (StringCount[t,"{"]==StringCount[t,"}"]):> t<>" &\\propto"<>u];
PutAlignMarkOnApprox[st_]:=StringReplace[st,StartOfString ~~Shortest[t___]~~"\\approx"~~u___~~EndOfString/; (StringCount[t,"{"]==StringCount[t,"}"]):> t<>" &\\approx"<>u];
PutAlignMarkOnPlusMinus[st_]:=StringReplace[st,StartOfString ~~Shortest[t___]~~Pattern[z,Blank[]]~~u___~~EndOfString  /; ((z==="+"||z==="-")&&StringCount[t,"{"]==StringCount[t,"}"]) :> t<>" &"<>z<>u];*)
PutAlignMark[st_String]:=If[Length[StringSplit[st,"\\\\"]]>1,
(StringRiffle[(tmps=PutAlignMarkSpecial[#]; If[StringContainsQ[tmps,"&"],tmps," &"<>tmps]) &/@StringSplit[st,"\\\\"],"\\\\\n"]),st];
base=StringReplace[base,"\\\\"~~Whitespace~~"\\end{aligned}" ->"\\end{aligned}"];
base=StringReplace[base,"\\begin{aligned}"~~Shortest[t___]~~"\\end{aligned}" /;!StringContainsQ[t,"\\begin{array}"]:>"\\begin{aligned}"<>PutAlignMark[t]<>"\\end{aligned} "];
base=StringReplace[base,"\\begin{aligned}\\begin{array}{l}"~~Shortest[t___]~~"\\end{array}\\end{aligned}" /;!StringContainsQ[t,"\\begin{array}"]:>"\\begin{aligned}"<>PutAlignMark[t]<>"\\end{aligned} "];
(*fix double superscript error connected with primed variables*)
base=StringReplace[base,"'^"~~t_ /;!(StringContainsQ[t,"{"]):>("^{\\prime "<>t<>"}")];
base=StringReplace[base,"'^{"~~Shortest[t___]~~"}" /;StringCount[t,"{"]==StringCount[t,"}"]:>("^{\\prime "<>t<>"}")];
base=StringReplace[base,"''^"~~t_ /;!(StringContainsQ[t,"{"]):>("^{\\prime \\prime "<>t<>"}")];
base=StringReplace[base,"''^{"~~Shortest[t___]~~"}" /;StringCount[t,"{"]==StringCount[t,"}"]:>("^{\\prime \\prime "<>t<>"}")];

base=StringReplace[base,"'"~~Whitespace~~"^"~~t_ /;!(StringContainsQ[t,"{"]):>("^{\\prime "<>t<>"}")];
base=StringReplace[base,"'"~~Whitespace~~"^{"~~Shortest[t___]~~"}" /;StringCount[t,"{"]==StringCount[t,"}"]:>("^{\\prime "<>t<>"}")];
(*cleanup*)
base=StringReplace[base,{"\\end{equation*}"~~Whitespace~~"\\begin{equation*}"->"\\end{equation*}\n\\begin{equation*}","\\end{equation}"~~Whitespace~~"\\begin{equation}"->"\\end{equation}\n\\begin{equation}","\\end{equation}"~~Whitespace~~"\\begin{equation*}"->"\\end{equation}\n\\begin{equation*}"}];
base=StringReplace[base,{"\\end{equation*}"~~Whitespace->"\\end{equation*}","\\end{equation}"~~Whitespace->"\\end{equation}"}];
(*remove spacing for glueing TeX ref*)
base=StringReplace[base,{Whitespace~~"~\\"->"~\\"}];
base=StringReplace[base,{"\\(\\("->"\\(","\\)\\)"->"\\)"}];
(*fix italics and bolds*)
base=StringReplace[StringReplace[base,{"\\)"~~Shortest[C__]~~"\\(":>StringJoin["\\)",StringReplace[C,"\\pmb{"->"\\textbf{"],"\\("],
StartOfString~~Shortest[C__]~~"\\(":>StringJoin[StringReplace[C,"\\pmb{"->"\\textbf{"],"\\("],
"\\)"~~Shortest[C__]~~EndOfString:>StringJoin["\\)",StringReplace[C,"\\pmb{"->"\\textbf{"]]
}],{"\\pmb{"->"\\bf{"}];
base=If[StringContainsQ[base,"\\("],base,StringReplace[base,{"\\pmb{"->"\\textbf{"}]];
(*make inline matrixes small*)
base=StringReplace[base,"\\("~~Shortest[C__]~~"\\)":>("\\("<>StringReplace[C,{"{array}{"~~Repeated["c"]..~~"}":>"{smallmatrix}","{array}":>"{smallmatrix}"}]<>"\\)")];
(*base=StringReplace[base,{"\\&"\[Rule]"&","{array}" -> "{aligned}"}];*)
ShowStatus["Exporting TeX file..."];
outname=If[OptionValue[TeXOutputName]==="",StringDrop[NotebookFileName[],-2]<>"tex",NotebookDirectory[EvaluationNotebook[]]<>OptionValue[TeXOutputName]<>".tex"];
Export[outname,prolog<>base<>epilog,"Text"];
ShowStatus["Trying to build the PDF from TeX (might fail)..."];

If[OptionValue[TeXExportToPDF],
PrintToConsole["Running command: "<>"!cd "<>NotebookDirectory[EvaluationNotebook[]]<>"; ls; pdflatex "<>outname];
(*SetEnvironment["PATH"\[Rule]Import["!source ~/.bash_profile; echo $PATH","Text"]];*)
Quiet@ReadList@OpenRead["!cd "<>NotebookDirectory[EvaluationNotebook[]]<>"; /Library/TeX/texbin/pdflatex "<>outname];
(*RunProcess@{"pdflatex.exe",StringDrop[FileNameTake[NotebookFileName[EvaluationNotebook[]]],-2]<>"tex"}*)];
ShowStatus["Done."];
];



End[];
EndPackage[];
