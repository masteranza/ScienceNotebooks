# Mathematica Anki 2.0


> Thanks to AnkiConnect and recent MathJax support in Anki the installation process has been simplified! ... but is still being debugged, so watchout!


The aim of this project is to collect and review knowledge in a systematic and efficient way.
Why create flash cards when you can have beautiful notebooks with highlighted items which you'd like to remember.
The highlighted items can then be exported to Anki as Cloze deletions with just one click.

![Example use](https://raw.githubusercontent.com/masteranza/MathematicaAnki/master/screen0.png)

The project consists of Mathematica stylesheet and tools for creating TeX grade notebooks and Anki cards out of them. 

# Prerequisites
1. Mathematica - https://www.wolfram.com/mathematica/
2. Anki - http://ankisrs.net
3. AnkiConnect - https://ankiweb.net/shared/info/2055492159

## Installation

1. Import MathematicaCloze.apkg to Anki. It's a special cloze note type which looks like this:

![https://raw.githubusercontent.com/masteranza/MathematicaAnki/master/screen1.png](https://raw.githubusercontent.com/masteranza/MathematicaAnki/master/screen1.png)

2. Go to `~/Library/Mathematica/` and copy (or symlink) files from the `Mathematica` directory to corresponding subfolders. Install the `.otf` fonts on your system manually if you don't have them.

> Warning! This project includes a variety of additions! 1. Custom package called `PackageUtils` (needed), custom stylesheet called `Science` (needed), `StrongFieldUtils`, `ScienceReport` and `PathAutoUpdate` packages (not needed), FrontEnd overloads located in `TextResources` folder and `Kernel` (also not needed). 

3. Create a root `Knowledge` folder. You can make it anywhere, but I recommend you putting it inside dropbox folder:
`mkdir ~/Dropbox/Knowledge`

4. Open Mathematica, Options Inspector (CMD+O) and search for `CreateCellID`, set the Scope to `Global Preferences` and set it to `True`. Make sure you don't forget about this step - otherwise we won't be able to identify Anki notes with Mathematica cells.

### Stuff that makes life easier (optional)

5. In Mathematica, Preferences -> System uncheck `Create and maintain version specific front end preferences`
and either set Notebook Security to `Always trust` or add the Knowledge folder to Trusted Directories.
6. In Mathematica, Preferences -> Messages set Kernel Messages to `Print to Console` it make notebooks more tidy by printing errors and logs to a seperate notebook.


## Test ride

Create a new notebook, go to and set a stylesheet through Format->Stylesheet->Science. You should see a docked cell (kind of a toolbar).

 1. Write a title (CMD+0) saying "Notebook title", move down with down
    arrow.
 2. Write a new section (CMD+1) saying "Section one" , move down
 3. Write a new subsection (CMD+2) saying "Subsection one" , move down
 4. Write a new text (CMD+7) saying "This is a test question with a witty answer"
 5. Select the text "witty answer" and press (CMD+D) the text should get green.
 6. Save the document in `Knowledge/Test/Subcategory/notebook.nb` filename can be different.
 7. Press "->Anki" in the toolbar. If everything goes right, after seeing "Importing to Anki..." in the status bar, you'll see  "Exported 1/1 cells to anki". 

    Only after exporting is finished you can open Anki. Be sure Anki is closed during exporting.

Here's the final efect:

![https://raw.githubusercontent.com/masteranza/MathematicaAnki/master/screen2.png](https://raw.githubusercontent.com/masteranza/MathematicaAnki/master/screen2.png)

To learn some more basics try opening `Using Mathematica-Anki.pdf`

## Currently supported styles

Not all cells get exported to Anki currently supported include:

* Text
* EquationNumbered
* Equation
* Figure
* Item1, Item2, Item3
* Item1Numbered, Item2Numbered, Item3Numbered
* Example, Exercise, Solution
* Question, Remark, Comment, FunFact
* Theorem, Proof, Axiom, Definition, Lemma

More tutorials coming soon.  (The system is poorly documented, but already has the capabilities to export equations and pictures as it was built out of my own need).

## Troubleshooting and notes

Highlighting shortcut apears not to work when caps-lock is enabled.
Someone might think that CMD+SHIFT+D will do the job and try to press it... and then BANG! You've just learned that it doesn't work, but instead splits the selected text into another cell - useful. Serendipity.

## Useful additions

If you'd like to have nice way to type in "post-integration limits" that will be compatible with TeX and Anki run this command once:
`SetOptions[$FrontEnd, 
 InputAliases -> 
  Join[InputAliases /. 
    Options[$FrontEnd, InputAliases], {"at" -> 
     TemplateBox[{"\[SelectionPlaceholder]", "Automatic", 
       "\[Placeholder]", "\[Placeholder]"}, "EvaluatedAt", 
      DisplayFunction -> (SubsuperscriptBox[
          RowBox[{#1, 
            StyleBox["\[VerticalLine]", SpanMinSize -> 1.5, 
             SpanSymmetric -> False]}], 
          RowBox[{"\[MediumSpace]", #3}], 
          RowBox[{"\[MediumSpace]", #4}]] &)]}]]`
