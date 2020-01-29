(* ::Package:: *)

Paclet[
	Name -> "ScienceWrittingUtils",
	Version -> "20.01.22",
	MathematicaVersion -> "10.0+",
	Description -> "Create Anki cards from knowledge gathered in Mathematica notebooks. Highlight with CMD+D, then export all cells containing highlights as cloze cards.",
	Creator -> "Michal Mandrysz <michal.mandrysz@gmail.com",
	URL -> "https://github.com/masteranza/MathematicaAnki",
	Loading -> "Startup",
	Extensions -> {
		{"Documentation"},
		{"Kernel", Root -> ".", Context -> "AnkiExporter`"},
		{"FrontEnd", Prepend -> True}
	}
]
