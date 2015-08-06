#!/bin/bash
# hardlink -u ~/Library/Mathematica/Kernel
# hardlink -u ~/Library/Mathematica/Applications/PackageUtils
# hardlink -u ~/Library/Mathematica/Applications/ScienceReport
# hardlink -u ~/Library/Mathematica/Applications/AnkiExporter
# hardlink -u ~/Library/Mathematica/SystemFiles/FrontEnd/StyleSheets
# hardlink -u ~/Library/Mathematica/SystemFiles/FrontEnd/TextResources

# rm -dr ~/Library/Mathematica/Kernel
# rm -dr ~/Library/Mathematica/SystemFiles/FrontEnd/StyleSheets
# rm -dr ~/Library/Mathematica/SystemFiles/FrontEnd/TextResources

# hardlink ./Kernel ~/Library/Mathematica/Mathematica/Kernel
# hardlink ./PackageUtils ~/Library/Mathematica/Applications/PackageUtils
# hardlink ./ScienceReport ~/Library/Mathematica/Applications/ScienceReport
# hardlink ./AnkiExporter ~/Library/Mathematica/Applications/AnkiExporter
# hardlink ./StyleSheets ~/Library/Mathematica/SystemFiles/FrontEnd/StyleSheets
# hardlink ./TextResources ~/Library/Mathematica/SystemFiles/FrontEnd/TextResources

hardlink ./Anki/runimport ~/Projects/anki/runimport
hardlink ./Anki/knowledgefile.py ~/Projects/anki/anki/importing/knowledgefile.py

hardlink ./Kernel ~/Library/Mathematica/Kernel
hardlink ./Applications/PackageUtils ~/Library/Mathematica/Applications/PackageUtils
hardlink ./Applications/ScienceReport ~/Library/Mathematica/Applications/ScienceReport
hardlink ./Applications/AnkiExporter ~/Library/Mathematica/Applications/AnkiExporter
hardlink ./SystemFiles/FrontEnd/StyleSheets ~/Library/Mathematica/SystemFiles/FrontEnd/StyleSheets
hardlink ./SystemFiles/FrontEnd/TextResources ~/Library/Mathematica/SystemFiles/FrontEnd/TextResources
