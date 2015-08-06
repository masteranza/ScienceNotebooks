#Mathematica Anki

It's a project focusing on collecting knowledge in a systematic and efficient way.
Why create flash cards when you can make your notes and then highlight the stuff that's important thus creating flash cards.

The project consists of Mathematica stylesheet and tools for creating TeX grade notebooks and Anki cards out of them. 

##Installation and prerequisites 
Installation is a bit long, but the process will be improved if enough people will ask for it. 
###Mathematica
The most expensive part of the setup, install a trial if you don't have the full version: https://www.wolfram.com/mathematica/trial/

###Homebrew 
In case you don't have it already:

    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

###Hardlink (optional, but useful)
    brew install hardlink-osx
    
###Anki app
Download and install the standalone: http://ankisrs.net

###Anki source (you need to have both!)
Get it from here: https://github.com/dae/anki/archive/master.zip

Download it and unpack it in "XXX", you'll not be able to run ./runanki straight away, first install and run the following:

    brew install python
    brew install pyqt
    brew install sip
    ./tools/build_ui.sh
   
    #test run
    ./runanki

If anki runs correctly then open your profile and import MathematicaCloze.apkg to make a correct Cloze note type. You should get a temporary _SANDBOX deck - you're free to remove it, but double check if the new default note type is *MathematicaCloze* by trying to add a new card. You should see something like this: 

![https://www.dropbox.com/s/q0dz6pawcbr8sbh/Screenshot%202015-08-06%2018.28.07.png?dl=0](https://photos-4.dropbox.com/t/2/AABcrhtQwOYi6B7Fm0aJAGAOIAuMyFxYuqe5x0tCBXpwCQ/12/2156550/png/32x32/1/_/1/2/Screenshot%202015-08-06%2018.28.07.png/EK3K2gEYj4HXQSABIAIoAQ/ngY0O__tcD9CjM_pcluSRASaonupfXJ02FLDMzCw1yU?size=1600x1200&size_mode=2)

Now, copy (or hardlink) `Anki/runimport` to anki source main directory and `Anki/knowledgefile.py` to /anki/importing/ subdirectory.

**Note**: you can make use of `hardlinker.sh` just check your paths and make sure you don't have your own stuff in Mathematica Stylesheets folder and others.

###Mathematica packages and styles
Go to `~/Library/Mathematica/` and copy (or hardlink) there files from the `Mathematica` directory to corresponding subfolders.

###Root Knowledge folder
You can make it anywhere, but I recommend you putting it inside dropbox folder:

`mkdir ~/Dropbox/Knowledge`
###Important 2 steps in Mathematica

First: Open `~/Library/Mathematica/Applications/AnkiExporter/Exporter.nb` and set appropriate paths:

 - `~/Dropbox/Anki/Ranza/collection.media/` to your Anki database collection (if you didn't move it it's  `~/Documents/Anki/<profile-name>/collection.media/`)
 - `~/Projects/anki/runimport` to your Anki source path

Second: Open Mathematica and open Options Inspector (CMD+O) and search for `CreateCellID`, set the Scope to `Global Preferences` and set it to `True`
Make sure you don't forget about this step - otherwise Export to Anki won't work.

###Useful Mathematica settings (optional)
In Preferences -> System uncheck `Create and maintain version specific front end preferences`
and either set Notebook Security to `Always trust` or add the Knowledge folder to Trusted Directories.

In Preferences -> Messages set Kernel Messages to `Print to Console` it make notebooks more tidy by printing errors and logs to a seperate notebook.

###Symbolic link Anki folder to dropbox (optional)
To keep a backup of your Anki database you can save it on Dropbox

    ln -s ~/Dropbox/Anki/ ~/Documents/Anki



##Test ride

Create a new notebook, go to and set a stylesheet through Format->Stylesheet->Science. You should see a docked cell (kind of a toolbar).
Write a title (CMD+0) saying "Notebook title", move down with down arrow
Write a new section (CMD+1) saying "Section one" , move down
Write a new subsection (CMD+2) saying "Subsection one" , move down
Write a new text (CMD+7) saying "This is a test question with a witty answer"
Select the text "witty answer" and press (CMD+D) the text should get green.
Save the document in `Knowledge/Test/Subcategory/notebook.nb` filename can be different.
Press "->Anki" in the toolbar.
If everything goes right, after seeing "Importing to Anki..." in the status bar, you'll see "Exported 1/1 cells to anki".
Only after exporting is finished you can open Anki. Be sure Anki is closed during exporting.

Here's the final efect:

![enter image description here](https://photos-3.dropbox.com/t/2/AABLckbyDrdeTLKUW72GkD7jRYdvAGDTuLb6Vgrbq4wRlA/12/2156550/png/32x32/1/_/1/2/Screenshot%202015-08-06%2020.56.25.png/EK3K2gEY-YHXQSABIAIoAQ/CaivJARHpaBrotEG9gBrbWkpEsVmPOqkannsmOdMWbg?size=1600x1200&size_mode=2)

To learn some more basics try opening `Using Mathematica-Anki.pdf`

More tutorials coming soon.  (The system is poorly documented, but already has the capabilities to export equations and pictures as it was built out of my own need).

> Written with [StackEdit](https://stackedit.io/).