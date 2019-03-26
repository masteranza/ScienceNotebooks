{
"translatorID":"04623cf0-313c-11df-9aae-0800200c1398",
"translatorType":2,
"label":"ZotSelect Mathematica",
"creator":"Michal Mandrysz",
"target":"html",
"minVersion":"2.0",
"maxVersion":"",
"priority":200,
"inRepository":false,
"displayOptions":{"exportCharset":"UTF-8"},
"lastUpdated":"2019-03-25 20:00:00"
}

function doExport() {
	var item;
	while(item = Zotero.nextItem()) 
	{
		var library_id = item.libraryID ? item.libraryID : 0;
		Zotero.write("Button[Row[{Style[\"\\[DoubleStruckZ] \", 30, Red], \""+item.creators[0].lastName+". ("+item.date+") " +item["title"] +"\"}], SystemOpen[\"zotero://select/items/"+ library_id+"_"+item.key+"\"], BaseStyle->\"GenericButton\"]");
	}
}
