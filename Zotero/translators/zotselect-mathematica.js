{
"translatorID":"4db53dc0-a3b1-4752-bb39-36193c5237f4",
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

		// var translator = Zotero.loadTranslator("import");
		// translator.setTranslator("a515a220-6fef-45ea-9842-8025dfebcc8f");
		// translator.setString(text);
		
		// Zotero.debug(translator);
		Zotero.debug(item);
		var library_id = item.libraryID ? item.libraryID : 0;
		// Zotero.write("Button[Row[{Style[\"\\[DoubleStruckZ] \", 30, Red], \""+item.creators[0].lastName+". ("+item.date+") " +item["title"] +"\"}], SystemOpen[\"zotero://select/items/"+ library_id+"_"+item.key+"\"], BaseStyle->\"GenericButton\"]");
		// Zotero.write("Button[Row[{Style[\"\\[DoubleStruckZ] \", 30, Red], \""+item.creators[0].lastName+". ("+item.date+") " +item["title"] +"\"}], SystemOpen[\"zotero://select/items/"+ library_id+"_"+item.key+"\"], BaseStyle->\"GenericButton\"]");
		Zotero.write("{\"zotero://select/library/items/"+item.key+"\", \"" + item.citekey+"\", \""+ (item.creators.length>0?(item.creators[0].lastName+(item.creators.length>1?"et al.":"")):"unknown author")+ " ("+((item.date!=undefined)?item.date:"year")+")" +"\", \""+  (item["title"]!=undefined?item["title"]:"notitle")+"\", \""+ item.collections[0]+ "\"}");
	}
}
