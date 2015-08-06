# -*- coding: utf-8 -*-
# Copyright: Damien Elmes <anki@ichi2.net>
# License: GNU AGPL, version 3 or later; http://www.gnu.org/licenses/agpl.html

import codecs
import csv
import re
import  cgi
from anki.consts import NEW_CARDS_RANDOM
from anki.lang import ngettext

from anki.utils import fieldChecksum, guid64, timestampID, \
    joinFields, intTime, splitFields
from anki.lang import _

from anki.importing.base import Importer

# Stores a list of fields, tags and deck
######################################################################

class ForeignNote(object):
    "An temporary object storing fields and attributes."
    def __init__(self):
        self.fields = []
        self.tags = []
        self.deck = None
        self.cards = {} # map of ord -> card

class ForeignCard(object):
    def __init__(self):
        self.due = 0
        self.ivl = 1
        self.factor = 2500
        self.reps = 0
        self.lapses = 0


class KnowledgeImporter(Importer):

    needDelimiter = False
    patterns = ("\t", ";")

    needMapper = True
    allowHTML = True
    importMode = 0

    def __init__(self, *args):
        #Inits with
        # args[0] = Collection obj
        # args[1] = Path to imported file
        # args[2] = Deck name
        Importer.__init__(self, args[0], args[1])
        self.model = args[0].models.current()
        self.mapping = None
        self._deckMap = {}

        self.deck = args[2].strip()
        self.lines = None
        self.fileobj = None
        self.delimiter = '#'
        self.tagsToAdd = []

    def initMapping(self):
        print self.model['flds']
        flds = [f['name'] for f in self.model['flds']]
        # truncate to provided count
        flds = flds[0:self.fields()]
        # if there's room left, add tags
        if self.fields() > len(flds):
            flds.append("_tags")
        # and if there's still room left, pad
        flds = flds + [None] * (self.fields() - len(flds))
        self.mapping = flds

    def run(self):
        print "run()"
        assert self.mapping
        c = self.foreignNotes()
        self.importNotes(c)

    def newData(self, n):
        print "newData()"
        id = self._nextID
        self._nextID += 1
        self._ids.append(id)
        if not self.processFields(n):
            return
        # note id for card updates later
        for ord, c in n.cards.items():
            self._cards.append((id, ord, c))
        self.col.tags.register(n.tags)
        return [id, guid64(), self.model['id'],
                intTime(), self.col.usn(), self.col.tags.join(n.tags),
                n.fieldsStr, "", "", 0, ""]

    def mappingOk(self):
        print "mappingOK()"
        return self.model['flds'][0]['name'] in self.mapping

    # def open(self):
        
    #     "Open file and ensure it's in the right format."
    #     return

    def importNotes(self, notes):
        "Convert each card into a note, apply attributes and add to col."
        print "importNotes()"
        print notes

        assert self.mappingOk()
        # note whether tags are mapped
        self._tagsMapped = False
        for f in self.mapping:
            if f == "_tags":
                self._tagsMapped = True
        # gather checks for duplicate comparison
        csums = {}
        for csum, id in self.col.db.execute(
            "select csum, id from notes where mid = ?", self.model['id']):
            if csum in csums:
                csums[csum].append(id)
            else:
                csums[csum] = [id]
        firsts = {}
        fld0idx = self.mapping.index(self.model['flds'][0]['name'])
        self._fmap = self.col.models.fieldMap(self.model)
        self._nextID = timestampID(self.col.db, "notes")
        # loop through the notes
        updates = []
        updateLog = []
        updateLogTxt = _("First field matched: %s")
        dupeLogTxt = _("Added duplicate with first field: %s")
        new = []
        self._ids = []
        self._cards = []
        self._emptyNotes = False
        for n in notes:
            if not self.allowHTML:
                for c in range(len(n.fields)):
                    n.fields[c] = cgi.escape(n.fields[c])
            fld0 = n.fields[fld0idx]
            csum = fieldChecksum(fld0)
            # first field must exist
            if not fld0:
                self.log.append(_("Empty first field: %s") %
                                " ".join(n.fields))
                continue
            # earlier in import?
            if fld0 in firsts and self.importMode != 2:
                # duplicates in source file; log and ignore
                self.log.append(_("Appeared twice in file: %s") %
                                fld0)
                continue
            firsts[fld0] = True
            # already exists?
            found = False
            if csum in csums:
                # csum is not a guarantee; have to check
                for id in csums[csum]:
                    flds = self.col.db.scalar(
                        "select flds from notes where id = ?", id)
                    sflds = splitFields(flds)
                    if fld0 == sflds[0]:
                        # duplicate
                        found = True
                        if self.importMode == 0:
                            data = self.updateData(n, id, sflds)
                            if data:
                                updates.append(data)
                                updateLog.append(updateLogTxt % fld0)
                                found = True
                            break
                        elif self.importMode == 2:
                            # allow duplicates in this case
                            updateLog.append(dupeLogTxt % fld0)
                            found = False
            # newly add
            if not found:
                data = self.newData(n)
                if data:
                    new.append(data)
                    # note that we've seen this note once already
                    firsts[fld0] = True

        did = self.col.decks.id(self.deck)
        self.col.decks.select(did)
        #SUPER IMPORTANT (setting the associated deck to the model)
        self.model['did'] = did

        print "Selected: ", self.col.decks.get(self.col.decks.selected());

        self.addNew(new)
        self.addUpdates(updates)
        # make sure to update sflds, etc
        self.col.updateFieldCache(self._ids)
        # generate cards
        if self.col.genCards(self._ids):
            self.log.insert(0, _(
                "Empty cards found. Please run Tools>Empty Cards."))
        # apply scheduling updates
        self.updateCards()
        # we randomize or order here, to ensure that siblings
        # have the same due#
        

        # m = self.col.models.byName("Basic")
        # deck = self.col.decks.get(did)
        # deck['mid'] = m['id']
        # self.col.decks.save(deck)

        # print "Deck:", self.col.decks.byName(self.deck)
        # print "DID:", did
        
        # save tags to model
        # m = self.note.model()
        # m['tags'] = self.note.tags
        # self.mw.col.models.save(m)

        conf = self.col.decks.confForDid(did)
        # print "Conf: ",conf
        # in order due?
        if conf['new']['order'] == NEW_CARDS_RANDOM:
            self.col.sched.randomizeCards(did)
        else:
            self.col.sched.orderCards(did)
        part1 = ngettext("%d note added", "%d notes added", len(new)) % len(new)
        part2 = ngettext("%d note updated", "%d notes updated", self.updateCount) % self.updateCount
        self.log.append("%s, %s." % (part1, part2))
        print part1, part2, "on deck: [", self.deck, "]"
        self.log.extend(updateLog)
        if self._emptyNotes:
            print "there were empty notes"
            self.log.append(_("""\
One or more notes were not imported, because they didn't generate any cards. \
This can happen when you have empty fields or when you have not mapped the \
content in the text file to the correct fields."""))
        self.total = len(self._ids)

    def foreignNotes(self):
        print "foreignNotes()"
        self.open()
        # process all lines
        log = []
        notes = []
        lineNum = 0
        ignored = 0
        if self.delimiter:
            reader = csv.reader(self.data, delimiter=self.delimiter, doublequote=True)
        else:
            reader = csv.reader(self.data, self.dialect, doublequote=True)
        try:
            for row in reader:
                row = [unicode(x, "utf-8") for x in row]
                if len(row) != self.numFields:
                    if row:
                        log.append(_(
                            "'%(row)s' had %(num1)d fields, "
                            "expected %(num2)d") % {
                            "row": u" ".join(row),
                            "num1": len(row),
                            "num2": self.numFields,
                            })
                        ignored += 1
                    continue
                note = self.noteFromFields(row)
                notes.append(note)
        except (csv.Error), e:
            log.append(_("Aborted: %s") % str(e))
        self.log = log
        self.ignored = ignored
        self.fileobj.close()
        return notes

    def open(self):
        print "open()"
        # "Parse the top line and determine the pattern and number of fields."
        # load & look for the right pattern
        self.cacheFile()

    def cacheFile(self):
        "Read file into self.lines if not already there."
        print "cacheFile()"
        if not self.fileobj:
            self.openFile()

    def openFile(self):
        print "openFile()"
        self.dialect = None
        self.fileobj = open(self.file, "rbU")
        self.data = self.fileobj.read()
        if self.data.startswith(codecs.BOM_UTF8):
            self.data = self.data[len(codecs.BOM_UTF8):]
        def sub(s):
            return re.sub("^\#.*$", "__comment", s)
        self.data = [sub(x)+"\n" for x in self.data.split("\n") if sub(x) != "__comment"]
        if self.data:
            if self.data[0].startswith("tags:"):
                tags = unicode(self.data[0][5:], "utf8").strip()
                self.tagsToAdd = tags.split(" ")
                del self.data[0]
            self.updateDelimiter()
        if not self.dialect and not self.delimiter:
            raise Exception("unknownFormat")

    def updateDelimiter(self):
        print "updateDelimiter()"
        def err():
            raise Exception("unknownFormat")
        self.dialect = None
        sniffer = csv.Sniffer()
        delims = ['#']
        if not self.delimiter:
            try:
                self.dialect = sniffer.sniff("\n".join(self.data[:10]),
                                             delims)
            except:
                try:
                    self.dialect = sniffer.sniff(self.data[0], delims)
                except:
                    pass
        if self.dialect:
            try:
                reader = csv.reader(self.data, self.dialect, doublequote=True)
            except:
                err()
        else:
            if not self.delimiter:
                if "\t" in self.data[0]:
                    self.delimiter = "\t"
                elif ";" in self.data[0]:
                    self.delimiter = ";"
                elif "," in self.data[0]:
                    self.delimiter = ","
                else:
                    self.delimiter = " "
            reader = csv.reader(self.data, delimiter=self.delimiter, doublequote=True)
        try:
            while True:
                row = reader.next()
                if row:
                    self.numFields = len(row)
                    break
        except:
            err()
        self.initMapping()

    def fields(self):
        "Number of fields."
        print "fields()"
        self.open()
        return self.numFields

    def noteFromFields(self, fields):
        print "noteFromFields()"
        note = ForeignNote()
        note.deck = self.deck
        note.fields.extend([x.strip().replace("\n", "<br>") for x in fields])
        note.tags.extend(self.tagsToAdd)
        return note




    def addNew(self, rows):
        print "addNew()"
        self.col.db.executemany(
            "insert or replace into notes values (?,?,?,?,?,?,?,?,?,?,?)",
            rows)

    def updateData(self, n, id, sflds):
        print "updateData()"
        self._ids.append(id)
        if not self.processFields(n, sflds):
            return
        if self._tagsMapped:
            self.col.tags.register(n.tags)
            tags = self.col.tags.join(n.tags)
            return [intTime(), self.col.usn(), n.fieldsStr, tags,
                    id, n.fieldsStr, tags]
        else:
            return [intTime(), self.col.usn(), n.fieldsStr,
                    id, n.fieldsStr]

    def addUpdates(self, rows):
        print "addUpdates()"
        old = self.col.db.totalChanges()
        if self._tagsMapped:
            self.col.db.executemany("""
update notes set mod = ?, usn = ?, flds = ?, tags = ?
where id = ? and (flds != ? or tags != ?)""", rows)
        else:
            self.col.db.executemany("""
update notes set mod = ?, usn = ?, flds = ?
where id = ? and flds != ?""", rows)
        self.updateCount = self.col.db.totalChanges() - old

    def processFields(self, note, fields=None):
        print "processFields()"
        if not fields:
            fields = [""]*len(self.model['flds'])
        for c, f in enumerate(self.mapping):
            if not f:
                continue
            elif f == "_tags":
                note.tags.extend(self.col.tags.split(note.fields[c]))
            else:
                sidx = self._fmap[f][0]
                fields[sidx] = note.fields[c]
        note.fieldsStr = joinFields(fields)
        ords = self.col.models.availOrds(self.model, note.fieldsStr)
        if not ords:
            self._emptyNotes = True
        return ords

    def updateCards(self):
        print "updateCards()"
        data = []
        for nid, ord, c in self._cards:
            data.append((c.ivl, c.due, c.factor, c.reps, c.lapses, nid, ord))
        # we assume any updated cards are reviews
        self.col.db.executemany("""
update cards set type = 2, queue = 2, ivl = ?, due = ?,
factor = ?, reps = ?, lapses = ? where nid = ? and ord = ?""", data)