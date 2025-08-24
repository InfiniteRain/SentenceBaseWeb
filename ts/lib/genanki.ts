import { sha256 } from "js-sha256";
import initSqlJs, { Database } from "sql.js";
import JSZip from "jszip";
import { saveAs } from "file-saver";

/**
 * [mkanki]{@link https://github.com/nornagon/mkanki}
 * @copyright Copyright (c) 2018 Jeremy Apthorp
 * @license AGPL-3.0 License
 *
 *
 * [genanki]{@link https://github.com/kerrickstaley/genanki}
 * @copyright Copyright (c) Kerrick Staley 2021
 * @license The MIT License
 *
 *
 * [genanki-js]{@link https://github.com/krmanik/genanki-js}
 * @copyright Copyright (c) 2021 Mani
 * @license AGPL-3.0 License
 */

type AnkiModelField = {
  name: string;
  ord: number;
  sticky: boolean;
  rtl: boolean;
  font: string;
  size: number;
  media: string[];
};

type AnkiModelTemplate = {
  name: string;
  ord: number;
  qfmt: string;
  afmt: string;
  did: number | null;
  bqfmt: string;
  bafmt: string;
};

type AnkiModel = {
  id: string;
  name: string;
  flds: AnkiModelField[];
  tmpls: AnkiModelTemplate[];
  req: [number, "all" | "any", number[]][];
  css: string;
  sortf: number;
  did: number;
  latexPre: string;
  latexPost: string;
  mod: number;
  usn: number;
  vers: string[];
  type: number;
  tags: string[];
};

export type ModelPropsField = {
  name: string;
};

export type ModelPropsTemplate = {
  name?: string;
  qfmt: string;
  afmt: string;
};

export type ModelProps = {
  id: string;
  name: string;
  flds: ModelPropsField[];
  tmpls: ModelPropsTemplate[];
  req: [number, "all" | "any", number[]][];
  css: string;
};

export type ClozeModelProps = {
  id: string;
  name: string;
  flds: ModelPropsField[];
  tmpl: ModelPropsTemplate;
};

const sqlPromise = new Promise<initSqlJs.SqlJsStatic>((resolve) => {
  initSqlJs({
    locateFile: () => `sql-wasm.wasm`,
  }).then((sql) => {
    resolve(sql);
  });
});

const BASE91_TABLE = [
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "j",
  "k",
  "l",
  "m",
  "n",
  "o",
  "p",
  "q",
  "r",
  "s",
  "t",
  "u",
  "v",
  "w",
  "x",
  "y",
  "z",
  "A",
  "B",
  "C",
  "D",
  "E",
  "F",
  "G",
  "H",
  "I",
  "J",
  "K",
  "L",
  "M",
  "N",
  "O",
  "P",
  "Q",
  "R",
  "S",
  "T",
  "U",
  "V",
  "W",
  "X",
  "Y",
  "Z",
  "0",
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "!",
  "#",
  "$",
  "%",
  "&",
  "(",
  ")",
  "*",
  "+",
  ",",
  "-",
  ".",
  "/",
  ":",
  ";",
  "<",
  "=",
  ">",
  "?",
  "@",
  "[",
  "]",
  "^",
  "_",
  "`",
  "{",
  "|",
  "}",
  "~",
];

function ankiHash(fields: string[]): string {
  const str = fields.join("__");
  const h = sha256.create();
  h.update(str);
  const hex = h.digest();

  let hash_int = 0n;
  for (let i = 0; i < 8; i++) {
    hash_int *= 256n;
    hash_int += BigInt(hex[i]);
  }

  // convert to the weird base91 format that Anki uses
  let rv_reversed = [];
  while (hash_int > 0) {
    rv_reversed.push(BASE91_TABLE[Number(hash_int % 91n)]);
    hash_int = hash_int / 91n;
  }

  return rv_reversed.reverse().join("");
}

const MODEL_STD = 0;
const MODEL_CLOZE = 1;

export class Model {
  props: AnkiModel;
  fieldNameToOrd: Record<string, number>;

  constructor(props: ModelProps) {
    console.log(props);
    this.props = {
      ...defaultModel,
      ...props,
      flds: props.flds.map((f, i) => ({ ...defaultField, ord: i, ...f })),
      tmpls: props.tmpls.map((t, i) => ({
        ...defaultTemplate,
        ord: i,
        name: `Card ${i + 1}`,
        ...t,
      })),
      mod: new Date().getTime(),
    };
    this.fieldNameToOrd = {};
    this.props.flds.forEach((f) => {
      this.fieldNameToOrd[f.name] = f.ord;
    });
  }

  note(
    fields: string[],
    tags: string[] | null = null,
    guid: string | null = null,
  ): Note {
    if (fields.length !== this.props.flds.length) {
      throw new Error(
        `Expected ${
          this.props.flds.length
        } fields for model '${this.props.name}' but got ${fields.length}`,
      );
    }

    return new Note(this, fields, tags, guid);
  }
}

export class ClozeModel extends Model {
  constructor(props: ClozeModelProps) {
    super({
      css: `
         .card {
           font-family: arial;
           font-size: 20px;
           text-align: center;
           color: black;
           background-color: white;
         }
 
         .cloze {
           font-weight: bold;
           color: blue;
         }
       `,
      tmpls: [{ name: "Cloze", ...props.tmpl }],
      req: [],
      ...props,
    });

    this.props.type = MODEL_CLOZE;
  }
}

const defaultModel = {
  sortf: 0, // sort field
  did: 1, // deck id
  latexPre: `\\documentclass[12pt]{article}
 \\special{papersize=3in,5in}
 \\usepackage[utf8]{inputenc}
 \\usepackage{amssymb,amsmath}
 \\pagestyle{empty}
 \\setlength{\\parindent}{0in}
 \\begin{document}`,
  latexPost: "\\end{document}",
  mod: 0, // modification time
  usn: 0, // unsure, something to do with sync?
  vers: [], // seems to be unused
  type: MODEL_STD,
  css: `.card {
  font-family: arial;
  font-size: 20px;
  text-align: center;
  color: black;
  background-color: white;
 }`,
  /* also:
    name: string,
    flds: [Field],
    tmpls: [Template],
    tags: [??],
    id: string
    */
  tags: [],
};

const defaultField = {
  name: "",
  ord: null,
  sticky: false,
  rtl: false,
  font: "Arial",
  size: 20,
  media: [],
};

const defaultTemplate = {
  name: "",
  ord: null,
  qfmt: "",
  afmt: "",
  did: null,
  bqfmt: "",
  bafmt: "",
};

// whether new cards should be mixed with reviews, or shown first or last
const NEW_CARDS_DISTRIBUTE = 0;

const defaultConf = {
  // review options
  activeDecks: [1],
  curDeck: 1,
  newSpread: NEW_CARDS_DISTRIBUTE,
  collapseTime: 1200,
  timeLim: 0,
  estTimes: true,
  dueCounts: true,
  // other config
  curModel: null,
  nextPos: 1,
  sortType: "noteFld",
  sortBackwards: false,
  addToCur: true, // add new to currently selected deck?
  dayLearnFirst: false,
};

// new card insertion order
const NEW_CARDS_DUE = 1;

const STARTING_FACTOR = 2500;

const defaultDeckConf = {
  name: "Default",
  new: {
    delays: [1, 10],
    ints: [1, 4, 7], // 7 is not currently used
    initialFactor: STARTING_FACTOR,
    separate: true,
    order: NEW_CARDS_DUE,
    perDay: 20,
    // may not be set on old decks
    bury: false,
  },
  lapse: {
    delays: [10],
    mult: 0,
    minInt: 1,
    leechFails: 8,
    // type 0=suspend, 1=tagonly
    leechAction: 0,
  },
  rev: {
    perDay: 200,
    ease4: 1.3,
    fuzz: 0.05,
    minSpace: 1, // not currently used
    ivlFct: 1,
    maxIvl: 36500,
    // may not be set on old decks
    bury: false,
    hardFactor: 1.2,
  },
  maxTaken: 60,
  timer: 0,
  autoplay: true,
  replayq: true,
  mod: 0,
  usn: 0,
};

const defaultDeck = {
  newToday: [0, 0], // currentDay, count
  revToday: [0, 0],
  lrnToday: [0, 0],
  timeToday: [0, 0], // time in ms
  conf: 1,
  usn: 0,
  desc: "",
  dyn: 0, // anki uses int/bool interchangably here
  collapsed: false,
  // added in beta11
  extendNew: 10,
  extendRev: 50,
};

export class Deck {
  id: number;
  name: string;
  desc: string;
  notes: Note[];

  constructor(id: number, name: string, desc: string = "") {
    this.id = id;
    this.name = name;
    this.desc = desc;
    this.notes = [];
  }

  addNote(note: Note): void {
    this.notes.push(note);
  }
}

export class Note {
  #guid: string | null;

  model: Model;
  fields: string[];
  tags: string[] | null;

  constructor(
    model: Model,
    fields: string[],
    tags: string[] | null = null,
    guid: string | null = null,
  ) {
    this.model = model;
    this.fields = fields;
    this.tags = tags;
    this.#guid = guid;
  }

  get guid(): string {
    return this.#guid ? this.#guid : ankiHash(this.fields);
  }

  get cards(): number[] {
    if (this.model.props.type === MODEL_STD) {
      const isEmpty = (f: string) => {
        return !f || f.toString().trim().length === 0;
      };
      const rv: number[] = [];
      for (const [card_ord, any_or_all, required_field_ords] of this.model.props
        .req) {
        const op = any_or_all === "any" ? "some" : "every";
        if (required_field_ords[op]((f) => !isEmpty(this.fields[f]))) {
          rv.push(card_ord);
        }
      }
      return rv;
    } else {
      // the below logic is copied from anki's ModelManager._availClozeOrds
      const ords = new Set<number>();
      const matches = [];
      const curliesRe = /{{[^}]*?cloze:(?:[^}]?:)*(.+?)}}/g;
      const percentRe = /<%cloze:(.+?)%>/g;
      const { qfmt } = this.model.props.tmpls[0]; // cloze models only have 1 template
      let m;
      while ((m = curliesRe.exec(qfmt))) matches.push(m[1]);
      while ((m = percentRe.exec(qfmt))) matches.push(m[1]);
      const map: Record<string, [number, AnkiModelField]> = {};
      this.model.props.flds.forEach((fld, i) => {
        map[fld.name] = [i, fld];
      });
      for (const fname of matches) {
        if (!(fname in map)) continue;
        const ord = map[fname][0];
        const re = /{{c(\d+)::.+?}}/gs;
        while ((m = re.exec(this.fields[ord]))) {
          const i = parseInt(m[1]);
          if (i > 0) ords.add(i - 1);
        }
      }
      if (ords.size === 0) {
        // empty clozes use first ord
        return [0];
      }
      return Array.from(ords);
    }
  }
}

export class Package {
  decks: Deck[];
  media: ({ name: string; data: Blob } | { name: string; filename: string })[];

  constructor() {
    this.decks = [];
    this.media = [];
  }

  addDeck(deck: Deck): void {
    this.decks.push(deck);
  }

  addMedia(data: Blob, name: string): void {
    this.media.push({ name, data });
  }

  addMediaFile(filename: string, name: string | null = null): void {
    this.media.push({ name: name || filename, filename });
  }

  writeToFile(filename: string): void {
    sqlPromise.then((sql) => {
      var db = new sql.Database();
      db.run(APKG_SCHEMA);

      this.write(db);

      var zip = new JSZip();

      const data = db.export();
      const buffer = new Uint8Array(data).buffer;

      zip.file("collection.anki2", buffer);

      const media_info: Record<number, string> = {};

      this.media.forEach((m, i) => {
        if ("filename" in m) {
          zip.file(i.toString(), m.filename);
        } else {
          zip.file(i.toString(), m.data);
        }

        media_info[i] = m.name;
      });

      zip.file("media", JSON.stringify(media_info));

      zip
        .generateAsync({ type: "blob", mimeType: "application/apkg" })
        .then(function (content) {
          saveAs(content, filename);
        });
    });
  }

  write(db: Database): void {
    const defaultDeckToAdd = { ...defaultDeck, id: 1, name: "Default" };

    const now = new Date();
    const models: Record<string, AnkiModel> = {};
    const decks: Record<string, typeof defaultDeckToAdd> = {};

    // AnkiDroid failed to import subdeck, So add a Default deck
    decks["1"] = defaultDeckToAdd;

    this.decks.forEach((d) => {
      d.notes.forEach((n) => (models[n.model.props.id] = n.model.props));
      decks[d.id] = {
        ...defaultDeck,
        id: d.id,
        name: d.name,
        desc: d.desc,
      };
    });

    const col = [
      null, // id
      (+now / 1000) | 0, // crt
      +now, // mod
      +now, // scm
      11, // ver
      0, // dty
      0, // usn
      0, // ls
      JSON.stringify(defaultConf), // conf
      JSON.stringify(models), // models
      JSON.stringify(decks), // decks
      JSON.stringify({ 1: { id: 1, ...defaultDeckConf } }), // dconf
      JSON.stringify({}), // tags
    ];

    db.prepare(
      `INSERT INTO col
         (id, crt, mod, scm, ver, dty, usn, ls, conf, models, decks, dconf, tags)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
    ).run(col);

    const insert_notes =
      db.prepare(`INSERT INTO notes (id, guid, mid, mod, usn, tags, flds, sfld, csum, flags, data) 
        VALUES (null, ?, ?, ?, ?, ?, ?, ?, 0, 0, '')`);

    const insert_cards =
      db.prepare(`INSERT INTO cards (id, nid, did, ord, mod, usn, type, queue, due, ivl, factor, reps, lapses, left, odue, odid, flags, data) 
        VALUES (null, ?, ?, ?, ?, ?, ?, ?, 0, 0, 0, 0, 0, 0, 0, 0, 0, '')`);

    for (const deck of this.decks) {
      for (const note of deck.notes) {
        var tags = note.tags == null ? "" : note.tags.join(" ");
        insert_notes.run([
          note.guid, // guid
          note.model.props.id, // mid
          (+now / 1000) | 0, // mod
          -1, // usn
          tags, // tags
          note.fields.join("\x1f"), // flds
          0, // sfld
        ]);

        var rowID = db.exec("select last_insert_rowid();");
        var note_id = rowID[0]["values"][0][0];

        for (const card_ord of note.cards) {
          insert_cards.run([
            note_id, // nid
            deck.id, // did
            card_ord, // ord
            (+now / 1000) | 0, // mod
            -1, // usn
            0, // type 0=new, 1=learning, 2=due
            0, // queue -1 for suspended
          ]);
        }
      }
    }
  }
}

/*
 * [genanki]{@link https://github.com/kerrickstaley/genanki}
 * @copyright Copyright (c) Kerrick Staley 2021
 * @license The MIT License
 */

const APKG_SCHEMA = `
PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;

CREATE TABLE col (
    id              integer primary key,
    crt             integer not null,
    mod             integer not null,
    scm             integer not null,
    ver             integer not null,
    dty             integer not null,
    usn             integer not null,
    ls              integer not null,
    conf            text not null,
    models          text not null,
    decks           text not null,
    dconf           text not null,
    tags            text not null
);
CREATE TABLE notes (
    id              integer primary key,   /* 0 */
    guid            text not null,         /* 1 */
    mid             integer not null,      /* 2 */
    mod             integer not null,      /* 3 */
    usn             integer not null,      /* 4 */
    tags            text not null,         /* 5 */
    flds            text not null,         /* 6 */
    sfld            integer not null,      /* 7 */
    csum            integer not null,      /* 8 */
    flags           integer not null,      /* 9 */
    data            text not null          /* 10 */
);
CREATE TABLE cards (
    id              integer primary key,   /* 0 */
    nid             integer not null,      /* 1 */
    did             integer not null,      /* 2 */
    ord             integer not null,      /* 3 */
    mod             integer not null,      /* 4 */
    usn             integer not null,      /* 5 */
    type            integer not null,      /* 6 */
    queue           integer not null,      /* 7 */
    due             integer not null,      /* 8 */
    ivl             integer not null,      /* 9 */
    factor          integer not null,      /* 10 */
    reps            integer not null,      /* 11 */
    lapses          integer not null,      /* 12 */
    left            integer not null,      /* 13 */
    odue            integer not null,      /* 14 */
    odid            integer not null,      /* 15 */
    flags           integer not null,      /* 16 */
    data            text not null          /* 17 */
);
CREATE TABLE revlog (
    id              integer primary key,
    cid             integer not null,
    usn             integer not null,
    ease            integer not null,
    ivl             integer not null,
    lastIvl         integer not null,
    factor          integer not null,
    time            integer not null,
    type            integer not null
);
CREATE TABLE graves (
    usn             integer not null,
    oid             integer not null,
    type            integer not null
);
CREATE INDEX ix_notes_usn on notes (usn);
CREATE INDEX ix_cards_usn on cards (usn);
CREATE INDEX ix_revlog_usn on revlog (usn);
CREATE INDEX ix_cards_nid on cards (nid);
CREATE INDEX ix_cards_sched on cards (did, queue, due);
CREATE INDEX ix_revlog_cid on revlog (cid);
CREATE INDEX ix_notes_csum on notes (csum);
COMMIT;
`;
