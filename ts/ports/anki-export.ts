import * as TaskPort from "elm-taskport";
import { Model, Deck, Package } from "../lib/genanki";

type AnkiModel = {
  id: number;
  name: string;
  fields: { name: string }[];
  templates: { name: string | null; frontHtml: string; backHtml: string }[];
  styling: string;
};

type AnkiExportArg = {
  deck: {
    id: number;
    name: string;
    models: {
      [_: number]: AnkiModel;
    };
    notes: {
      [_: number]: string[][];
    };
    files: [string, string][];
  };
  fileName: string;
};

TaskPort.register("ankiExport", async (config: AnkiExportArg) => {
  const models = new Map<string, Model>();

  for (const [key, { name, fields, templates, styling }] of Object.entries(
    config.deck.models,
  )) {
    models.set(
      key,
      new Model({
        name,
        id: key,
        flds: fields,
        req: [],
        tmpls: templates.map((template) => ({
          ...(template.name !== null ? { name: template.name } : {}),
          qfmt: template.frontHtml,
          afmt: template.backHtml,
        })),
        css: styling,
      }),
    );
  }

  const deck = new Deck(config.deck.id, config.deck.name);

  for (const [key, notes] of Object.entries(config.deck.notes)) {
    for (const note of notes) {
      deck.addNote(models.get(key)!.note(note));
    }
  }

  const ankiPackage = new Package();

  for (const [fileName, data] of config.deck.files) {
    const result = await fetch(data);
    const blob = await result.blob();

    ankiPackage.addMedia(blob, fileName);
  }

  ankiPackage.addDeck(deck);
  ankiPackage.writeToFile(config.fileName);
});
