import { Elm } from "./Main.elm";
import * as TaskPort from "elm-taskport";
import "./style.css";
import { Model, Deck, Package } from "./genanki";

const CLIENT_ID = import.meta.env.VITE_CLIENT_ID;
const SCOPES = [
  "https://www.googleapis.com/auth/userinfo.email",
  "https://www.googleapis.com/auth/drive.file",
];

const LOCAL_STORAGE_KEYS = {
  GOOGLE_EMAIL: "sentence_base_google_email",
  GOOGLE_TOKEN: "sentence_base_google_token",
};

let tokenClient: google.accounts.oauth2.TokenClient | null = null;
let tokenCallback!: (
  tokenResponse: google.accounts.oauth2.TokenResponse,
) => void;
let tokenErrorCallback!: (
  error: google.accounts.oauth2.ClientConfigError,
) => void;

const googleApiFetch = async <T>(
  input: RequestInfo | URL,
  token: string,
  init?: RequestInit,
): Promise<T> => {
  const finalInit = {
    ...init,
    headers: {
      ...init?.headers,
      Authorization: `Bearer ${token}`,
    },
  };
  let response = await fetch(input, finalInit);
  return await response.json();
};

TaskPort.install({
  logCallErrors: true,
  logInteropErrors: true,
});

TaskPort.register(
  "googleInitialize",
  () =>
    new Promise<void>((resolve) => {
      tokenClient = google.accounts.oauth2.initTokenClient({
        client_id: CLIENT_ID,
        scope: SCOPES.join(" "),
        callback: (arg) => {
          tokenCallback(arg);
        },
        error_callback: (e) => {
          tokenErrorCallback(e);
        },
      });

      resolve();
    }),
);

TaskPort.register(
  "googleGetToken",
  (shouldRefresh: boolean) =>
    new Promise<string>((resolve, reject) => {
      if (tokenClient === null) {
        reject(new Error("Token client was not initialized."));
        return;
      }

      const token = localStorage.getItem(LOCAL_STORAGE_KEYS.GOOGLE_TOKEN);

      if (token && !shouldRefresh) {
        resolve(token);
        return;
      }

      tokenCallback = async (arg) => {
        if (!arg.error) {
          if (!localStorage.getItem(LOCAL_STORAGE_KEYS.GOOGLE_EMAIL)) {
            const userInfo = await googleApiFetch<{ email: string }>(
              "https://www.googleapis.com/oauth2/v2/userinfo",
              arg.access_token,
            );
            localStorage.setItem(
              LOCAL_STORAGE_KEYS.GOOGLE_EMAIL,
              userInfo.email,
            );
          }

          localStorage.setItem(
            LOCAL_STORAGE_KEYS.GOOGLE_TOKEN,
            arg.access_token,
          );

          resolve(arg.access_token);
        } else if (arg.error === "interaction_required") {
          localStorage.removeItem(LOCAL_STORAGE_KEYS.GOOGLE_EMAIL);
          localStorage.removeItem(LOCAL_STORAGE_KEYS.GOOGLE_TOKEN);
          location.reload();
        } else {
          reject(new Error(arg.error_description));
        }
      };
      tokenErrorCallback = (err) => {
        reject(new Error(err.message));
      };

      const email = localStorage.getItem(LOCAL_STORAGE_KEYS.GOOGLE_EMAIL);

      if (email) {
        tokenClient.requestAccessToken({ prompt: "none", login_hint: email });
      } else {
        tokenClient.requestAccessToken({ prompt: "consent" });
      }
    }),
);

TaskPort.register("readClipboard", async () => {
  try {
    return await navigator.clipboard.readText();
  } catch {}
});

TaskPort.register(
  "timeout",
  (config: { id: number; timeout: number }) =>
    new Promise((resolve) => {
      setTimeout(() => {
        resolve(config.id);
      }, config.timeout);
    }),
);

const localStoragePrefix = "sentence_base_storage_";

TaskPort.register(
  "localStorageSet",
  async ({ key, value }: { key: string; value: any }) => {
    console.log("set: ", { key, value });
    localStorage.setItem(`${localStoragePrefix}${key}`, JSON.stringify(value));
  },
);

TaskPort.register("localStorageGet", async (key: string) => {
  console.log("get: ", key);
  return JSON.parse(localStorage.getItem(`${localStoragePrefix}${key}`) ?? "");
});

TaskPort.register("localStorageRemove", async (key: string) => {
  localStorage.removeItem(`${localStoragePrefix}${key}`);
});

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

  ankiPackage.addDeck(deck);
  ankiPackage.writeToFile(config.fileName);
});

const getRandomInts = (n: number) => {
  const randInts = new Uint32Array(n);
  crypto.getRandomValues(randInts);
  return Array.from(randInts);
};

const randomInts = getRandomInts(4);

Elm.Main.init({
  node: document.getElementById("app"),
  flags: {
    seed1: randomInts[0],
    seed2: randomInts[1],
    seed3: randomInts[2],
    seed4: randomInts[3],
  },
});
