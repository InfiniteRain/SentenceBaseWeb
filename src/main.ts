import { Elm } from "./Main.elm";
import * as TaskPort from "elm-taskport";
import "./style.css";

const CLIENT_ID = import.meta.env.VITE_CLIENT_ID;
const SCOPES = ["https://www.googleapis.com/auth/drive.file"];

const LOCAL_STORAGE_KEYS = {
  GOOGLE_TOKEN: "sentence_base_google_token",
};

let tokenClient: google.accounts.oauth2.TokenClient | null = null;
let tokenCallback!: (
  tokenResponse: google.accounts.oauth2.TokenResponse,
) => void;
let tokenErrorCallback!: (
  error: google.accounts.oauth2.ClientConfigError,
) => void;

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
          localStorage.setItem(
            LOCAL_STORAGE_KEYS.GOOGLE_TOKEN,
            arg.access_token,
          );

          resolve(arg.access_token);
        } else if (arg.error === "interaction_required") {
          localStorage.removeItem(LOCAL_STORAGE_KEYS.GOOGLE_TOKEN);
          location.reload();
        } else {
          reject(new Error(arg.error_description));
        }
      };
      tokenErrorCallback = (err) => {
        reject(new Error(err.message));
      };

      if (token) {
        tokenClient.requestAccessToken({ prompt: "none", login_hint: token });
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
