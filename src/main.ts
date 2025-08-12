import { Elm } from "./Main.elm";
import * as TaskPort from "elm-taskport";
import "./style.css";

const CLIENT_ID = import.meta.env.VITE_CLIENT_ID;
const API_KEY = import.meta.env.VITE_API_KEY;
const DISCOVERY_DOC =
  "https://www.googleapis.com/discovery/v1/apis/drive/v3/rest";
const SCOPES = [
  "https://www.googleapis.com/auth/userinfo.email",
  "https://www.googleapis.com/auth/drive.file",
];
const DEFAULT_ERROR = "Unknown error.";

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
  init?: RequestInit,
): Promise<T> => {
  const finalInit = {
    ...init,
    headers: {
      ...init?.headers,
      Authorization: `Bearer ${gapi.client.getToken().access_token}`,
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
    new Promise<void>((resolve, reject) => {
      gapi.load("client", async () => {
        try {
          await gapi.client.init({
            apiKey: API_KEY,
            discoveryDocs: [DISCOVERY_DOC],
          });

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
        } catch (e) {
          const message =
            typeof e === "object" && e !== null
              ? "message" in e && typeof e.message === "string"
                ? e.message
                : "error" in e &&
                    typeof e.error === "object" &&
                    e.error != null &&
                    "message" in e.error &&
                    typeof e.error.message === "string"
                  ? e.error.message
                  : DEFAULT_ERROR
              : DEFAULT_ERROR;

          reject(new Error(message));
        }
      });
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
