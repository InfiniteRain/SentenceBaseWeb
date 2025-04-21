import { Elm } from "./Main.elm";
import { match } from "ts-pattern";

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
};

type IncomingMessage =
  | { tag: "initializeRequest" }
  | { tag: "authenticateRequest" };

type OutgoingMessage =
  | {
      tag: "initializedResponse";
    }
  | {
      tag: "initializeFailedResponse";
      message: string;
    }
  | {
      tag: "authenticateResponse";
      token: string;
    }
  | {
      tag: "authenticateFailedResponse";
      message: string;
    }
  | {
      tag: "interactionRequiredResponse";
    };

type Ports = {
  messageSenderPort: ElmReceivePort<IncomingMessage>;
  messageReceiverPort: ElmSendPort<OutgoingMessage>;
  clipboardPort: ElmSendPort<string>;
};

const app = Elm.Main.init<Ports>({
  node: document.getElementById("app"),
});

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

app.ports.messageSenderPort.subscribe((message) => {
  match(message)
    .with({ tag: "initializeRequest" }, () => {
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

          app.ports.messageReceiverPort.send({
            tag: "initializedResponse",
          });
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

          app.ports.messageReceiverPort.send({
            tag: "initializeFailedResponse",
            message,
          });
        }
      });
    })
    .with({ tag: "authenticateRequest" }, () => {
      if (tokenClient === null) {
        app.ports.messageReceiverPort.send({
          tag: "authenticateFailedResponse",
          message: "Token client was not initialized.",
        });
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

          app.ports.messageReceiverPort.send({
            tag: "authenticateResponse",
            token: arg.access_token,
          });
        } else if (arg.error === "interaction_required") {
          app.ports.messageReceiverPort.send({
            tag: "interactionRequiredResponse",
          });
        } else {
          app.ports.messageReceiverPort.send({
            tag: "authenticateFailedResponse",
            message: arg.error_description,
          });
        }
      };
      tokenErrorCallback = (err) => {
        app.ports.messageReceiverPort.send({
          tag: "authenticateFailedResponse",
          message: err.message,
        });
      };

      const email = localStorage.getItem(LOCAL_STORAGE_KEYS.GOOGLE_EMAIL);

      if (email) {
        tokenClient.requestAccessToken({ prompt: "none", login_hint: email });
      } else {
        tokenClient.requestAccessToken({ prompt: "consent" });
      }
    })
    .exhaustive();
});

let lastText: string | null = null;
const clipboardInterval = 100;
const clipboardSnapshot = async () => {
  let text: string;
  try {
    text = await navigator.clipboard.readText();
    if (text !== lastText) {
      app.ports.clipboardPort.send(text);
      lastText = text;
    }
  } catch {
  } finally {
    setTimeout(clipboardSnapshot, clipboardInterval);
  }
};

clipboardSnapshot();
