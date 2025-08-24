import * as TaskPort from "elm-taskport";

const CLIENT_ID = import.meta.env.VITE_CLIENT_ID;
const SCOPES = [
  "https://www.googleapis.com/auth/userinfo.email",
  "https://www.googleapis.com/auth/drive.file",
];

const LOCAL_STORAGE_KEYS = {
  GOOGLE_EMAIL: "sentence_base_google_email",
  GOOGLE_TOKEN: "sentence_base_google_token",
};

let tokenCallback!: (
  tokenResponse: google.accounts.oauth2.TokenResponse,
) => void;
let tokenErrorCallback!: (
  error: google.accounts.oauth2.ClientConfigError,
) => void;
const tokenClient = google.accounts.oauth2.initTokenClient({
  client_id: CLIENT_ID,
  scope: SCOPES.join(" "),
  callback: (arg) => {
    tokenCallback(arg);
  },
  error_callback: (e) => {
    tokenErrorCallback(e);
  },
});

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
