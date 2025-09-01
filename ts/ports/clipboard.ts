import * as TaskPort from "elm-taskport";

TaskPort.register("readClipboard", async () => {
  try {
    return await navigator.clipboard.readText();
  } catch {
    return "";
  }
});
