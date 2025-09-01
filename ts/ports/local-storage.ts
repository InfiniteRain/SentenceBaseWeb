import * as TaskPort from "elm-taskport";

const localStoragePrefix = "sentence_base_storage_";

TaskPort.register(
  "localStorageSet",
  async ({ key, value }: { key: string; value: any }) => {
    localStorage.setItem(`${localStoragePrefix}${key}`, JSON.stringify(value));
  },
);

TaskPort.register("localStorageGet", async (key: string) => {
  const item = localStorage.getItem(`${localStoragePrefix}${key}`);

  if (item === null) {
    return null;
  }

  return JSON.parse(item);
});

TaskPort.register("localStorageRemove", async (key: string) => {
  localStorage.removeItem(`${localStoragePrefix}${key}`);
});
