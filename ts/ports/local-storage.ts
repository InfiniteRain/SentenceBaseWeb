import * as TaskPort from "elm-taskport";

const localStoragePrefix = "sentence_base_storage_";

TaskPort.register(
  "localStorageSet",
  async ({ key, value }: { key: string; value: any }) => {
    localStorage.setItem(`${localStoragePrefix}${key}`, JSON.stringify(value));
  },
);

TaskPort.register("localStorageGet", async (key: string) => {
  return JSON.parse(localStorage.getItem(`${localStoragePrefix}${key}`) ?? "");
});

TaskPort.register("localStorageRemove", async (key: string) => {
  localStorage.removeItem(`${localStoragePrefix}${key}`);
});
