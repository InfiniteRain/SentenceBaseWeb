import * as TaskPort from "elm-taskport";

TaskPort.register(
  "timeout",
  (config: { id: number; timeout: number }) =>
    new Promise((resolve) => {
      setTimeout(() => {
        resolve(config.id);
      }, config.timeout);
    }),
);
