/// <reference types="vite/client" />

declare module "*.elm" {
  export const Elm: ElmInstance;
}

type ElmInstance = {
  Main: ElmMain;
};

type ElmMain = {
  init<
    T extends Record<string, ElmPort<any>>,
    U extends Record<string, ElmFlag> | undefined = undefined,
  >(
    options: {
      node?: Node | null;
    } & (U extends undefined ? {} : { flags: U }),
  ): ElmApp<T>;
};

type ElmReceivePort<T> = {
  subscribe(handler: (value: T) => void): void;
  unsubscribe(handler: (value: T) => void): void;
};

type ElmSendPort<T> = {
  send(value: T): void;
};

type ElmPort<T> = ElmReceivePort<T> | ElmSendPort<T>;

type ElmApp<T> = {
  ports: T;
};

type ElmFlag = number | string | boolean | Record<string, ElmFlag> | ElmFlag[];

declare module "elm-taskport" {
  export type Configuration = {
    logCallErrors?: boolean;
    logInteropErrors?: boolean;
  };

  export const install: (configuration?: Configuration) => void;
  export const register: (name: string, fn: (args?: any) => any) => void;
}
