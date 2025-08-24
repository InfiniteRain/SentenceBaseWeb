import { Elm } from "../src/Main.elm";
import * as TaskPort from "elm-taskport";
import "./style.css";
import "./ports/google";
import "./ports/clipboard";
import "./ports/timeout";
import "./ports/local-storage";
import "./ports/azure-speech";
import "./ports/anki-export";

TaskPort.install({
  logCallErrors: true,
  logInteropErrors: true,
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
