import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import mkcert from "vite-plugin-mkcert";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  plugins: [mkcert(), elmPlugin(), tailwindcss()],
  base: "https://infiniterain.github.io/sentence-base/",
});
