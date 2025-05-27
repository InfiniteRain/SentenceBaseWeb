import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import mkcert from "vite-plugin-mkcert";

export default defineConfig({
  plugins: [mkcert(), elmPlugin()],
});
