import { defineConfig } from "astro/config";
import elmPlugin from "vite-plugin-elm";

// https://astro.build/config
export default defineConfig({
  base: "/elm-form",
  vite: {
    plugins: [
      elmPlugin()
    ]
  }
});
