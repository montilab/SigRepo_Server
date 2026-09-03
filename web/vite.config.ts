import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vite.dev/config/
// TYPE CHECKING: use `npm run build`, which runs `tsc -b` across
// tsconfig.app.json and tsconfig.node.json.
//
// `npx tsc --noEmit` is NOT equivalent and will mislead you: it resolves the
// root tsconfig, which delegates to those two via project references and checks
// almost nothing itself, so it reports success on code that `npm run build`
// rejects outright.

export default defineConfig({
  plugins: [react()],
  server: {
    // Proxy "/api/*" to the local Plumber API so the browser makes
    // same-origin requests (no CORS). The API routes have no /api prefix,
    // so strip it. run_sigrepo_api.R listens on 3838.
    proxy: {
      '/api': {
        target: process.env.VITE_API_TARGET ?? 'http://localhost:3838',
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/api/, ''),
      },
    },
  },
})
