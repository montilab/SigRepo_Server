# Production image for the SigRepo React interface.
#
# Two stages: build the static bundle with Node, then serve it with nginx. The
# runtime image carries no Node or source, only the compiled assets.
#
# The app calls the API at a same-origin /api path (see src/api/client.ts), so
# nginx proxies /api to the sigrepo-api container rather than the browser
# talking to it directly. That keeps the API off the public internet and avoids
# CORS entirely.

# ---- build ----------------------------------------------------------------
FROM node:20-alpine AS build

WORKDIR /app

# Install dependencies first so this layer caches across source-only changes.
COPY package.json package-lock.json ./
RUN npm ci

COPY . .
RUN npm run build

# ---- serve ----------------------------------------------------------------
FROM nginx:alpine

COPY --from=build /app/dist /usr/share/nginx/html
COPY nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 80
