#syntax=docker/dockerfile:1.2
FROM haskell:8.10.2 AS backend

RUN apt-get update && apt-get install -y libpq-dev liblzma-dev

COPY backend/package.yaml backend/stack.yaml backend/stack.yaml.lock backend/Setup.hs ./
RUN mkdir -p src
RUN --mount=type=cache,target=/root/.stack stack setup --install-ghc && stack build --only-dependencies

COPY backend/src/ src/
RUN --mount=type=cache,target=/root/.stack stack build && cp "$(stack path --dist-dir)/build/futuLog/futuLog" .

FROM node:14.4-buster AS frontend

COPY frontend/package.json frontend/package-lock.json ./
RUN npm ci

COPY frontend/tsconfig.json ./
COPY frontend/src ./src
COPY frontend/public ./public
RUN npm run build

FROM debian:buster AS service

RUN apt-get update && apt-get install -y libpq5 ca-certificates

COPY ./startup.sh ./

# Add backend
COPY --from=0 futuLog .
EXPOSE 8000

# Add frontend
COPY --from=1 build ./static

CMD ["./startup.sh"]
