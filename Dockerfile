FROM haskell:8.10.2 AS backend

RUN apt-get update && apt-get install -y libpq-dev liblzma-dev

COPY backend/package.yaml backend/stack.yaml backend/stack.yaml.lock backend/Setup.hs ./
RUN mkdir -p src
RUN stack setup --install-ghc && stack build --only-dependencies

COPY backend/src/ src/
RUN stack build
RUN cp "$(stack path --dist-dir)/build/office-tracker/office-tracker" .

FROM node:14.4-buster AS frontend

COPY frontend/package.json frontend/package-lock.json ./
RUN npm ci

COPY frontend/tsconfig.json ./
COPY frontend/src ./src
COPY frontend/public ./public
RUN npm run build

FROM debian:buster

RUN apt-get update && apt-get install -y libpq5 ca-certificates

# Add backend
COPY --from=0 office-tracker .
COPY offices.yaml shifts.yaml ./
EXPOSE 8000

# Add frontend
COPY --from=1 build ./static

CMD ["./office-tracker"]
