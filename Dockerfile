FROM haskell:8.8.3

COPY package.yaml stack.yaml stack.yaml.lock Setup.hs ./
RUN mkdir -p src
RUN stack build --only-dependencies

COPY src/* src/
RUN stack build
RUN cp "$(stack path --dist-dir)/build/office-tracker/office-tracker" .

FROM debian:buster
COPY --from=0 office-tracker .
COPY rooms.yaml .
EXPOSE 3000
CMD ["./office-tracker"]
