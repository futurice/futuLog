# futuLog frontend

This directory contains the frontend for [futuLog](https://futulog.play.futurice.com/). This is a standard [Create React App](https://github.com/facebook/create-react-app)-based project. You will need to setup the backend for local development, find instructions in the root [README.md](../README.md).

The frontend is written with Typescript, using React, Material UI as a component library and the main styling mechanism and react-query to handle remote data fetching and caching.

## Local development

Once you have the backend compiled, run `npm start` to start the development mode. This script will do the following:

- Starts the backend at port `8000` by running `docker-compose up` in the parent directory (via `dev:backend-server` NPM script).
- Starts a CORS-passing proxy server at port `5000` against the backend (via `dev:backend-proxy` NPM script).
- Starts the frontend app with live reloading and all the CRA-goodies at port `3000` (via `dev:frontend` NPM script).

In development mode the frontend will contact the API through `http://localhost:5000`, i.e. the proxy server to allow CORS-requests to pass through to another port, you can find the switch for this in the code at [services.tsx](src/app/services/services.tsx).

To run tests (which there aren't any) and linter, run `CI=1 npm test`. The `CI` environment variable is recommended so far as there are no tests, otherwise the command would leave `jest` in watch mode.

You might want to install Prettier, ESLint and Typescript support for your editor.

## Production

Run `npm run build` to create a production bundle and assets to `build`-directory. This step is done automatically by the Docker build in the parent directory, which is what the production pipeline does.

## App structure

Entrypoints to CRA are in `src` folder, but the grunt of the application exists under `src/app`. From there you can find the following:

- `src/app/assets`

  Static assets that are referenced and bundled from the source, for e.g. Futurice fonts.

- `src/app/services`

  This app uses a _service_-model, where moving parts of the app are provided as interfaces in a single object, such as API client, local storage, etc. This object is injected into the React-tree through _Context_ named `ServicesContext`.

- `src/app/ui`

  App components are stored in relatively flat hierarchy with a system where each main component exists in a folder named with a camel-cased version of the component, the component itself in pascal-cased file. Sub components specific to the main component also exist in the main components folder. Page components are named such, postfixed with `Page`. Folder named `src/app/ui/ux` contains design-system specific code, components, helpers, etc.

- `src/app/utils`

  Miscellaneous utility code.

- `public`

  Static assets that are bundled with the app.

## Resources

- [Material UI documentation](https://material-ui.com/getting-started/installation/)
- [Create React App documentation](https://create-react-app.dev/docs/getting-started/)
- [react-query](https://github.com/tannerlinsley/react-query)
