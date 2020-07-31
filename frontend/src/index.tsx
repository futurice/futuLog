import "app/styles.scss";

import React from "react";
import ReactDOM from "react-dom";
import { App } from "app/ui/app/App";
import { createServices } from "app/services/services";
// import * as serviceWorker from "./serviceWorker";

const services = createServices();
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
window.__services = services;

ReactDOM.render(
  <App services={services} />,
  document.getElementById("root")
);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA

// serviceWorker.unregister();
