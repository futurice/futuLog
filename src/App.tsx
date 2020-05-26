import React from "react";
import { Route, Router } from "react-router-dom";
import { history } from "./routes/config";
import Routes from "./routes";
import "./App.scss";

function App() {
  return (
    <Router history={history}>
      <Route />
      <Routes />
    </Router>
  );
}

export default App;
