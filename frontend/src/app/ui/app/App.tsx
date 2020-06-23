import React from "react";
import { Router } from "react-router-dom";
import { IServices, ServicesContext } from "app/services/services";
import { AppRoutes } from "app/ui/app/AppRoutes";
import { ThemeProvider } from "@material-ui/core";
import { theme } from "app/ui/ux/theme";
import { ReactQueryCacheProvider } from "react-query";

interface IApp {
  services: IServices;
}

export const App: React.FC<IApp> = ({ services }) => (
  <div className="App">
    <ThemeProvider theme={theme}>
      <ServicesContext.Provider value={services}>
        <ReactQueryCacheProvider queryCache={services.queryCache}>
          <Router history={services.history}>
            <AppRoutes />
          </Router>
        </ReactQueryCacheProvider>
      </ServicesContext.Provider>
    </ThemeProvider>
  </div>
);
