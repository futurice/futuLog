import React from "react";
import { Provider } from "react-redux";
import { Router } from "react-router-dom";
import { IServices, ServicesContext } from "app/services/services";
import { AppRoutes } from "app/ui/app/AppRoutes";
import { ThemeProvider } from "@material-ui/core";
import { theme } from "app/ui/ux/theme";

interface IApp {
  services: IServices;
}

export const App: React.FC<IApp> = ({ services }) => (
  <div className="App">
    <ThemeProvider theme={theme}>
      <ServicesContext.Provider value={services}>
        <Provider store={services.storeService}>
          <Router history={services.historyService}>
            <AppRoutes />
          </Router>
        </Provider>
      </ServicesContext.Provider>
    </ThemeProvider>
  </div>
);
