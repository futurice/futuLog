import React from "react";
import { Router } from "react-router-dom";
import { IServices, ServicesContext } from "app/services/services";
import { AppRoutes } from "app/ui/app/AppRoutes";
import { ThemeProvider } from "@material-ui/core";
import { theme } from "app/ui/ux/theme";
import { ReactQueryCacheProvider, ReactQueryConfigProvider } from "react-query";
import { MuiPickersUtilsProvider } from "@material-ui/pickers";
import DayjsUtils from "@date-io/dayjs";

interface IApp {
  services: IServices;
}

export const App: React.FC<IApp> = ({ services }) => (
  <div className="App">
    <ThemeProvider theme={theme}>
      <ServicesContext.Provider value={services}>
        <ReactQueryConfigProvider config={{ refetchOnWindowFocus: false }}>
          <ReactQueryCacheProvider queryCache={services.queryCache}>
            <MuiPickersUtilsProvider utils={DayjsUtils}>
              <Router history={services.history}>
                <AppRoutes />
              </Router>
            </MuiPickersUtilsProvider>
          </ReactQueryCacheProvider>
        </ReactQueryConfigProvider>
      </ServicesContext.Provider>
    </ThemeProvider>
  </div>
);
