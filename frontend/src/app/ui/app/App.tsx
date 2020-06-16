import React from "react";
import { Provider } from "react-redux";
import { Router } from "react-router-dom";
import { IServices, ServicesContext } from "app/services/services";
import { AppRoutes } from "app/ui/app/AppRoutes";
import { styled, ThemeProvider } from "@material-ui/core";
import { theme, colors } from "app/ui/ux/theme";

interface IApp {
  services: IServices;
}

const StyledApp = styled("div")({
  "@global": {
    body: {
      margin: 0,
      fontFamily: "Roboto, sans-serif",
      backgroundColor: colors.white,
      color: colors["deep-blue-60"],
      fontSize: "16px",
    },
  },
});

export const App: React.FC<IApp> = ({ services }) => (
  <StyledApp className="App">
    <ThemeProvider theme={theme}>
      <ServicesContext.Provider value={services}>
        <Provider store={services.storeService}>
          <Router history={services.historyService}>
            <AppRoutes />
          </Router>
        </Provider>
      </ServicesContext.Provider>
    </ThemeProvider>
  </StyledApp>
);
