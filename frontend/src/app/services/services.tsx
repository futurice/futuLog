import React, { useContext } from "react";
import qhistory from "qhistory";
import { parse as qsParse, stringify as qsStringify } from "query-string";
import { createBrowserHistory } from "history";
import { createAPIClientService, IAPIClientService } from "app/services/apiClientService";
import { configureStore, getDefaultMiddleware, Store, Middleware } from "@reduxjs/toolkit";
import { rootStore } from "app/stores/rootStore";

//
// History

const stringifyWithBrackets = (params: Record<string, unknown>) =>
  qsStringify(params, { arrayFormat: "bracket" });

const parseWithBrackets = (url: string) => qsParse(url, { arrayFormat: "bracket" });

export function createHistoryService() {
  const history = qhistory(createBrowserHistory(), stringifyWithBrackets, parseWithBrackets);
  return history;
}

export type IHistoryService = ReturnType<typeof createHistoryService>;

//
// Store

export type IStoreService = Store;

const multi: Middleware = ({ dispatch }) => (next) => (action) => {
  if (Array.isArray(action)) {
    return action.map(dispatch);
  } else {
    return next(action);
  }
};

//
// Services

export interface IServices {
  historyService: IHistoryService;
  apiClientService: IAPIClientService;
  storeService: IStoreService;
}

export function createServices() {
  const services = {} as IServices;

  // NOTE: Make sure you populate all services here
  services.historyService = createHistoryService();
  services.apiClientService = createAPIClientService(
    process.env.NODE_ENV === "production" ? "" : "http://localhost:5000"
  );
  services.storeService = configureStore({
    reducer: rootStore,
    middleware: [
      multi,
      ...getDefaultMiddleware({
        thunk: { extraArgument: services },
      }),
    ],
  });

  return services;
}

export const ServicesContext = React.createContext<IServices>(null as never);
export const Services = ServicesContext.Consumer;

export const useServices = () => useContext(ServicesContext);
