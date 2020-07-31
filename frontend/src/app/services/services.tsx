import React, { useContext } from "react";
import qhistory from "qhistory";
import { parse as qsParse, stringify as qsStringify } from "query-string";
import { createBrowserHistory } from "history";
import { QueryCache, makeQueryCache } from "react-query";
import { createAPIClientService, IAPIClientService } from "app/services/apiClientService";

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
// Services

export interface IServices {
  history: IHistoryService;
  apiClient: IAPIClientService;
  queryCache: QueryCache;
  localStorage: Storage;
}

export function createServices(): IServices {
  const services = {
    history: createHistoryService(),
    apiClient: createAPIClientService(""),
    queryCache: makeQueryCache(),
    localStorage: localStorage,
  };

  return services;
}

export const ServicesContext = React.createContext<IServices>(null as never);
export const Services = ServicesContext.Consumer;

export const useServices = () => useContext(ServicesContext);
