import React from "react";
import qhistory from "qhistory";
import { parse, stringify } from "query-string";
import { History, createBrowserHistory } from "history";

//
// History

const stringifyWithBrackets = (params: object) =>
  stringify(params, { arrayFormat: "bracket" });

const parseWithBrackets = (url: string) =>
  parse(url, { arrayFormat: "bracket" });

export function createHistoryService() {
  const history = qhistory(
    createBrowserHistory(),
    stringifyWithBrackets,
    parseWithBrackets
  );
  return history;
}

//
// Services

export interface IServices {
  history: History;
}

export function createServices() {
  return {
    history: createHistoryService(),
  };
}

export const ServicesContext = React.createContext<IServices>(null as any);
export const Services = ServicesContext.Consumer;
