import { generatePath } from "react-router";
import qhistory from "qhistory";
import { parse, stringify } from "query-string";
import { createBrowserHistory } from "history";

export const routes = {
  BOOKING: "/booking",
  TRACKING: "/tracking",
};

const stringifyWithBrackets = (params: object) =>
  stringify(params, { arrayFormat: "bracket" });

const parseWithBrackets = (url: string) =>
  parse(url, { arrayFormat: "bracket" });

export const history = qhistory(
  createBrowserHistory(),
  stringifyWithBrackets,
  parseWithBrackets
);

interface IUrlParams {
  [paramName: string]: string | number | boolean;
}

export const generateUrl = (
  urlPattern: string,
  urlParams: IUrlParams,
  queryParams?: object
) => {
  const filledUrlPattern = generatePath(urlPattern, urlParams);

  if (!queryParams) {
    return filledUrlPattern;
  }

  return `${filledUrlPattern}?${stringify(queryParams, {
    arrayFormat: "bracket",
  })}`;
};
