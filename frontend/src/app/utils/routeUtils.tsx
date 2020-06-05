import { generatePath } from "react-router";
import { stringify } from "query-string";

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
