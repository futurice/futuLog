import React from "react";
import { Theme } from "app/ui/ux/theme";
import { useMediaQuery } from "@material-ui/core";

interface IMediaQuery {
  query: string | ((theme: Theme) => string);
  children: React.ReactElement | null | (() => React.ReactElement | null);
}

export const MediaQuery: React.FC<IMediaQuery> = ({ query, children }) => {
  const matches = useMediaQuery(query);
  return matches && ((typeof children === "function" ? children() : children) as any);
};
