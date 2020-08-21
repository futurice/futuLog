import React from "react";
import { makeStyles } from "@material-ui/core/styles";
import { Autocomplete } from "@material-ui/lab";
import { AutocompleteProps as MuiAutocompleteProps } from "@material-ui/lab/Autocomplete";
import { IconArrowDown } from "./icons";
import { colors } from "./theme";

const useStyles = makeStyles(() => ({
  popupIndicator: {
    backgroundColor: `${colors["deep-blue-80"]}`,
    borderRadius: "0 3px 3px 0",
    height: "36px",
    width: "36px",

  },
  popupIndicatorOpen: {
    "& .MuiSvgIcon-root": {
      transform: "rotate(180deg)",
    },
  },
}));

type option = {
  label: string;
  value: string;
};

export const Searchbox = (props: MuiAutocompleteProps<option, boolean, boolean, boolean>) => {
  const classes = useStyles();
  return (
    <Autocomplete
      {...props}
      popupIcon={<IconArrowDown color={colors.white} />}
      classes={{
        popupIndicator: classes.popupIndicator,
        popupIndicatorOpen: classes.popupIndicatorOpen,
      }}
    />
  );
};
