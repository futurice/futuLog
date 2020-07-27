import React, { useState } from "react";
import { styled } from "@material-ui/core/styles";
import { FormControl as FormControlMui } from "@material-ui/core/";
import { FormControlProps as MuiFormControlProps } from "@material-ui/core/FormControl";
import { makeStyles } from "@material-ui/core/styles";
import { colors, theme } from "./theme";

export const FormControl= styled((props: MuiFormControlProps) => <FormControlMui {...props} />)({
  margin: theme.spacing(1),
  minWidth: 120,
  color: `${colors["deep-blue-80"]}`,
  border: `1px solid ${colors["deep-blue-80"]}`,
  boxSizing: "border-box",
  borderRadius: "4px",
  "&:hover": {
    border: `1px solid ${colors["deep-blue-50"]}`,
    backgroundColor: `${colors["deep-blue-80"]}`,
  },
  "&:active": {
    backgroundColor: `${colors["deep-blue-80"]}`,
  },
  "&:disabled": {
    background: "rgba(255, 255, 255, 0.5)",
    border: "1px solid rgba(166, 157, 199, 0.5)",
    color: "rgba(255, 255, 255, 0.5)"
  },
  "&.MuiSelect-icon": {
    background: `${colors["deep-blue-80"]}`,
    color: `${colors.white}`,
  },
});
