import React from "react";
import { styled } from "@material-ui/core/styles";
import { FormControl as FormControlMui } from "@material-ui/core/";
import { FormControlProps as MuiFormControlProps } from "@material-ui/core/FormControl";
import { theme } from "./theme";

export const FormControl= styled((props: MuiFormControlProps) => <FormControlMui {...props} variant="outlined" />)({
  margin: theme.spacing(1),
  minWidth: 150,
  "& .MuiInput-underline:before":{
    display:"none",
  },
  "& .MuiInput-underline:after":{
    display:"none",
  },
});
