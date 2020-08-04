import React from "react";
import { styled } from "@material-ui/core/styles";
import {InputBase as MuiInputBase} from "@material-ui/core";
import {InputBaseProps as MuiInputBaseProps} from "@material-ui/core/InputBase";
import {TextField as MuiTextField} from '@material-ui/core';
import { TextFieldProps as MuiTextFieldProps } from '@material-ui/core/TextField';
import { colors } from "./theme";

export const InputBase = styled((props: MuiInputBaseProps) => <MuiInputBase {...props} />)({
  background: `${colors["white"]}`,
  border: `1px solid ${colors["deep-blue-80"]}`,
  boxSizing: "border-box",
  borderRadius: "4px",
  padding: "0px 5px",
  lineHeight: "1.5",
  color:`${colors["deep-blue-80"]}`,
  "&:hover": {
    border: `1px solid ${colors["deep-blue-50"]}`,
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
  },
  "&:focus": {
    border: `2px solid ${colors["deep-blue-50"]}`,
  },
  "&:active": {
    border: `1px solid ${colors["deep-blue-80"]}`,
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
  },
  "&:disabled":{
    border: `1px solid ${colors["deep-blue-20"]}`,
    color: `${colors["deep-blue-90"]}`,
    opacity: "0.5",
  },
});




export const TextField = styled((props: MuiTextFieldProps) => <MuiTextField {...props}/>)({
  background: `${colors["white"]}`,
  border: `1px solid ${colors["deep-blue-80"]}`,
  boxSizing: "border-box",
  borderRadius: "4px",
  lineHeight: "1.5",
  color:`${colors["deep-blue-80"]}`,
  transition: "background-color 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms," +
    "box-shadow 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms," +
    "border 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms",
  "&:hover": {
    border: `1px solid ${colors["deep-blue-50"]}`,
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
  },
  "&:focus": {
    border: `2px solid ${colors["deep-blue-50"]}`,
  },
  "&:active": {
    border: `1px solid ${colors["deep-blue-80"]}`,
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
  },
  "&:disabled":{
    border: `1px solid ${colors["deep-blue-20"]}`,
    color: `${colors["deep-blue-90"]}`,
    opacity: "0.5",
  },
  "& .MuiButtonBase-root":{
    marginTop: "-5px",
    marginRight: "-2px"
  },
  "& .MuiSelect-icon": {
    width: "36px",
    height: "34px",
    top: "unset",

    justifyContent: "center",
    alignItems: "center",

    border: `2px solid ${colors["deep-blue-80"]}`,
    backgroundColor: colors["deep-blue-80"],
    borderTopRightRadius: "5px",
    borderBottomRightRadius: "5px",
    transition: "background-color 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms," +
      "border 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms",

  },
  "&:hover .MuiSelect-icon": {
    backgroundColor: colors["deep-blue-50"],
    border: `2px solid ${colors["deep-blue-50"]}`,
  },
  "& .MuiInputBase-input":{
    color:`${colors["deep-blue-80"]}`,
  },
  "& .MuiAutocomplete-popupIndicatorOpen":{
    transform: "none!important"
  }



});
