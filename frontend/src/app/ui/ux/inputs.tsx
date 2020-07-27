import React from "react";
import { styled } from "@material-ui/core/styles";
import {InputBase as InputBaseMui} from "@material-ui/core";
import {InputBaseProps as MuiInputBaseProps} from "@material-ui/core/InputBase";
import { colors } from "./theme";

export const InputBase = styled((props: MuiInputBaseProps) => <InputBaseMui {...props} />)({
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
