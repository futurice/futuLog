import React from "react";
import { styled } from "@material-ui/core/styles";
import InputBase, {InputBaseProps as MuiInputBaseProps} from "@material-ui/core/InputBase"
import { LinkProps, Link as ReactRouterLink } from "react-router-dom";
import { colors } from "./theme";

export type InputFieldProps = MuiInputBaseProps;

export const InputField = styled((props: MuiInputBaseProps) => <InputBase {...props} />)({
  background: "#FFFFFF",
  border: `1px solid ${colors["deep-blue-80"]}`,
  boxSizing: "border-box",
  borderRadius: "4px",
  padding: '0px 5px',
  fontFamily: 'Roboto',
  fontStyle: 'normal',
  fontWeight: 'normal',
  fontSize: '16px',
  lineHeight: '150%',
  color:`${colors["deep-blue-80"]}`,
  "&:hover": {
    border: `1px solid ${colors["deep-blue-50"]}`,
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
  },
  "&:focus": {
    border: `2px solid ${colors["deep-blue-50"]}`,
  },
  "&:active": {
    border: "1px solid #200A74",
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
  },
  "&:disabled":{
    border: "1px solid #D2CEE3",
    color: '#14074B',
    opacity: '0.5',
  },
});
