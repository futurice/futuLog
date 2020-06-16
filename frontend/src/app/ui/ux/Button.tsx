import React from "react";
import { styled } from "@material-ui/core/styles";
import MuiButton, { ButtonProps as MuiButtonProps } from "@material-ui/core/Button";

export type ButtonProps = MuiButtonProps;

export const Button = styled((props) => <MuiButton {...props} disableRipple />)({
  padding: "0.75rem 2rem",
  textTransform: "none",
  fontFamily: "Futurice",
  fontSize: "1rem",
  fontWeight: "bold",
  lineHeight: "1.25",
  "&:hover": {
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
  },
});

export const IconButton = styled(Button)({
  "& > .MuiButton-label": {
    justifyContent: "flex-start",
  },
  "& .MuiButton-startIcon": {
    marginLeft: 0,
  },
});

export const FauxLink = styled(Button)({
  display: "inline",
  minWidth: 0,
  padding: 0,
  fontWeight: "normal",
  fontSize: "1rem",
  fontFamily: "inherit",
  textDecoration: "underline",
  lineHeight: "inherit",
  color: "inherit",
  borderRadius: 0,
  letterSpacing: "inherit",
  verticalAlign: "inherit",
  "&:hover": {
    backgroundColor: "transparent",
    textDecoration: "underline",
    boxShadow: "none",
  },
});
