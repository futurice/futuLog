import React from "react";
import { styled } from "@material-ui/core/styles";
import {IconButton as MuiIconButton, Link as MuiLink } from "@material-ui/core";
import MuiButton, { ButtonProps as MuiButtonProps } from "@material-ui/core/Button";
import { LinkProps, Link as ReactRouterLink } from "react-router-dom";
import { colors } from "./theme";

export type ButtonProps = MuiButtonProps;

export const Button = styled((props: MuiButtonProps) => <MuiButton {...props} disableRipple />)({
  padding: "0.75rem 2rem",
  textTransform: "none",
  fontFamily: "Futurice",
  fontSize: "1rem",
  fontWeight: "bold",
  lineHeight: "1.25",
  boxShadow: "none",
  borderRadius: "8px",
  outline: "none",

  "&:hover": {
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
    backgroundColor: colors["deep-blue-50"]
  },
  "&:focus": {
    boxShadow: `inset 0 0 0px 3px ${colors["deep-blue-50"]}`
  },
  "&:active": {
    boxShadow: `inset 0 0 0px 1px ${colors["deep-blue-80"]}`,
    backgroundColor: colors["deep-blue-50"]
  }
});

export const ButtonWithIcon = styled(Button)({
  borderRadius: "4px",

  "&:hover, &:focus": {
    borderWidth: "1px !important"
  },
  "&:hover": {
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)"
  },
  "&:focus": {
    boxShadow: `inset 0 0 0px 1px ${colors["deep-blue-50"]}`
  },
  "&:active": {
    boxShadow: "none",
    backgroundColor: "initial",

    "& > .MuiButton-label": {
      fontWeight: "bold"
    }
  },
  "& > .MuiButton-label": {
    justifyContent: "flex-start"
  },
  "& .MuiButton-startIcon": {
    marginLeft: 0
  },
  "&:disabled .MuiButton-startIcon": {
    opacity: 0.5
  }
});

export const ButtonDiscrete = styled(Button)({
  lineHeight: 1.5,
  borderRadius: "4px",
  padding: "0",

  "&:hover": {
    boxShadow: "none",
    backgroundColor: colors["deep-blue-20"]
  },
  "&:focus": {
    boxShadow: `0 0 0px 4px ${colors["deep-blue-50"]}`,
  },
  "&:active": {
    boxShadow: `0 0 0px 2px ${colors["deep-blue-80"]}`,
    backgroundColor: "transparent"
  }
});

export const ButtonDiscreteWithEndIcon = styled(ButtonDiscrete)({
  borderRadius: "4px",
  padding: "0 0.5rem",

  "& > .MuiButton-label": {
    justifyContent: "flex-start"
  },
  "& .MuiButton-endIcon": {
    marginRight: 0
  },
  "&:disabled .MuiButton-endIcon": {
    opacity: 0.5
  }
});

export const IconButton = styled(MuiIconButton)({
  "&:hover": {
    boxShadow: "none",
    backgroundColor: colors["deep-blue-20"]
  },
  "&:focus": {
    boxShadow: `0 0 0px 4px ${colors["deep-blue-50"]}`,
  },
  "& > .MuiTouchRipple-root": {
    display: "none"
  }
});

export const Link = styled(MuiLink)({
  borderRadius: "4px",
  textDecoration: "underline",
  outline: "none",

  "&:focus": {
    boxShadow: `0 0 0px 4px ${colors["deep-blue-50"]}`,
  },
  "&:hover": {
    boxShadow: "none",
    color: colors["deep-blue-50"]
  }
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

const A = styled(ReactRouterLink)({
  display: "inline-block",
  textDecoration: "none"
});

// Link + Button type helper
export const LinkButton: React.FC<LinkProps & ButtonProps> = ({
  component = Button,
  to,
  replace,
  innerRef,
  ...props
}) => {
  const C = component;
  return (
    <A to={to as any} replace={replace} innerRef={innerRef} tabIndex={-1}>
      <C {...props} />
    </A>
  );
};
