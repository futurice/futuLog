import React from "react";
import { styled } from "@material-ui/core/styles";
import MuiButton, { ButtonProps as MuiButtonProps } from "@material-ui/core/Button";
import { LinkProps, Link } from "react-router-dom";
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

  "&:after": {
    content: "''",
    display: "flex",
    zIndex: 10,
    position: "absolute",
    width: "100%",
    height: "100%",
    borderRadius: '4px',
    backgroundColor: "transparent",
    color: "transparent",
    transition: "box-shadow 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms",
  },

  "&:hover, &:focus": {
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
    borderWidth: "1px !important",

    "&:after": {
      boxShadow: "inset 0 0 1px 1px" + colors["deep-blue-80"],
    }
  },
});

export const ButtonWithIcon = styled(Button)({
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

const A = styled(Link)({
  display: "inline-block",
  textDecoration: "none",
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
    <A to={to as any} replace={replace} innerRef={innerRef}>
      <C {...props} />
    </A>
  );
};
