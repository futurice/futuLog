import React from "react";
import { styled } from "@material-ui/core/styles";
import MuiButton, { ButtonProps as MuiButtonProps } from "@material-ui/core/Button";
import { LinkProps, Link } from "react-router-dom";

export type ButtonProps = MuiButtonProps;

export const Button = styled((props: MuiButtonProps) => <MuiButton {...props} disableRipple />)({
  padding: "0.75rem 2rem",
  textTransform: "none",
  fontFamily: "Futurice",
  fontSize: "1rem",
  fontWeight: "bold",
  lineHeight: "1.25",
  boxShadow: "none",
  "&:hover, &:focus": {
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
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
